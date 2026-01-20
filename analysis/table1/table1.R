# Load libraries --------------------------------------------------------------
print('Load libraries')

library(dplyr)
library(tidyverse)
library(lubridate)
library(data.table)
library(readr)
library(jsonlite)
library(here)
library(fs)
library(base)
library(stats)

# Define table1 output folder ---------------------------------------------------------
print("Creating output/table1 output folder")

table1_dir <- "output/table1/"
fs::dir_create(here::here(table1_dir))

# Specify redaction threshold --------------------------------------------------
print('Specify redaction threshold')

threshold <- 6

# Source common functions ------------------------------------------------------
print('Source common functions')

source("analysis/utility.R")

# Specify command arguments ----------------------------------------------------
print('Specify command arguments')

args <- commandArgs(trailingOnly = TRUE)
print(length(args))
if (length(args) == 0) {
  cohort <- "precovid"
} else {
  cohort <- args[[1]]
}

# Load data ----------------------------------------------------------------------
print('Load data')

input <- readr::read_rds(paste0(
  "output/dataset_clean/input_",
  cohort,
  "_clean.rds"
))
message(paste0(
  "Dataset has been read successfully with N = ",
  nrow(input),
  " rows"
))

# Restrict columns for table 1 ------------------------------------------------
print('Restrict columns')

n_before <- ncol(input)

input <- input %>%
  select(
    practice_id,
    matches("^list_size"),
    starts_with("practice_region"),
    matches("^age"),
    matches("^sex"),
    matches("^ethnicity"),
    matches("^imd"),
    matches("^rurality"),
    matches("^carehome"),
    matches("^smoking"),
    matches("^obesity"),
    matches("^cons_")
  ) %>%
  distinct(practice_id, .keep_all = TRUE)

n_after <- ncol(input)
n_removed <- n_before - n_after

message("Number of variables before restriction: ", n_before)
message("Number of variables after restriction: ", n_after)
message("Number of variables removed: ", n_removed)

# GP characteristics of interest for making strata and calculating cutoffs ----
print(
  "GP characteristics of interest for making strata and calculating cutoffs"
)

vars_interest <- c(
  "sex_female",
  "imd_1_most",
  "ethnicity_white",
  "obesity",
  "smoking_current",
  "age_0_4",
  "age_80",
  "rurality_urban_comb"
)

# 75th percentile thresholds for each
cutoffs_p75 <- input %>%
  summarise(
    across(
      all_of(vars_interest),
      ~ quantile(.x, 0.75, na.rm = TRUE)
    )
  ) %>%
  as.list()

# 25th percentile thresholds for each
cutoffs_p25 <- input %>%
  summarise(
    across(
      all_of(vars_interest),
      ~ quantile(.x, 0.25, na.rm = TRUE)
    )
  ) %>%
  as.list()

# add strata flags
input <- input %>%
  mutate(
    strata_female_above_p75 = as.integer(sex_female >= cutoffs_p75$sex_female),
    strata_imd1_above_p75 = as.integer(imd_1_most >= cutoffs_p75$imd_1_most),
    strata_eth_non_white_above_p75 = as.integer(
      ethnicity_white <= cutoffs_p25$ethnicity_white
    ),
    strata_obesity_above_p75 = as.integer(obesity >= cutoffs_p75$obesity),
    strata_smoker_above_p75 = as.integer(
      smoking_current >= cutoffs_p75$smoking_current
    ),
    strata_under5y_above_p75 = as.integer(age_0_4 >= cutoffs_p75$age_0_4),
    strata_age80plus_above_p75 = as.integer(age_80 >= cutoffs_p75$age_80),
    strata_urban_comb_above_p75 = as.integer(
      rurality_urban_comb >= cutoffs_p75$rurality_urban_comb
    )
  )

message("Strata flags added to input dataset")

# Create Table 1 -----------------------------------------------------------------
# Generate function to summarise distribution ----
print("Generate function to summarise distribution")
summarise_dist <- function(x) {
  q <- quantile(x, probs = seq(0.1, 0.9, 0.1), na.rm = TRUE)
  tibble(
    n_practices = sum(!is.na(x)), # total practices with non-missing values
    min = min(x, na.rm = TRUE),
    max = max(x, na.rm = TRUE),
    range = max - min,
    mean = mean(x, na.rm = TRUE),
    sd = sd(x, na.rm = TRUE),
    median = median(x, na.rm = TRUE),
    q1 = quantile(x, 0.25, na.rm = TRUE),
    q3 = quantile(x, 0.75, na.rm = TRUE),
    iqr = IQR(x, na.rm = TRUE),
    p10 = q[[1]],
    p20 = q[[2]],
    p30 = q[[3]],
    p40 = q[[4]],
    p50 = q[[5]],
    p60 = q[[6]],
    p70 = q[[7]],
    p80 = q[[8]],
    p90 = q[[9]]
  )
}
# Create long version of overall data ----
print("Create long version of data")

table1_long <- input %>%
  select(
    practice_id,
    practice_region,
    matches("_mp6$"),
    starts_with("strata_")
  ) %>%
  pivot_longer(
    cols = matches("_mp6$"),
    names_to = c("characteristic", "subcharacteristic"),
    names_pattern = "^([^_]+)(?:_(.*))?_mp6$",
    values_to = "value"
  ) %>%
  rename(strata_region = practice_region) %>%
  mutate(
    strata_region = coalesce(strata_region, "Unknown"),
    subcharacteristic = ifelse(
      is.na(subcharacteristic) | subcharacteristic == "",
      "True",
      subcharacteristic
    )
  )

# Summarise overall data ----
print("Create summary without redaction")
table1_summary <- table1_long %>%
  group_by(characteristic, subcharacteristic) %>%
  summarise(summarise_dist(value), .groups = "drop") %>%
  mutate(strata_region = "Overall") %>%
  bind_rows(
    table1_long %>%
      group_by(strata_region, characteristic, subcharacteristic) %>%
      summarise(summarise_dist(value), .groups = "drop")
  ) %>%
  mutate(
    across(
      matches("n_practices"),
      ~ roundmid_num(., to = threshold)
    )
  ) %>%
  rename_with(
    ~ paste0(.x, "_midpoint6"),
    where(is.numeric)
  )

# Create long version of strata data ----
print("Create long version of strata data")

# keep long version of strata
practice_strata_long <- input %>%
  select(
    practice_id,
    matches("_mp6$"),
    starts_with("strata_")
  ) %>%
  pivot_longer(
    cols = starts_with("strata_"),
    names_to = "strata",
    values_to = "in_stratum"
  ) %>%
  filter(in_stratum == 1) %>%
  select(-in_stratum)

# create long version of data for strata
table1_long_strata <- practice_strata_long %>%
  pivot_longer(
    cols = matches("_mp6$"),
    names_to = c("characteristic", "subcharacteristic"),
    names_pattern = "^([^_]+)(?:_(.*))?_mp6$",
    values_to = "value"
  ) %>%
  mutate(
    subcharacteristic = ifelse(
      is.na(subcharacteristic) | subcharacteristic == "",
      "True",
      subcharacteristic
    )
  )

# Summarise strata data ----
print("Summarise strata data")
table1_summary_strata <- table1_long_strata %>%
  group_by(strata, characteristic, subcharacteristic) %>%
  summarise(summarise_dist(value), .groups = "drop") %>%
  mutate(
    across(
      matches("n_practices"),
      ~ roundmid_num(., to = threshold)
    )
  ) %>%
  rename_with(
    ~ paste0(.x, "_midpoint6"),
    where(is.numeric)
  )

# Clean up names ----
table1_summary <- table1_summary %>%
  rename(strata = strata_region)

# Bind together overall and strata summaries ----
print("Bind together overall and strata summaries")
table1_summary_all <- bind_rows(table1_summary, table1_summary_strata)

# Save rounded Table 1 -----------------------------------------------------------------
print("Save Table 1")

write.csv(
  table1_summary_all,
  paste0(
    table1_dir,
    "table1-cohort_",
    cohort,
    "-midpoint6.csv"
  ),
  row.names = FALSE
)
