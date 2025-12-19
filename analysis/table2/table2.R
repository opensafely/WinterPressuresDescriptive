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

# Define table2 output folder ---------------------------------------------------------
print("Creating output/table2 output folder")

table2_dir <- "output/table2/"
fs::dir_create(here::here(table2_dir))

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

# Define input file path ---------------------------------------------------------
print('Define input file path')

file_path <- paste0("output/dataset_clean/input_", cohort, "_clean.csv")

# Load data ----------------------------------------------------------------------
print('Load data')

input <- read_csv(file_path)
message(paste0(
    "Dataset has been read successfully with N = ",
    nrow(input),
    " rows"
))

# Restrict columns for table 2 ------------------------------------------------
print('Restrict columns')

n_before <- ncol(input)

input <- input %>%
    select(
        practice_id,
        matches("^age"),
        matches("^sex"),
        matches("^ethnicity"),
        matches("^imd"),
        matches("^rurality"),
        matches("^carehome"),
        matches("^smoking"),
        matches("^obesity"),
        -matches("_mp6$"), # drop mp6 versions of the above, as we only need raw data for generating strata
        matches("^practice_region$"),
        matches("^cons_.*_mean"),
        matches("^apc_.*_(mean|cumu)_mp6$"),
        matches("^ec_.*_(mean|cumu)_mp6$"),
        matches("^week")
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
    "age_0_4",
    "age_80",
    "sex_female",
    "ethnicity_white",
    "imd_1_most",
    "rurality_urban_comb",
    "smoking_current",
    "obesity"
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
        strata_female_above_p75 = as.integer(
            sex_female >= cutoffs_p75$sex_female
        ),
        strata_imd1_above_p75 = as.integer(
            imd_1_most >= cutoffs_p75$imd_1_most
        ),
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

# Create Table 2 -----------------------------------------------------------------
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

table2_long <- input %>%
    select(
        practice_id,
        practice_region,
        matches("_mp6$"),
        starts_with("strata_")
    ) %>%
    pivot_longer(
        cols = matches("_mp6$"),
        names_to = "outcome_name",
        values_to = "value"
    ) %>%
    rename(strata_region = practice_region) %>%
    mutate(
        strata_region = coalesce(strata_region, "Unknown")
    )

# Summarise overall data ----
print("Create summary without redaction")
table2_summary <- table2_long %>%
    group_by(outcome_name) %>%
    summarise(summarise_dist(value), .groups = "drop") %>%
    mutate(strata_region = "Overall") %>%
    bind_rows(
        table2_long %>%
            group_by(strata_region, outcome_name) %>%
            summarise(summarise_dist(value), .groups = "drop")
    ) %>%
    mutate(
        across(
            matches("n_practices"),
            ~ roundmid_num(., to = threshold)
        )
    ) %>%
    mutate(
        # sample: main or sub_xxx
        group = str_extract(outcome_name, "(main|sub_[a-z]+)"),

        # source: apc / ec
        source = case_when(
            str_detect(outcome_name, "^apc_") ~ "apc",
            str_detect(outcome_name, "^ec_") ~ "ec",
            TRUE ~ NA_character_
        ),

        # APC plan status
        apc_plan_status = case_when(
            source != "apc" ~ NA_character_,
            str_detect(outcome_name, "^apc_plan_") ~ "planned",
            str_detect(outcome_name, "^apc_unpl_") ~ "unplanned",
            TRUE ~ "all"
        ),

        # ACSC flag
        acsc = str_detect(outcome_name, "_acsc_"),

        # ACSC condition
        acsc_condition = case_when(
            !acsc ~ NA_character_,
            TRUE ~ str_extract(
                outcome_name,
                "(?<=_acsc_)(copd|asth|htn|diab|ang|any)"
            )
        ),
        # statistic
        stat = str_extract(outcome_name, "(mean|cumu)(?=_mp6$)"),
        # redact
        midpoint6 = case_when(
            grepl("mp6$", outcome_name) ~ TRUE,
            TRUE ~ FALSE
        )
    ) %>%
    relocate(
        group,
        source,
        apc_plan_status,
        acsc,
        acsc_condition,
        stat,
        midpoint6,
        .after = outcome_name
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
table2_long_strata <- practice_strata_long %>%
    pivot_longer(
        cols = matches("_mp6$"),
        names_to = "outcome_name",
        values_to = "value"
    )

# Summarise strata data ----
print("Summarise strata data")
table2_summary_strata <- table2_long_strata %>%
    group_by(strata, outcome_name) %>%
    summarise(summarise_dist(value), .groups = "drop") %>%
    mutate(
        across(
            matches("n_practices"),
            ~ roundmid_num(., to = threshold)
        )
    ) %>%
    mutate(
        # sample: main or sub_xxx
        group = str_extract(outcome_name, "(main|sub_[a-z]+)"),

        # source: apc / ec
        source = case_when(
            str_detect(outcome_name, "^apc_") ~ "apc",
            str_detect(outcome_name, "^ec_") ~ "ec",
            TRUE ~ NA_character_
        ),

        # APC plan status
        apc_plan_status = case_when(
            source != "apc" ~ NA_character_,
            str_detect(outcome_name, "^apc_plan_") ~ "planned",
            str_detect(outcome_name, "^apc_unpl_") ~ "unplanned",
            TRUE ~ "all"
        ),

        # ACSC flag
        acsc = str_detect(outcome_name, "_acsc_"),

        # ACSC condition
        acsc_condition = case_when(
            !acsc ~ NA_character_,
            TRUE ~ str_extract(
                outcome_name,
                "(?<=_acsc_)(copd|asth|htn|diab|ang|any)"
            )
        ),
        # statistic
        stat = str_extract(outcome_name, "(mean|cumu)(?=_mp6$)"),
        # redact
        midpoint6 = case_when(
            grepl("mp6$", outcome_name) ~ TRUE,
            TRUE ~ FALSE
        )
    ) %>%
    relocate(
        group,
        source,
        apc_plan_status,
        acsc,
        acsc_condition,
        stat,
        midpoint6,
        .after = outcome_name
    )

# Clean up names ----
table2_summary <- table2_summary %>%
    rename(strata = strata_region)

table2_summary_strata <- table2_summary_strata %>%
    mutate(strata = strata)

# Bind together overall and strata summaries ----
print("Bind together overall and strata summaries")
table2_summary_all <- bind_rows(table2_summary, table2_summary_strata)

# Save rounded Table 2 -----------------------------------------------------------------
print("Save Table 2")

write.csv(
    table2_summary_all,
    paste0(
        table2_dir,
        "table2-cohort_",
        cohort,
        "-midpoint6.csv"
    ),
    row.names = FALSE
)
