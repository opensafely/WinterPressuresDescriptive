# Load libraries ---------------------------------------------------------------
print("Load libraries")

library(magrittr)
library(here)
library(dplyr)

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
    cohort <- "postcovid1"
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

# Restrict columns for table 1 ------------------------------------------------
print('Restrict columns')

n_before <- ncol(input)

input <- input %>%
    select(
        practice_id,
        starts_with("index_date"),
        starts_with("exp_") # Exposures
    )

n_after <- ncol(input)
n_removed <- n_before - n_after

message("Number of variables before restriction: ", n_before)
message("Number of variables after restriction: ", n_after)
message("Number of variables removed: ", n_removed)

# GP characteristics of interest for making strata and calculating cutoffs ----
print(
    'GP characteristics of interest for making strata and calculating cutoffs'
)

vars_interest <- c(
    "exp_prop_female",
    "exp_prop_imd_5_least",
    "exp_prop_eth_white",
    "exp_prop_obesity",
    "exp_prop_smoker_current",
    "exp_prop_age_85_plus"
)

# 95th percentile thresholds for each
cutoffs <- input %>%
    summarise(across(
        all_of(vars_interest),
        ~ quantile(.x, 0.95, na.rm = TRUE)
    )) %>%
    as.list()

# add flags
input <- input %>%
    mutate(
        strata_female_high = ifelse(
            exp_prop_female >= cutoffs$exp_prop_female,
            1,
            0
        ),
        strata_imd5_high = ifelse(
            exp_prop_imd_5_least >= cutoffs$exp_prop_imd_5_least,
            1,
            0
        ),
        strata_eth_white_high = ifelse(
            exp_prop_eth_white >= cutoffs$exp_prop_eth_white,
            1,
            0
        ),
        strata_obesity_high = ifelse(
            exp_prop_obesity >= cutoffs$exp_prop_obesity,
            1,
            0
        ),
        strata_smoker_high = ifelse(
            exp_prop_smoker_current >= cutoffs$exp_prop_smoker_current,
            1,
            0
        ),
        strata_age85plus_high = ifelse(
            exp_prop_age_85_plus >= cutoffs$exp_prop_age_85_plus,
            1,
            0
        )
    )
message("Strata flags added to input dataset")

# Create Table 1 -----------------------------------------------------------------
summarise_dist <- function(x) {
    q <- quantile(x, probs = seq(0.1, 0.9, 0.1), na.rm = TRUE)
    tibble(
        n_practices = sum(!is.na(x)), # total practices with non-missing values
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

# Add overall label
table1_long <- input %>%
    select(
        practice_id,
        exp_cat_region,
        starts_with("exp_num_"),
        starts_with("exp_denom_"),
        starts_with("exp_prop_")
    ) %>%
    pivot_longer(
        cols = -c(practice_id, exp_cat_region),
        names_to = c("type", "category"),
        names_pattern = "exp_(num|denom|prop)_(.*)",
        values_to = "value"
    ) %>%
    rename(strata_region = exp_cat_region) %>%
    mutate(strata_region = coalesce(strata_region, "Unknown"))

# Create summary without redaction ----
print("Create summary without redaction")
table1_summary <- table1_long %>%
    group_by(category, type) %>%
    summarise(summarise_dist(value), .groups = "drop") %>%
    mutate(strata_region = "Overall") %>%
    bind_rows(
        table1_long %>%
            group_by(strata_region, category, type) %>%
            summarise(summarise_dist(value), .groups = "drop")
    )

# keep long version of strata
practice_strata_long <- input %>%
    select(
        practice_id,
        starts_with("exp_num_"),
        starts_with("exp_denom_"),
        starts_with("exp_prop_"),
        starts_with("strata_")
    ) %>%
    pivot_longer(
        cols = starts_with("strata_"),
        names_to = "strata",
        values_to = "in_stratum"
    ) %>%
    filter(in_stratum == 1) %>%
    select(-in_stratum)

table1_long_strata <- practice_strata_long %>%
    pivot_longer(
        cols = c(
            starts_with("exp_num_"),
            starts_with("exp_denom_"),
            starts_with("exp_prop_")
        ),
        names_to = c("type", "category"),
        names_pattern = "exp_(num|denom|prop)_(.*)",
        values_to = "value"
    )


table1_summary_strata <- table1_long_strata %>%
    group_by(strata, category, type) %>%
    summarise(summarise_dist(value), .groups = "drop")

table1_summary <- table1_summary %>%
    rename(strata = strata_region)

table1_summary_strata <- table1_summary_strata %>%
    mutate(strata = strata)

# bind together
table1_summary_all <- bind_rows(table1_summary, table1_summary_strata)

# Save Table 1 -----------------------------------------------------------------
print("Save Table 1")

write.csv(
    table1_summary_all,
    paste0(
        table1_dir,
        "table1-cohort_",
        cohort,
        ".csv"
    ),
    row.names = FALSE
)

# Apply redaction/rounding ----
table1_summary_midpoint <- table1_summary_all %>%
    group_split(type) %>%
    purrr::map_dfr(
        ~ {
            if (unique(.x$type) %in% c("num", "denom")) {
                # counts: midpoint rounding to threshold
                .x %>%
                    mutate(across(
                        where(is.numeric),
                        ~ roundmid_any(.x, to = threshold)
                    ))
            } else if (unique(.x$type) == "prop") {
                # proportions: adaptive rounding
                .x %>%
                    mutate(across(
                        where(is.numeric),
                        ~ roundmid_prop_adaptive(.x)
                    ))
            } else {
                .x
            }
        }
    ) %>%
    rename_with(~ paste0(.x, "_midpoint6"), where(is.numeric)) # <-- add suffix

message("Redaction complete")

# Save Table 1 -----------------------------------------------------------------
print("Save rounded Table 1")

write.csv(
    table1_summary_midpoint,
    paste0(
        table1_dir,
        "table1-cohort_",
        cohort,
        "-midpoint6.csv"
    ),
    row.names = FALSE
)
