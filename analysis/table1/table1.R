# Load libraries ---------------------------------------------------------------
print("Load libraries")

library(magrittr)
library(here)
library(dplyr)

# Define table1 output folder ---------------------------------------------------------
print("Creating output/table1 output folder")

table1_dir <- "output/table1/"
fs::dir_create(here::here(table1_dir))
# Define input file path ---------------------------------------------------------
print('Define input file path')

file_path <- paste0("output/dataset_clean/input_", cohort, "_clean.csv")

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


summarise_dist <- function(x) {
    q <- quantile(x, probs = seq(0.1, 0.9, 0.1), na.rm = TRUE)
    tibble(
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

table1_long <- practice_summary %>%
    select(
        practice_id,
        starts_with("exp_num_"),
        starts_with("exp_denom_"),
        starts_with("exp_prop_")
    ) %>%
    pivot_longer(
        cols = -practice_id,
        names_to = c("type", "category"),
        names_pattern = "exp_(num|denom|prop)_(.*)",
        values_to = "value"
    )
# ---- 1. Create summary without redaction ----
table1_summary <- table1_long %>%
    group_by(category, type) %>%
    summarise(summarise_dist(value), .groups = "drop")

# Save Table 1 -----------------------------------------------------------------
print("Save Table 1")

write.csv(
    table1_summary,
    paste0(
        table1_dir,
        "table1-cohort_",
        cohort,
        ".csv"
    ),
    row.names = FALSE
)

# ---- 2. Apply redaction/rounding ----
table1_summary_midpoint <- table1_summary %>%
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
    )
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
