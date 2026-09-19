# Load libraries --------------------------------------------------------------
print("Load libraries")
library(tidyverse)
library(haven)
library(dplyr)
library(tidyr)
library(stringr)
library(here)
library(fs)
# Define output folder ---------------------------------------------------------
print("Creating output/graphs input folder")

trajectory_dir <- "output/graphs/"
fs::dir_create(here::here(trajectory_dir))

# Specify redaction threshold --------------------------------------------------
print("Specify redaction threshold")

threshold <- 6

# Source common functions ------------------------------------------------------
print("Source common functions")

source("analysis/utility.R")

# Specify command arguments ----------------------------------------------------
print("Specify command arguments")

args <- commandArgs(trailingOnly = TRUE)
print(length(args))
sensitivity_type <- if (length(args) >= 1) {
    args[[1]]
} else {
    "main"
}

# Specify input/output file based on sensitivity_type ----------------------------------
input_dir <- if (sensitivity_type == "main") {
    file.path("output", "dataset_clean")
} else {
    file.path("output", "dataset_clean", sensitivity_type)
}
output_suffix <- if (sensitivity_type == "main") {
    ""
} else {
    paste0("_", sensitivity_type)
}

# Load data ----------------------------------------------------------------------
print("Load data")

file_list <- list.files(
    path = input_dir,
    pattern = "^icc_input-.*\\.dta$",
    full.names = TRUE
)

input <- file_list %>%
    lapply(function(f) {
        cohort <- stringr::str_remove_all(
            basename(f),
            "^icc_input-|\\.dta$"
        )

        read_dta(f) %>%
            mutate(
                cohort = cohort,
                week_number = as.integer(week_number)
            )
    }) %>%
    bind_rows()

message(paste("Combined dataset has", nrow(input), "rows"))

# Outcome columns (everything except practice_id and week_number)
outcome_cols <- setdiff(
    names(input),
    c("practice_id", "week_number", "cohort")
)


# Generate summary table
outcome_traj_summary <- input %>%
    pivot_longer(
        cols = all_of(outcome_cols),
        names_to = "outcome",
        values_to = "value"
    ) %>%
    group_by(cohort, week_number, outcome) %>%
    summarise(
        summarise_dist(value, is_outcome = TRUE),
        .groups = "drop"
    ) %>%
    mutate(n_practices_midpoint6 = roundmid_num(n_practices, to = threshold)) %>%
    select(-n_practices) %>%
    arrange(cohort, outcome, week_number)


# Save unrounded table -----------------------------------------------------------------
print("Save trajectory outcomes input")

write.csv(
    outcome_traj_summary,
    paste0(
        trajectory_dir,
        "input_trajectory_outcomes",
        output_suffix,
        ".csv"
    ),
    row.names = FALSE
)
