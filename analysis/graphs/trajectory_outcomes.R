# Load libraries --------------------------------------------------------------
print("Load libraries")
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

# Source common functions ------------------------------------------------------
print("Source common functions")

source("analysis/utility.R")

# Load data ----------------------------------------------------------------------
print("Load data")

file_list <- list.files(
    path = "output/dataset_clean",
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
    arrange(cohort, outcome, week_number)


# Save unrounded table -----------------------------------------------------------------
print("Save trajectory outcomes input")

write.csv(
    outcome_traj_summary,
    paste0(
        trajectory_dir,
        "input_trajectory_outcomes.csv"
    ),
    row.names = FALSE
)
