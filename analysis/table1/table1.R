# Load libraries --------------------------------------------------------------
print("Load libraries")

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
print("Specify redaction threshold")

threshold <- 6
threshold_practice <- 50

# Source common functions ------------------------------------------------------
print("Source common functions")

source("analysis/utility.R")
lapply(
  list.files("analysis/table1", full.names = TRUE, pattern = "fn-"),
  source
)

# Specify command arguments ----------------------------------------------------
print("Specify command arguments")

args <- commandArgs(trailingOnly = TRUE)
print(length(args))
cohort <- if (length(args) >= 1) args[[1]] else "precovid"
sensitivity_type <- if (length(args) >= 2) args[[2]] else "main"

# Specify input/output file based on sensitivity_type ----------------------------------
input_dir <- if (sensitivity_type == "main") {
  "output/dataset_clean/"
} else {
  paste0("output/dataset_clean/", sensitivity_type, "/")
}

output_suffix <- if (sensitivity_type == "main") {
  ""
} else {
  paste0("_", sensitivity_type)
}

# Load data ----------------------------------------------------------------------
print("Load data")

input <- readr::read_rds(paste0(
  input_dir,
  "input_",
  cohort,
  "_clean.rds"
))
message(paste0(
  "Dataset has been read successfully with N = ",
  nrow(input),
  " rows"
))

# Restrict columns to those needed for Table 1 --------------------------------------------------------------
print("Restrict columns to those needed for Table 1")

table1_patient_vars <- c(
  "age",
  "sex",
  "ethnicity",
  "imd",
  "rurality",
  "carehome",
  "smoking",
  "obesity",
  "cons",
  "cms"
)
table1_patient_vars_pattern <- paste0(
  "^(",
  paste(table1_patient_vars, collapse = "|"),
  ")"
)

input <- input %>%
  select(
    practice_id,
    list_size_mp6,
    matches("^list_size_sub_(asth|copd|htn|diab|sevmh)_mp6$"),
    practice_region,
    practice_rurality,
    matches(table1_patient_vars_pattern)
  ) %>%
  distinct(practice_id, .keep_all = TRUE)

rounded_vars <- names(input)[
  grepl(paste0(table1_patient_vars_pattern, ".*_mp6$"), names(input))
]

unrounded_vars <- names(input)[
  grepl(table1_patient_vars_pattern, names(input)) &
    !grepl("_mp6$", names(input))
]

# Separate subgroup and whole-population variables -----------------------------
subgroup_pattern <- "_sub_(asth|copd|htn|diab|sevmh)(_mp6)?$"

rounded_vars_subgroups <- rounded_vars[
  grepl(subgroup_pattern, rounded_vars)
]

unrounded_vars_subgroups <- unrounded_vars[
  grepl(subgroup_pattern, unrounded_vars)
]

# Keep only whole-population variables for the original create_table1().
rounded_vars <- rounded_vars[
  !grepl(subgroup_pattern, rounded_vars)
]

unrounded_vars <- unrounded_vars[
  !grepl(subgroup_pattern, unrounded_vars)
]

# Add Strata variables if needed --------------------------------------------------------------
print("Add Strata variables if needed")
input <- add_strata_vars(input, Strata = TRUE)

# Create Table 1 -----------------------------------------------------------------
table1_summary_all_rounded <- create_table1(
  input,
  rounded = TRUE,
  Strata = TRUE,
  rounded_vars = rounded_vars,
  unrounded_vars = unrounded_vars,
  threshold = threshold,
  threshold_practice = threshold_practice
)
message("Created Table 1 summary with rounded variables")

table1_summary_all_unrounded <- create_table1(
  input,
  rounded = FALSE,
  Strata = TRUE,
  rounded_vars = rounded_vars,
  unrounded_vars = unrounded_vars,
  threshold = threshold,
  threshold_practice = threshold_practice
)
message("Created Table 1 summary with unrounded variables")

# Create patient-group summaries without additional strata ---------------------
table1_summary_subgroups_rounded <- create_table1_subgroups(
  input,
  rounded = TRUE,
  rounded_vars = rounded_vars_subgroups,
  unrounded_vars = unrounded_vars_subgroups,
  threshold = threshold,
  threshold_practice = threshold_practice
)

table1_summary_subgroups_unrounded <- create_table1_subgroups(
  input,
  rounded = FALSE,
  rounded_vars = rounded_vars_subgroups,
  unrounded_vars = unrounded_vars_subgroups,
  threshold = threshold,
  threshold_practice = threshold_practice
)

# Append patient groups to the existing Table 1 outputs -------------------------
table1_summary_all_rounded <- bind_rows(
  table1_summary_all_rounded,
  table1_summary_subgroups_rounded
)

table1_summary_all_unrounded <- bind_rows(
  table1_summary_all_unrounded,
  table1_summary_subgroups_unrounded
)

message("Appended patient-group summaries to Table 1")

# Save rounded Table 1 -----------------------------------------------------------------
print("Save rounded Table 1")

write.csv(
  table1_summary_all_rounded,
  paste0(
    table1_dir,
    "table1-cohort_",
    cohort,
    output_suffix,
    "-midpoint6.csv"
  ),
  row.names = FALSE
)

# Save unrounded Table 1 -----------------------------------------------------------------
print("Save Table 1")

write.csv(
  table1_summary_all_unrounded,
  paste0(
    table1_dir,
    "table1-cohort_",
    cohort,
    output_suffix,
    ".csv"
  ),
  row.names = FALSE
)
