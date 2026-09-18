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
input_type <- if (length(args) >= 2) args[[2]] else "main"

# Load data ----------------------------------------------------------------------
print("Load data")

input_file <- switch(input_type,
  main = file.path(
    "output/dataset_clean",
    paste0("input_", cohort, "_clean.rds")
  ),
  sensitivity_consultation = file.path(
    "output/dataset_clean",
    paste0("input_", cohort, "_clean_sensitivity.rds")
  ),
  stop("Unknown input_type: ", input_type)
)

if (!file.exists(input_file)) {
  stop("Input file does not exist: ", input_file)
}

input <- readr::read_rds(input_file)

output_suffix <- if (input_type == "main") {
  ""
} else {
  paste0("_", input_type)
}

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
  "cons"
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
