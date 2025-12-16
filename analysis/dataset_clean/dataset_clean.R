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

# Define clean dataset output folder -------------------------------------------
print("Creating output/dataset_clean output folder")

dataclean_dir <- "output/dataset_clean/"
dir_create(here::here(dataclean_dir))

# Source common functions ------------------------------------------------------
print('Source common functions')

source("analysis/utility.R")
lapply(
  list.files("analysis/dataset_clean", full.names = TRUE, pattern = "fn-"),
  source
)

# Specify command arguments ----------------------------------------------------
print('Specify command arguments')

args <- commandArgs(trailingOnly = TRUE)
print(length(args))
if (length(args) == 0) {
  cohort <- "precovid"
} else {
  cohort <- args[[1]]
}

# Flag to control whether inclusion/exclusion criteria are applied
apply_inex <- FALSE # Set to TRUE for real run, FALSE for testing

# Preprocess patient-level data --------------------------------------------------------------
print('Preprocess patient-level data')
input <- preprocess_patients(cohort)
message(paste0("Preprocessed data has N = ", nrow(input), " rows"))

# Create practice-level summary dataset from patient-level data ----------------------------------------
print('Create practice-level summary dataset')
practice_summary <- aggregat(input)
message(paste0(
  "Practice-level summary dataset has N = ",
  nrow(practice_summary),
  " rows"
))

# Preprocess measure tables -------------------------------------------------------
print('Preprocess measure tables')

measure_output_clean <- preprocess_measure(cohort)
message(paste0(
  "Measure output clean dataset has N = ",
  nrow(measure_output_clean),
  " rows"
))

# Merge measure outputs with practice_summary -----------------------------------
print('Merge measure outputs with practice_summary')

input <- practice_summary %>%
  right_join(measure_output_clean, by = "practice_id", suffix = c(".x", ""))

# Remove duplicated columns (those with .x suffix)
n_removed <- sum(endsWith(names(input), ".x"))
input <- input %>%
  select(-ends_with(".x"))

message(paste0(
  "Removed ",
  n_removed,
  " duplicated columns from practice_summary"
))
message(paste0(
  "Practice summary dataset after merging measure outputs has N = ",
  nrow(input),
  " rows"
))

# Collapse categorical variables where needed ----------------------------------------
print('Collapse categorical variables where needed')

input <- collapse_categories(input)
message("Categorical variables collapsed where needed")

# Apply redaction ----------------------------------------
print('Apply redaction')
input <- redact(input)
message("Redaction applied")

# Restrict to relevant variables only ----------------------------------------
print('Restrict to relevant variables only')

input <- restrict_variables(input)
message("Restricted to relevant variables only")

# Apply inclusion/exclusion criteria ----------------------------------------
print('Apply inclusion/exclusion criteria')

if (apply_inex) {
  input <- inex(input)
  message(paste0(
    "Practice summary dataset AFTER applying inclusion/exclusion criteria has N = ",
    nrow(input),
    " rows"
  ))
} else {
  message(
    "Skipping inclusion/exclusion (apply_inex = FALSE) using full dataset for testing."
  )
}

# Remove prop_ from variable names ----------------------------------------
print('Remove prop_ from variable names')
input <- input %>%
  rename_with(~ gsub("^prop_", "", .x))
message("Removed prop_ from variable names")

# Save clean dataset ---------------------------------------------------
print('Save clean dataset')

saveRDS(input, paste0(dataclean_dir, "input_", cohort, "_clean.rds"), compress = TRUE)

