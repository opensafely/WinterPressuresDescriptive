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
  cohort <- "postcovid1"
} else {
  cohort <- args[[1]]
}

# Preprocess data --------------------------------------------------------------
print('Preprocess data')
input <- preprocess(cohort)
message(paste0("Preprocessed data has N = ", nrow(input), " rows"))

# Create practice-level summary dataset ----------------------------------------
print('Create practice-level summary dataset')
practice_summary <- collapse(input)
message(paste0(
  "Practice-level summary dataset has N = ",
  nrow(practice_summary),
  " rows"
))

# Process measure outputs -------------------------------------------------------
print('Process measure outputs')

measure_output_clean <- process_measure_output(cohort)
message(paste0(
  "Measure output clean dataset has N = ",
  nrow(measure_output_clean),
  " rows"
))

# Merge measure outputs with practice_summary -----------------------------------
print('Merge measure outputs with practice_summary')

practice_summary <- practice_summary %>%
  right_join(measure_output_clean, by = "practice_id", suffix = c(".x", ""))

# Remove duplicated columns from practice_summary (those with .x suffix)
n_removed <- sum(endsWith(names(practice_summary), ".x"))
practice_summary <- practice_summary %>%
  select(-ends_with(".x"))

message(paste0(
  "Removed ",
  n_removed,
  " duplicated columns from practice_summary"
))
message(paste0(
  "Practice summary dataset after merging measure outputs has N = ",
  nrow(practice_summary),
  " rows"
))

# Remove practices with <1000 patients ----------------------------------------
print("Remove practices with <1000 patients")

n_before <- nrow(practice_summary)

practice_summary <- practice_summary %>%
  filter(exp_denom_total >= 1000)

n_after <- nrow(practice_summary)

n_removed <- n_before - n_after

message(paste0("Number of practices with <1000 patients: ", n_removed))
message(paste0(
  "Practice summary dataset after removing small practices has N = ",
  n_after,
  " rows"
))

# Save practice_summary dataset ---------------------------------------------------
print('Save practice_summary dataset')
output_path <- paste0(dataclean_dir, "input_", cohort, "_clean.csv")
write_csv(practice_summary, output_path)
message(paste0("Practice-level summary dataset saved to ", output_path))
