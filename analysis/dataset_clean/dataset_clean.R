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
library(haven)

# Define clean dataset output folder -------------------------------------------
print("Creating output/dataset_clean output folder")

dataclean_dir <- "output/dataset_clean/"
dir_create(here::here(dataclean_dir))

# Source common functions ------------------------------------------------------
print("Source common functions")

source("analysis/utility.R")
lapply(
  list.files("analysis/dataset_clean", full.names = TRUE, pattern = "fn-"),
  source
)

# Specify command arguments ----------------------------------------------------
print("Specify command arguments")

args <- commandArgs(trailingOnly = TRUE)
print(length(args))
if (length(args) == 0) {
  cohort <- "precovid"
} else {
  cohort <- args[[1]]
}

# Preprocess patient-level data --------------------------------------------------------------
print("Preprocess patient-level data")
input <- preprocess_patient(cohort)
message(paste0("Preprocessed data has N = ", nrow(input), " rows"))

# Create practice-level summary dataset from patient-level data ----------------------------------------
print("Create practice-level summary dataset")
practice_summary <- aggregate_patient(input)
message(paste0(
  "Practice-level summary dataset has N = ",
  nrow(practice_summary),
  " rows"
))

# Preprocess measure tables -------------------------------------------------------
print("Preprocess measure tables")

measure_output_clean <- preprocess_measure(cohort)
message(paste0(
  "Measure output clean dataset has N = ",
  nrow(measure_output_clean),
  " rows"
))

# Merge measure outputs with practice_summary -----------------------------------
print("Merge measure outputs with practice_summary")

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
print("Collapse categorical variables where needed")

input <- collapse_categories(input)
message("Categorical variables collapsed where needed")

# Apply redaction ----------------------------------------
print("Apply redaction")
input <- redact(input)
message("Redaction applied")

# Initialise flow table ------------------------------------------------------
flow <- data.frame(
  Description = "Input before practice exclusions",
  N = n_distinct(input$practice_id),
  Excluded = NA_integer_,
  stringsAsFactors = FALSE
)

# Apply inclusion/exclusion criteria ----------------------------------------
print("Apply inclusion/exclusion criteria")

inex_results <- inex_practice(
  input,
  flow,
  exclude_small_practices = TRUE,
  exclude_unknown_region = TRUE
)

input <- inex_results$input

# Save flow data after inclusion/exclusion criteria ----------------------------
print("Saving flow data after inclusion/exclusion criteria")

flow <- inex_results$flow
flow$N <- as.numeric(flow$N)

# Replace Excluded with removed, following the previous project's format
flow$Excluded <- NULL
flow$removed <- lag(flow$N, default = first(flow$N)) - flow$N

write.csv(
  flow,
  file = paste0(dataclean_dir, "flow-cohort_", cohort, ".csv"),
  row.names = FALSE
)

# Apply midpoint-6 rounding ----------------------------------------------------
print("Applying midpoint-6 rounding to flow data")

flow_midpoint6 <- flow
flow_midpoint6$removed <- NULL

flow_midpoint6$N_midpoint6 <- roundmid_num(
  flow_midpoint6$N,
  to = 6
)

flow_midpoint6$removed_derived <- lag(
  flow_midpoint6$N_midpoint6,
  default = first(flow_midpoint6$N_midpoint6)
) - flow_midpoint6$N_midpoint6

flow_midpoint6$N <- NULL

# Save rounded flow data -------------------------------------------------------
print("Saving midpoint-6 rounded flow data")

write.csv(
  flow_midpoint6,
  file = paste0(dataclean_dir, "flow-cohort_", cohort, "-midpoint6.csv"),
  row.names = FALSE
)

# Restrict to relevant variables only ----------------------------------------
print("Restrict to relevant variables only")

restricted_input <- restrict_column(input)
message("restricted to relevant variables only")

# Save clean dataset ---------------------------------------------------
print("Save clean dataset")

saveRDS(
  restricted_input$input,
  paste0(dataclean_dir, "input_", cohort, "_clean.rds"),
  compress = TRUE
)

print("Save dataset for ICC")
haven::write_dta(
  restricted_input$icc,
  paste0(dataclean_dir, "icc_input-", cohort, ".dta")
)
