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
lapply(
  c(
    list.files("analysis/table1", full.names = TRUE, pattern = "fn-"),
    list.files("analysis/table2", full.names = TRUE, pattern = "fn-")
  ),
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

# Load data ----------------------------------------------------------------------
print('Load data')

input <- readr::read_rds(paste0(
    "output/dataset_clean/input_",
    cohort,
    "_clean.rds"
))
message(paste0(
    "Dataset has been read successfully with N = ",
    nrow(input),
    " rows"
))

# Restrict columns for table 2 ------------------------------------------------
print("Restrict columns for table 2")

patient_vars <- c(
  "age",
  "sex",
  "ethnicity",
  "imd",
  "rurality",
  "carehome",
  "smoking",
  "obesity"
)

patient_vars_pattern <- paste0(
  "^(",
  paste(patient_vars, collapse = "|"),
  ")"
)

table2_outcome_vars <- names(input)[
    grepl("^(apc|ec)_.*_(mean|cumu)", names(input))
]

input <- input %>%
    select(
        practice_id,
        practice_region,
        matches(patient_vars_pattern),
        -matches("_mp6$"), # only keep raw data of the above, as we need it for generating strata
        matches(table2_outcome_vars),
        matches("^week")
    ) %>%
    distinct(practice_id, .keep_all = TRUE)

rounded_vars <- table2_outcome_vars[grepl("_mp6$", table2_outcome_vars)]
unrounded_vars <- table2_outcome_vars[!grepl("_mp6$", table2_outcome_vars)]

# Add Strata variables if needed --------------------------------------------------------------
print('Add Strata variables if needed')
input <- add_strata_vars(input, Strata = TRUE)

# Create Table 2 -----------------------------------------------------------------
table2_summary_all_rounded <- create_table2(
  input,
  rounded = TRUE,
  Strata = TRUE,
  rounded_vars = rounded_vars,
  unrounded_vars = unrounded_vars,
  threshold = threshold
)
message("Created Table 2 summary with rounded variables")

table2_summary_all_unrounded <- create_table2(
  input,
  rounded = FALSE,
  Strata = TRUE,
  rounded_vars = rounded_vars,
  unrounded_vars = unrounded_vars,
  threshold = threshold
)
message("Created Table 2 summary with unrounded variables")


# Save rounded Table 2 -----------------------------------------------------------------
print("Save Table 2")

write.csv(
    table2_summary_all_rounded,
    paste0(
        table2_dir,
        "table2-cohort_",
        cohort,
        "-midpoint6.csv"
    ),
    row.names = FALSE
)

# Save unrounded Table 2 -----------------------------------------------------------------
print("Save Table 2")

write.csv(
    table2_summary_all_unrounded,
    paste0(
        table2_dir,
        "table2-cohort_",
        cohort,
        ".csv"
    ),
    row.names = FALSE
)