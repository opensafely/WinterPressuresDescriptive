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

# Define table2 output folder ---------------------------------------------------------
print("Creating output/table2 output folder")

table2_dir <- "output/table2/"
fs::dir_create(here::here(table2_dir))

# Specify redaction threshold --------------------------------------------------
print("Specify redaction threshold")

threshold <- 6
threshold_practice <- 50

# Source common functions ------------------------------------------------------
print("Source common functions")

source("analysis/utility.R")
lapply(
    c(
        list.files("analysis/table1", full.names = TRUE, pattern = "fn-"),
        list.files("analysis/table2", full.names = TRUE, pattern = "fn-")
    ),
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
        practice_rurality,
        matches(patient_vars_pattern),
        -matches("_mp6$"), # only keep raw data of the above, as we need it for generating strata
        matches(table2_outcome_vars),
        matches("^week")
    ) %>%
    distinct(practice_id, .keep_all = TRUE)

rounded_vars <- table2_outcome_vars[grepl("_mp6$", table2_outcome_vars)]
unrounded_vars <- table2_outcome_vars[!grepl("_mp6$", table2_outcome_vars)]

# Add Strata variables if needed --------------------------------------------------------------
print("Add Strata variables if needed")
input <- add_strata_vars(input, Strata = TRUE)

# Create Table 2 -----------------------------------------------------------------
table2_summary_all_rounded <- create_table2(
    input,
    rounded = TRUE,
    Strata = TRUE,
    rounded_vars = rounded_vars,
    unrounded_vars = unrounded_vars,
    threshold = threshold,
    threshold_practice = threshold_practice
)
message("Created Table 2 summary with rounded variables")

table2_summary_all_unrounded <- create_table2(
    input,
    rounded = FALSE,
    Strata = TRUE,
    rounded_vars = rounded_vars,
    unrounded_vars = unrounded_vars,
    threshold = threshold,
    threshold_practice = threshold_practice
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
        output_suffix,
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
        output_suffix,
        ".csv"
    ),
    row.names = FALSE
)
