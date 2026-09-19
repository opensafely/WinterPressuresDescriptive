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

# Source common functions ------------------------------------------------------
print("Source common functions")

source("analysis/utility.R")
lapply(
    list.files("analysis/dataset_clean_sensitivity", full.names = TRUE, pattern = "fn-"),
    source
)

# Specify command arguments ----------------------------------------------------
print("Specify command arguments")

args <- commandArgs(trailingOnly = TRUE)

cohort <- if (length(args) >= 1) {
    args[[1]]
} else {
    "precovid"
}

sensitivity_type <- if (length(args) >= 2) {
    args[[2]]
} else {
    "sensitivity_consultation"
}

# Define clean sensitivity dataset output folder -------------------------------------------
print("Creating output/dataset_clean_sensitivity output folder")

dataclean_dir <- "output/dataset_clean/"
dir_create(here::here(dataclean_dir))
dataclean_sensitivity_dir <- paste0("output/dataset_clean/", sensitivity_type, "/")
dir_create(here::here(dataclean_sensitivity_dir))

# Load data ----------------------------------------------------------------------
print("Load data")

input <- readr::read_rds(paste0(
    dataclean_dir,
    "input_",
    cohort,
    "_clean.rds"
))
message(paste0(
    "Dataset has been read successfully with N = ",
    nrow(input),
    " rows"
))

# Load main flow table --------------------------------------------------------
print("Load main flow table")

flow <- read.csv(
    paste0(dataclean_dir, "flow-cohort_", cohort, ".csv"),
    stringsAsFactors = FALSE
) %>%
    select(Description, N)

flow$N <- as.numeric(flow$N)

if (
    nrow(flow) == 0 ||
        anyNA(flow$N) ||
        tail(flow$N, 1) != n_distinct(input$practice_id)
) {
    stop("The main flow table does not match the practices in the clean dataset.")
}

# Define available sensitivity criteria ----------------------------------------
sensitivity_functions <- list(
    sensitivity_consultation = exclude_zero_consultation
)

if (!sensitivity_type %in% names(sensitivity_functions)) {
    stop(
        "Unknown sensitivity_type: ",
        sensitivity_type,
        ". Available options are: ",
        paste(names(sensitivity_functions), collapse = ", ")
    )
}

# Apply sensitivity exclusion criteria ----------------------------------------
print("Apply sensitivity exclusion criteria")

inex_result <- sensitivity_functions[[sensitivity_type]](
    input,
    flow
)

input <- inex_result$input
flow <- inex_result$flow

flow$N <- as.numeric(flow$N)

# Make ICC input dataset --------------------------------------------------------
print("Make ICC input dataset")

outcome_vars <- names(input)[
    grepl("^(apc|ec)_", names(input)) &
        !grepl("(mean|cumu|mp6)", names(input)) &
        !grepl("acsc_(asth|copd|htn|diab|ang)", names(input))
]

if (length(outcome_vars) == 0) {
    stop("No raw weekly outcome variables found for the ICC input.")
}

icc <- input %>% select(
    practice_id,
    week_number,
    all_of(outcome_vars)
)

# Calculate exclusions for the full flow table ---------------------------------
flow$removed <- dplyr::lag(
    flow$N,
    default = dplyr::first(flow$N)
) - flow$N

# Apply midpoint-6 rounding ----------------------------------------------------
print("Apply midpoint-6 rounding to flow data")

flow_midpoint6 <- flow %>% select(Description)
flow_midpoint6$N_midpoint6 <- roundmid_num(flow$N, to = 6)
flow_midpoint6$removed_derived <- dplyr::lag(
    flow_midpoint6$N_midpoint6,
    default = dplyr::first(flow_midpoint6$N_midpoint6)
) - flow_midpoint6$N_midpoint6

# Save sensitivity dataset ----------------------------------------------------
saveRDS(
    input,
    paste0(
        dataclean_sensitivity_dir,
        "input_", cohort, "_clean.rds"
    ),
    compress = TRUE
)

# Save sensitivity ICC input ---------------------------------------------------
haven::write_dta(
    icc,
    paste0(
        dataclean_sensitivity_dir,
        "icc_input-", cohort, ".dta"
    )
)

# Save unrounded flow table ----------------------------------------------------
write.csv(
    flow,
    paste0(
        dataclean_sensitivity_dir,
        "flow-cohort_", cohort, ".csv"
    ),
    row.names = FALSE
)

# Save rounded flow table ------------------------------------------------------
write.csv(
    flow_midpoint6,
    paste0(
        dataclean_sensitivity_dir,
        "flow-cohort_", cohort, "-midpoint6.csv"
    ),
    row.names = FALSE
)
