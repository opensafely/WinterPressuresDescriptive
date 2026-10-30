## IMPORTING R PACKAGES
library(tidyverse)
library(haven) # Allows you to import STATA .dta files
library(glue)
library(lubridate)
library(here)
library(data.table) # Allows you to import .csv files, and write .csv files
library(fs)
# library(arrow)

# Source common functions ------------------------------------------------------
print("Source common functions")

source("analysis/utility.R")
lapply(
  list.files("analysis/dataset_clean", full.names = TRUE, pattern = "fn-"),
  source
)

# Define clean dataset output folder -------------------------------------------
print("Creating output/dataset_clean output folder")

dataclean_dir <- "output/dataset_clean/"
dir_create(here::here(dataclean_dir))

# DEFINING ARGUMENTS
args <- commandArgs(trailingOnly = TRUE)
print("Length of args:")
print(length(args))
if (length(args) == 0) {
  # So we can use args when testing codes locally
  cohort <- "postcovid3" # e.g., "precovid", "postcovid1", etc.
  start_date <- as.Date("2024-10-01") # The index date for each cohort
} else {
  cohort <- args[[1]]
  start_date <- as.Date(args[[2]])
}

# OS
print("This should be the working directory")
wd <- getwd()
print(wd)

fs::dir_create(here::here("output", "measures"))
measures_path <- here::here("output", "measures")
output_path <- here::here("output")

print(here::here)

## IMPORTING FILES
# list.files: lists all the files in a specified directory
# pattern: option, only identifies files that match a specific regular expression
# grepl: an easier way to identify strings, b/c list.files doesn't support full regex in pattern (I think) # nolint

test <- list.files(path = "/workspace/output/measures", full.names = TRUE)
print("This is the test list")
print(test)

# FOR CODE DEVELOPMENT, USE: measures_csv <- list.files(path = measures_path, pattern = "postcovid3\\.csv$", full.names = TRUE)
# OS (incorporates the 'cohort' arguments needed for the .yaml file)
measures_csv <- list.files(
  path = measures_path,
  pattern = paste0(cohort, ".*\\.csv$"),
  full.names = TRUE
)

exp_measures_csv <- grep(
  "_apc|_ec|_consultation|_vax",
  measures_csv,
  invert = TRUE,
  value = TRUE
)

# Separate subgroup CS files from whole-practice CS files ----------
exp_subgroup_measures_csv <- exp_measures_csv[
  str_detect(basename(exp_measures_csv), "_sub_")
]

exp_measures_csv <- setdiff(exp_measures_csv, exp_subgroup_measures_csv)

exp_vax_measures_csv <- grep("_vax", measures_csv, value = TRUE)
exp_cons_measures_csv <- grep("_consultation", measures_csv, value = TRUE)

out_measures_csv <- grep("_apc|_ec", measures_csv, value = TRUE)

out_main_measures_csv <- grep(
  "main",
  out_measures_csv,
  value = TRUE
)

out_subgroup_measures_csv <- grep(
  "_sub_",
  out_measures_csv,
  value = TRUE
)

print("This should be the list of ALL the CSV files (measures_csv)")
print(measures_csv) # nolint

print("This should be the list of exp_measures_csv files")
print(exp_measures_csv)

print("This should be the list of exp_vax_measures_csv files")
print(exp_vax_measures_csv)

print("This should be the list of exp_cons_measures_csv files")
print(exp_cons_measures_csv)

print("This should be the list of out_measures_csv files")
print(out_measures_csv)

print("This should be the list of out_main_measures_csv files")
print(out_main_measures_csv)

print("This should be the list of out_subgroup_measures_csv files")
print(out_subgroup_measures_csv)

## CLEANING THE DATA
# Exposures (cross-sectional):
# "Transform" so that each proportion variable is it's own column. Data should be one row per practice. Then check & merge.

if (
  var_consistency_check(
    exp_measures_csv,
    var_list = c("interval_start", "interval_end")
  )
) {
  message("Variable consistency check passed for each dataset")

  # Pre-allocating objects
  wide_exp_measures <- vector("list", length(exp_measures_csv)) # list containing transformed datasets, set length = length of exp_measures_csv
  rename_list <- c(
    "numerator" = "num",
    "denominator" = "denom",
    "ratio" = "prop"
  ) # Renaming rules for dataset
  print("Pre-allocation done")

  # For-loop of the data management steps
  for (i in seq_along(exp_measures_csv)) {
    print("For-loop started - seq along exp_measures_csv")
    df <- readr::read_csv(exp_measures_csv[[i]])
    print("import csv into df")

    wide_exp_measures[[i]] <- df %>%
      pivot_wider(
        # Transform the dataset to "wide', i.e. one column per measure
        names_from = measure,
        values_from = c(numerator, denominator, ratio)
      ) %>%
      rename_with(~ str_replace_all(., rename_list)) # Renaming the variables

    # Check that each wide dataset now has one row per practice
    if (one_row_check(wide_exp_measures[[i]], "practice_id")) {} else {
      stop()
    }
    # Check that each numerical variable is non-negative
    if (all(positive_var_check(wide_exp_measures[[i]]))) {} else {
      stop()
    }
  }
  # Merging each dataset, dropping repeat variables
  merge_and_drop(
    wide_exp_measures,
    var_list = c("interval_start", "interval_end"),
    c("practice_id"),
    merged_df_name = "merged_exp_measures"
  )
  # Once again, checking that data are one row per practice
  if (one_row_check(merged_exp_measures, "practice_id")) {} else {
    message("merged_exp_measures is NOT one row per practice")
  }
  # Checking that each proportion variable goes between 0 and 1
  prop_vars <- names(merged_exp_measures)[grepl(
    "prop",
    names(merged_exp_measures)
  )]
  range_check(
    merged_exp_measures,
    var_list = prop_vars,
    min = 0.000000000000000000,
    max = 1.00000000000000000000000
  )
}


# Exposures (vaccines, cross-sectional)
# "Transform" so that each vax proportion variable is it's own column

if (
  var_consistency_check(
    exp_vax_measures_csv,
    var_list = c("interval_start", "interval_end")
  )
) {
  message(
    "Variable consistency check passed for each dataset in ",
    exp_vax_measures_csv
  )

  # Pre-allocating objects
  wide_exp_vax_measures <- vector("list", length(exp_vax_measures_csv)) # list containing transformed datasets
  rename_list <- c(
    "numerator" = "num",
    "denominator" = "denom",
    "ratio" = "prop"
  ) # Renaming rules for dataset
  print("Pre-allocation done")

  # For-loop of the data management steps
  for (i in seq_along(exp_vax_measures_csv)) {
    print(
      "Data management for-loop started: seq along files in exp_vax_measures_csv"
    )
    df <- readr::read_csv(exp_vax_measures_csv[[i]])
    print(".csv files imported into R as dataframe object,  df")

    print("Starting reshape to wide & rename")
    wide_exp_vax_measures[[i]] <- df %>% # Transform the dataset to "wide', i.e. one column per measure
      pivot_wider(
        names_from = measure,
        values_from = c(numerator, denominator, ratio)
      ) %>%
      rename_with(~ str_replace_all(., rename_list)) # Renaming the variables

    # Check that each wide dataset now has one row per practice
    if (one_row_check(wide_exp_vax_measures[[i]], "practice_id")) {} else {
      stop()
    }

    # Check that each numerical variable is non-negative
    if (all(positive_var_check(wide_exp_vax_measures[[i]]))) {} else {
      stop()
    }
  }

  # Merging each dataset, dropping repeat variables
  print("Starting to merge all the reshaped files in wide_exp_vax_measures")
  merge_and_drop(
    wide_exp_vax_measures,
    var_list = c("interval_start", "interval_end"),
    c("practice_id"),
    merged_df_name = "merged_exp_vax_measures"
  )

  # Once again, checking that data are one row per practice
  if (one_row_check(merged_exp_vax_measures, "practice_id")) {} else {
    message("merged_exp_vax_measures is NOT one row per practice")
  }

  # Checking that each proportion variable goes between 0 and 1
  prop_vars <- names(merged_exp_vax_measures)[grepl(
    "prop",
    names(merged_exp_vax_measures)
  )]
  range_check(
    merged_exp_vax_measures,
    var_list = prop_vars,
    min = 0.000000000000000000,
    max = 1.00000000000000000000000
  )
}


# Exposures (consultations, longitudinal) ---------------------------------------
# One row per practice, with separate monthly columns for each population.
print("Process monthly consultation measures")

start_date_cons <- start_date - years(1)

# Filenames use long subgroup names; columns use existing short names.
consultation_groups <- c(
  main = "",
  sub_asthma = "sub_asth",
  sub_copd = "sub_copd",
  sub_hypertension = "sub_htn",
  sub_diabetes = "sub_diab",
  sub_sev_mental_ill = "sub_sevmh"
)

wide_cons_list <- vector("list", length(consultation_groups))
names(wide_cons_list) <- names(consultation_groups)

for (population in names(consultation_groups)) {

  suffix <- unname(consultation_groups[[population]])

  # Identify the expected file -----------------------------------------------
  filename <- if (population == "main") {
    paste0("measures_consultation_", cohort, ".csv")
  } else {
    paste0(
      "measures_consultation_", cohort, "_", population, ".csv"
    )
  }

  file_i <- exp_cons_measures_csv[
    basename(exp_cons_measures_csv) == filename
  ]

  if (length(file_i) != 1L) {
    stop("Expected one consultation file: ", filename)
  }

  message("Processing consultation measures: ", population)

  # Check monthly intervals --------------------------------------------------
  date_check_exp_cons <- date_check_long(
    file_i,
    start_date_var_list = c("interval_start"),
    end_date_var_list = c("interval_end"),
    group_vars = NULL,
    start_date = start_date_cons,
    n_expected = 12,
    by = "1 month"
  )

  if (!isTRUE(date_check_exp_cons$date_check_passed)) {
    stop("Consultation date check failed for: ", filename)
  }

  df <- readr::read_csv(
    file_i,
    show_col_types = FALSE
  )

  # Retain patients belonging to the subgroup --------------------------------
  if (population != "main") {

    if (!suffix %in% names(df)) {
      stop(
        "Missing subgroup indicator ", suffix,
        " in ", filename
      )
    }

    # Accept logical TRUE or its usual CSV representations.
    df <- df %>%
      filter(
        tolower(as.character(.data[[suffix]])) %in%
          c("true", "t", "1")
      )
  }

  # Check the expected measure name ------------------------------------------
  expected_measure <- if (population == "main") {
    "cons_m"
  } else {
    paste0("cons_m_", suffix)
  }

  if (
    anyNA(df$measure) ||
      any(df$measure != expected_measure)
  ) {
    stop("Unexpected consultation measure name in: ", filename)
  }

  df <- df %>%
    select(
      practice_id,
      interval_start,
      interval_end,
      numerator,
      denominator,
      ratio
    ) %>%
    mutate(
      interval_start = as.Date(interval_start),
      interval_end = as.Date(interval_end)
    )

  # Check that consultations precede the winter cohort -----------------------
  if (
    anyNA(df$interval_start) ||
      anyNA(df$interval_end) ||
      any(df$interval_start < start_date_cons) ||
      any(df$interval_end >= start_date)
  ) {
    stop("Consultation intervals outside the exposure period: ", filename)
  }

  # Prevent duplicate practice-month rows from producing list columns --------
  duplicate_rows <- df %>%
    count(practice_id, interval_start) %>%
    filter(n > 1L)

  if (nrow(duplicate_rows) > 0L) {
    stop("Duplicate practice-month consultation rows in: ", filename)
  }

  # Check counts -------------------------------------------------------------
  if (
    any(df$numerator < 0, na.rm = TRUE) ||
      any(df$denominator < 0, na.rm = TRUE)
  ) {
    stop("Negative consultation counts or denominators in: ", filename)
  }

  # Calculate rates using each month's own denominator -----------------------
  # These are consultation counts per patient and can exceed 1.
  df <- df %>%
    mutate(
      ratio = if_else(
        !is.na(denominator) & denominator > 0,
        numerator / denominator,
        NA_real_
      ),
      month_abbr = tolower(format(interval_start, "%b"))
    )

  # Reshape to one row per practice ------------------------------------------
  wide_df <- df %>%
    pivot_wider(
      id_cols = practice_id,
      names_from = month_abbr,
      values_from = c(numerator, denominator, ratio),
      names_glue = "{.value}_{month_abbr}"
    ) %>%
    rename_with(
      ~ str_replace(.x, "^numerator_", "num_cons_"),
      starts_with("numerator_")
    ) %>%
    rename_with(
      ~ str_replace(.x, "^denominator_", "denom_cons_"),
      starts_with("denominator_")
    ) %>%
    rename_with(
      ~ str_replace(.x, "^ratio_", "prop_cons_"),
      starts_with("ratio_")
    )

  # Add the subgroup suffix, preserving overall column names -----------------
  if (population != "main") {
    wide_df <- wide_df %>%
      rename_with(
        ~ paste0(.x, "_", suffix),
        -practice_id
      )
  }

  if (!one_row_check(wide_df, "practice_id")) {
    stop("Consultation data are not one row per practice: ", population)
  }

  wide_cons_list[[population]] <- wide_df

  message(
    "Processed consultation measures for ", population,
    ": ", nrow(wide_df), " practices."
  )
}

# Combine populations --------------------------------------------------------
# Retain practices in the overall consultation data.
# Missing subgroup records remain NA.
wide_exp_cons_measures <- wide_cons_list[["main"]]

for (population in setdiff(names(consultation_groups), "main")) {
  wide_exp_cons_measures <- wide_exp_cons_measures %>%
    left_join(
      wide_cons_list[[population]],
      by = "practice_id"
    )
}

message(
  "Combined consultation measures: ",
  nrow(wide_exp_cons_measures),
  " practices."
)

# Outcomes-main (longitudinal):
# Just need to date check & merge (structurally, can keep as is: one row per practice per week)
print("date_check_out for longitudinal OUTCOMES")
date_check_out <- date_check_long(
  out_measures_csv,
  start_date_var_list = c("interval_start"),
  end_date_var_list = c("interval_end"),
  group_vars = NULL,
  start_date = start_date,
  n_expected = 20,
  by = "1 week"
)
print("if date_check_out passed")
if (date_check_out$date_check_passed) {
  ## Pre-allocating objects
  wide_out_measures <- vector("list", length(out_measures_csv)) # list containing transformed datasets
  rename_list <- c(
    "numerator" = "num",
    "denominator" = "denom",
    "ratio" = "prop"
  )

  # For-loop of the data management steps
  for (i in seq_along(out_measures_csv)) {
    file_i <- out_measures_csv[[i]]

    # Processing subgroup files differently (mainly filtering to TRUE subgroup)
    if (file_i %in% out_subgroup_measures_csv) {
      wide_out_measures[[i]] <- process_subgroup_file(file_i)
    } else {
      wide_out_measures[[i]] <- readr::read_csv(file_i)
    }

    # Check that there are multiple rows per practice
    if (!one_row_check(wide_out_measures[[i]], "practice_id")) {
      message("There are multiple rows per practice in dataset: ", i)
    } else {
      stop()
    }

    # Check that each numerical variable is non-negative
    if (all(positive_var_check(wide_out_measures[[i]]))) {} else {
      stop()
    }
    print("Renaming existing variables in wide_out_measures")
    # Using "reshape", but actually we're just renaming the existing variables and dropping the measure var
    wide_out_measures[[i]] <- wide_out_measures[[i]] %>%
      pivot_wider(
        names_from = measure,
        values_from = c(numerator, denominator, ratio)
      ) %>%
      rename_with(~ str_replace_all(., rename_list))
  }

  # Check that interval_start and interval_end are consistent ACROSS datasets
  if (
    !identical_vector_check(
      wide_out_measures,
      var_list = c("interval_start", "interval_end")
    )
  ) {
    warning(
      "The number of rows (per practice per time interval) are not consistent across outcome datasets.",
      call. = FALSE
    )
  }

  # Merge and delete duplicate columns
  merge_and_drop(
    wide_out_measures,
    var_list = c("interval_end"),
    join_var = c("practice_id", "interval_start"),
    merged_df_name = "merged_out_measures"
  )

  # Check again that there are multiple rows per practice
  if (!one_row_check(merged_out_measures, "practice_id")) {
    message("OK - merged_out_measures is multiple rows per practice")
  } else {
    message(
      "ERROR - something weird happened and merged_exp_measures is one row per practice"
    )
  }
  # Checking that each proportion variable goes between 0 and 1
  prop_vars <- names(merged_out_measures)[grepl(
    "prop",
    names(merged_out_measures)
  )]
  range_check(
    merged_out_measures,
    var_list = prop_vars,
    min = 0.000000000000000000,
    max = 1.00000000000000000000000
  )
} else {
  print("Date check NOT passed for wide_out_measures")
}

# Merging the exposures, exposures_vax, exposures_cons, outcomes, and outcomes_acscs data together
exp_data <- inner_join(
  merged_exp_measures,
  merged_exp_vax_measures,
  by = "practice_id"
) %>%
  rename(
    exp_interval_start = interval_start.x,
    exp_interval_end = interval_end.x,
    exp_vax_interval_start = interval_start.y,
    exp_vax_interval_end = interval_end.y
  )
# Check for duplicate denominator vars - drop the duplicates, highlight any that are unique
denom_vars <- grep("denom_", names(exp_data), value = TRUE)
exp_data <- drop_all_duplicates(
  exp_data,
  df_name = "exp_data",
  denom_vars,
  new_name = "list_size"
)

# Now merge in the wide GP consulations data
# Do in this order so that the denom vars in GP cons are not dropped!
exp_data <- inner_join(
  exp_data,
  wide_exp_cons_measures,
  by = "practice_id"
)

# Add subgroup CS measures after whole-practice denominator cleaning --
# Input filenames/flags use long names; output columns use existing short names.
subgroup_suffixes <- c(
  sub_asthma = "sub_asth",
  sub_copd = "sub_copd",
  sub_hypertension = "sub_htn",
  sub_diabetes = "sub_diab",
  sub_sev_mental_ill = "sub_sevmh"
)

cs_args <- c(
  "Age", "Sex", "Ethnicity", "IMD", "Rurality",
  "Carehome", "Smoking", "Obesity", "Multimorbidity"
)

for (subgroup in names(subgroup_suffixes)) {
  suffix <- unname(subgroup_suffixes[[subgroup]])
  message("Merging cross-sectional measures: ", subgroup)

  expected_files <- paste0(
    "measures_", tolower(cs_args), "_", cohort, "_", subgroup, ".csv"
  )
  missing_files <- setdiff(
    expected_files, basename(exp_subgroup_measures_csv)
  )
  if (length(missing_files) > 0L) {
    stop(
      "Missing cross-sectional subgroup files: ",
      paste(missing_files, collapse = ", ")
    )
  }

  subgroup_files <- exp_subgroup_measures_csv[
    match(expected_files, basename(exp_subgroup_measures_csv))
  ]
  subgroup_measures <- purrr::map_dfr(
    subgroup_files,
    ~ readr::read_csv(.x, show_col_types = FALSE)
  )

  subgroup_intervals <- subgroup_measures %>%
    distinct(interval_start, interval_end)
  if (
    nrow(subgroup_intervals) != 1L ||
      anyNA(subgroup_intervals) ||
      !all(as.Date(subgroup_intervals$interval_start) == start_date)
  ) {
    stop("Unexpected baseline interval for ", subgroup)
  }

  duplicate_measures <- subgroup_measures %>%
    count(practice_id, measure) %>%
    filter(n > 1L)
  if (nrow(duplicate_measures) > 0L) {
    stop("Duplicate practice-measure rows for ", subgroup)
  }

  # Reshape subgroup measures, retaining their denominators
  subgroup_wide <- subgroup_measures %>%
    select(
      practice_id,
      measure,
      numerator,
      denominator,
      ratio
    ) %>%
    mutate(
      ratio = if_else(is.finite(ratio), ratio, NA_real_)
    ) %>%
    pivot_wider(
      id_cols = practice_id,
      names_from = measure,
      values_from = c(numerator, denominator, ratio)
    ) %>%
    rename_with(
      ~ str_replace(.x, "^numerator_", "num_"),
      starts_with("numerator_")
    ) %>%
    rename_with(
      ~ str_replace(.x, "^denominator_", "denom_"),
      starts_with("denominator_")
    ) %>%
    rename_with(
      ~ str_replace(.x, "^ratio_", "prop_"),
      starts_with("ratio_")
    ) %>%
    rename_with(
      ~ paste0(.x, "_", suffix),
      -practice_id
    )

  # Collapse identical denominators using the existing function
  denom_vars <- grep(
    "^denom_",
    names(subgroup_wide),
    value = TRUE
  )

  # Use the age denominator as the first/reference denominator
  reference_denom <- paste0("denom_age_0_4_", suffix)

  denom_vars <- c(
    reference_denom,
    setdiff(denom_vars, reference_denom)
  )

  subgroup_wide <- drop_all_duplicates(
    subgroup_wide,
    df_name = paste0("subgroup_wide_", suffix),
    var_list = denom_vars,
    new_name = paste0("list_size_", suffix)
  )

  exp_data <- exp_data %>%
    left_join(subgroup_wide, by = "practice_id")
}

# Merging the exposure and outcome data together to create the final analytic dataset
analytic_data_long <- inner_join(
  merged_out_measures,
  exp_data,
  by = "practice_id"
) %>% # Merging the exp data to the longitudinal outcomes, excluding the new practice apears after winter.
  rename(
    out_interval_start = interval_start,
    out_interval_end = interval_end
  ) %>%
  group_by(practice_id) %>%
  mutate(week_number = dense_rank(out_interval_start)) %>%
  ungroup()

# #Prep to check & transform analytic_data_long
# date_vars <- grep("interval", names(analytic_data_long), value = TRUE)
# cons_vars <- grep("_cons_", names(analytic_data_long), value = TRUE)
# out_vars <- grep("_main|_sub", names(analytic_data_long), value = TRUE)

# #Check that the exp_variables merged correctly into the long dataset
# #i.e. all have one unique value per practice (per outcome week)
# exp_vars <- names(analytic_data_long)[!grepl(
#   paste(c(date_vars, cons_vars, out_vars), collapse = "|"),
#   names(analytic_data_long)
# )]

# exp_merge_check <- all(
#   analytic_data_long %>%
#     group_by(practice_id) %>%
#     summarise(
#       across(all_of(exp_vars), ~ n_distinct(.x) == 1),
#       .groups = "drop"
#     ) %>%
#     select(-practice_id) %>%
#     unlist()
# )

# if (exp_merge_check) {
#   analytic_data_wide <- analytic_data_long %>%
#     pivot_wider(
#       id_cols = c(practice_id, cons_vars, exp_vars),
#       names_from = week_number,
#       values_from = out_vars,
#       names_glue = "{.value}{week_number}"
#     )
# }

# EXPORTING MERGED DATASET
print("Saving merged datasets")

# Long dataset
output_path_long <- paste0(dataclean_dir, "merged_data_long_", cohort, ".csv.gz")

data.table::fwrite(
  analytic_data_long,
  here::here(output_path_long),
  compress = "gzip"
)

message(paste0(
  "Long-format merged dataset saved to ",
  output_path_long
))

# Wide dataset
# output_path_wide <- paste0(dataclean_dir, "merged_data_wide_", cohort, ".csv")
# data.table::fwrite(analytic_data_wide, here::here(output_path_wide))
# message(paste0("Wide-format merged dataset saved to ", output_path_wide))

# TO DO:
# Get the positive_var_check function to output a nice dataset (like date_check_long does)
# Maybe: Check that each denom variable is the same, and then drop them (because they should just be the practice list size)
# Only issue is if some patients have missing data on key characteristics.
# Add the number of registered patients used to calculate each proportion variable
# CHECK that this number is consistent within each dataset, and for each category variable
# Create the CMS

# FROM LP:
# I've been thinking about this indicator and what it means.
# I think it is largely an indicator of supply of appts, driven by practice (rather than patient demand).
# However it is not clear cut and could also be somewhat demand driven.
# Thinking about how to better understand the 'demand' element, I wonder if there may be variation between a practice that provides a lot of appts to a small proportion of their population (ie to the high demand and high need cohorts) Vs those who may provide the same average number of appointments but with a more even spread e.g a university practice.  I wonder if how appointments are distributed/managed across the registered patient list therefore may have a knock on effect in secondary care due to unmet demand from patients with different case-mixes. Would it be feasible to develop an indicator that would capture this e.g. the range of number of consultations/pt, the IQR, the SD?

## Code to clear items from memory, and free unused memory
# rm(list = ls())
# gc()
