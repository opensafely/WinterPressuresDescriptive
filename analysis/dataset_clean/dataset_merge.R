## IMPORTING R PACKAGES
library(tidyverse)
library(haven) # Allows you to import STATA .dta files
library(glue)
library(lubridate)
library(here)
library(data.table) # Allows you to import .csv files, and write .csv files
library(fs)
#library(arrow)

# Define clean dataset output folder -------------------------------------------
print("Creating output/dataset_clean output folder")

dataclean_dir <- "output/dataset_clean/"
dir_create(here::here(dataclean_dir))

#DEFINING ARGUMENTS
args <- commandArgs(trailingOnly = TRUE)
print("Length of args:")
print(length(args))
if (length(args) == 0) {
  #So we can use args when testing codes locally
  cohort <- "precovid" # e.g., "precovid", "postcovid1", etc.
  start_date <- as.Date("2018-10-01") #The index date for each cohort
} else {
  cohort <- args[[1]]
  start_date <- as.Date(args[[2]])
}


#DEFINING FUNCTIONS
##var_consistency_check:
## checks if the VALUES of a specific variable are the same:
#1. For the entire variable WITHIN a file & 2. For the variable if it exists across multiple files
## Arg 1: file_list: list of files you want to loop over
## Arg 2: var_list: list of variables you want to check
#Other functions used when defining "var_consistency_check"
#message: apparently a better way to print text than "print"

var_consistency_check <- function(file_list, var_list) {
  #Makes sure the second argument in the function is a character vector of column names
  if (!is.character(var_list)) {
    stop("`var_list` must be a character vector of column names.")
  }

  #Store reference values (from the first file)
  ref_values <- list()
  result <- TRUE #Add a function "result", start assuming the result = TRUE

  #Sequentially upload each .csv file in the file_list
  for (i in seq_along(file_list)) {
    df <- readr::read_csv(file_list[[i]])
    for (var in var_list) {
      #Then, sequentially go through each variable in the var_list
      if (!var %in% colnames(df)) {
        #Check whether each variable is actually in the dataset
        warning("Variable '", var, "' not found in dataset: ", i)
        result <- FALSE
        next #Allows the loop to continue?
      }
      # Variable - internal consistency: Check values are the same for each ROW in the dataset
      if (length(unique(df[[var]])) != 1) {
        #If the number of unique values in this var !=1
        warning("Error - var: '", var, "' has multiple values in dataset: ", i)
        result <- FALSE
      }
      #Variable - cross-file consistency, as compared to reference:
      if (i == 1) {
        #If it's the first file, set the reference values = the values from the first file
        ref_values[[var]] <- unique(df[[var]])
      } else {
        #If it's not the first file
        if (!setequal(unique(df[[var]]), ref_values[[var]])) {
          #Compare the values from this file to the reference values
          warning("Variable '", var, "' is inconsistent across datasets: ", i)
          result <- FALSE
        }
      }
    }
  }
  return(result)
}


##date_check_long
#Check date variable exists
#Check that the date values = specific SET of SEQUENTIAL values, BY certain variables
#Check that the date values are the same across datasets

date_check_long <- function(
  file_list,
  start_date_var_list,
  end_date_var_list,
  group_vars = NULL,
  start_date = NULL,
  n_expected = NULL,
  by = "1 week"
) {
  #First write a "helper" function to create all the components for the check
  date_check_long_helper <- function(df, date_var, is_start, i) {
    if (!date_var %in% colnames(df)) {
      #Check if the SPECIFIC DATE VARIABLE exists in your data & format it as a date (just in case)
      warning("Variable '", date_var, "' not found in dataset: ", i)
      return(NULL)
    }
    df[[date_var]] <- as.Date(df[[date_var]])
    start_date <- as.Date(start_date)

    #Create an "expected" sequence of dates, based on whether date_var contains starting or end dates
    expected <- if (is_start) {
      seq(min(df[[date_var]]), max(df[[date_var]]), by = by)
    } else {
      if (by == '1 month') {
        ceiling_date(
          seq(
            ymd(start_date),
            ymd(start_date + years(1) - months(1)),
            by = '1 month'
          ),
          unit = "months"
        ) -
          days(1)
      } else if (by == '1 week') {
        seq(
          ymd(start_date + days(6)),
          ymd(start_date + days(7) * (n_expected) - days(1)),
          by = '1 week'
        )
      }
    }

    #Create the actual set of dates
    actual <- sort(unique(df[[date_var]]))

    #Create a summary dataset
    date_check_summary <- df %>%
      group_by(across(all_of(group_vars))) %>% #groups the data by the variables specified in the group_vars argument
      summarise(
        #Creates a summary dataset containing the below specified columns (min_date, max_date, etc)
        dataset = i,
        var_type = if (is_start) "start" else "end",
        date_var = date_var,
        min_date = min(df[[date_var]]),
        max_date = max(df[[date_var]]),
        expected_dates = list(expected),
        actual_dates = list(actual),
        sequence_check = identical(actual, expected),
        start_date_check = if (is_start & !is.null(start_date)) {
          min(df[[date_var]]) == as.Date(start_date)
        } else {
          NA
        },
        n_check = if (!is.null(n_expected)) {
          n_distinct(df[[date_var]]) == n_expected
        } else {
          NA
        },
        .groups = "drop"
      )
  }

  #Now we're back in the "main" function
  #NOTE: We've defined the helper above, but haven't called it yet, so it hasn't created the summary dataset yet
  result_list <- list() # store all results

  for (i in seq_along(file_list)) {
    #Looping over each file in the dataset
    df <- read_csv(file_list[[i]]) # Importing the file

    pair_results <- map2_dfr(
      start_date_var_list,
      end_date_var_list,
      ~ {
        #Pair the start date/end date variables
        bind_rows(
          date_check_long_helper(df, .x, TRUE, i), # start variable
          date_check_long_helper(df, .y, FALSE, i) # end variable
        )
      }
    )

    result_list[[i]] <- pair_results
  }

  final_result <- bind_rows(result_list)

  list(
    final_result = final_result,
    date_check_passed = all(final_result$sequence_check, na.rm = TRUE),
    start_check_passed = all(final_result$start_date_check, na.rm = TRUE),
    n_check_passed = all(final_result$n_check, na.rm = TRUE)
  )
}

##To process each subgroup file
process_subgroup_file <- function(file) {
  df <- readr::read_csv(file)

  # ---- identify subgroup variable inside the dataset
  sub_var <- grep("^sub_", names(df), value = TRUE)

  if (length(sub_var) != 1) {
    stop(
      "Expected exactly one subgroup flag variable but found: ",
      paste(sub_var, collapse = ", ")
    )
  }

  if (!all(c(TRUE, FALSE) %in% unique(df[[sub_var]]))) {
    stop("Subgroup flag variable must be logical (TRUE/FALSE)")
  }

  # ---- keep only the TRUE group
  df <- df %>% filter(.data[[sub_var]] == TRUE)

  # ---- drop the subgroup flag
  df <- df %>% select(-all_of(sub_var))

  return(df)
}

##Identical_vector_check:
##Checks whether variables are COMPLETELY IDENTICAL across multiple dataframes
identical_vector_check <- function(df_list, var_list) {
  result <- TRUE
  for (i in seq_along(df_list)) {
    for (var in var_list) {
      result <- identical(df_list[[1]][[var]], df_list[[i]][[var]])
      if (!(result == "TRUE")) {
        message(
          "Variable '",
          var,
          "'in dataset '",
          i,
          "' is NOT identical to dataset 1"
        )
        result <- FALSE
      } else {
        print("All good!")
      }
    }
  }
  return(result)
}

##one_row_check
##Checks whether the dataframe has one row per id (unit of interest)
one_row_check <- function(df, id) {
  df_name <- deparse(substitute(df)) # captures the name of the data frame
  total_rows <- nrow(df)
  unique_ids <- dplyr::n_distinct(df[[id]])
  result <- total_rows == unique_ids

  if (result) {
    message("One row check = TRUE in dataset: ", df_name)
  } else {
    message(
      "Dataset is NOT one row per practice. Total rows: ",
      total_rows,
      ", Unique IDs: ",
      unique_ids,
      ", Dataset: ",
      df_name
    )
  }
  return(result)
}

##positive_var_check
#Checks whether the values of numeric variables are positive
positive_var_check <- function(df) {
  df_name <- deparse(substitute(df)) # captures the name of the data frame

  numeric_vars <- sapply(df, is.numeric) & names(df) != "practice_id"
  if (!any(numeric_vars)) {
    warning("ERROR! No numeric variables found in the dataset.")
    return(NULL)
  }

  result <- sapply(df[numeric_vars], function(col) all(col >= 0, na.rm = TRUE))
  if (all(result)) {
    message("Positive var check = TRUE in dataset: ", df_name)
  } else {
    negative_vars <- names(result)[!result]
    warning(
      "ERROR! The following variables contain negative values: ",
      paste(negative_vars, collapse = ", ")
    )
  }
  return(result)
}

##range_check
#Checks whether the value of a specific variable fits within a certain range
range_check <- function(df, var_list, min, max) {
  for (var in var_list) {
    if (!var %in% names(df)) {
      stop("Variable '", var, "' not found in the dataset.")
    }
    if (!is.numeric(df[[var]])) {
      stop("Variable '", var, "' is not numeric.")
    }

    #Now doing the actual check to see if all values of var are within the range (inclusive)
    values <- df[[var]]
    result <- all(values >= min & values <= max, na.rm = TRUE)

    if (result) {
      message(
        "All values in '",
        var,
        "' are within the range [",
        min,
        ", ",
        max,
        "]"
      )
    } else {
      warning(
        "Some values in '",
        var,
        "' are outside the range [",
        min,
        ", ",
        max,
        "]"
      )
    }
  }
  return(result)
}

##Rename if var exists: renames multiple variables across multiple files
rename_if_var_exists <- function(file_list, rename_rules) {
  lapply(file_list, function(df) {
    df %>% rename_with(~ str_replace_all(., rename_rules))
  })
}

##merge_and_drop:
##Merges dataset, and drops duplicate variables
merge_and_drop <- function(
  df_list,
  var_list,
  join_var,
  merged_df_name = "merged_df"
) {
  merged_df <- reduce(df_list, full_join, by = join_var)

  #Identify if any of the vars in var_list are now duplicates, post-merge
  duplicate_vars <- grep(
    paste0("^(", paste(var_list, collapse = "|"), ")"),
    colnames(merged_df),
    value = TRUE
  )

  #Create a list of vars to remove, but make sure to KEEP the first instance of each var
  remove_vars <- unlist(lapply(var_list, function(v) {
    matches <- grep(paste0("^", v), duplicate_vars, value = TRUE)
    if (length(matches) > 1) matches[-1] else character(0)
  }))

  #Keeping the first instance of the var, and removing the .x suffix
  merged_df <- merged_df %>%
    select(-all_of(remove_vars)) %>%
    rename_with(~ str_replace(., "\\.x$", "")) #Removing the .x suffix
  message("Dataset merged and duplicates removed successfully")
  assign(merged_df_name, merged_df, envir = .GlobalEnv) #adds new dataframe to the global environment
}


#Check how many duplicates you have, drop all duplicates
drop_all_duplicates <- function(
  df,
  df_name = "name",
  var_list,
  new_name = "denom"
) {
  # Identify which variables are duplicates (by value)
  duplicate_vars <- duplicated(as.list(df[var_list]))

  keep_vars <- var_list[!duplicate_vars]
  remove_vars <- var_list[duplicate_vars]

  # Warn if multiple non-duplicate variables are found
  if (length(keep_vars) > 1) {
    message(
      "The following denominator variables in ",
      df_name,
      " are NOT duplicates"
    )
    message("All non-duplicates will be kept:")
    print(keep_vars)
    message(
      "The first instance ",
      keep_vars[1],
      " (renamed to '",
      new_name,
      "')"
    )
    message("Removed duplicates: ", paste(remove_vars, collapse = ", "))
  } else if (length(keep_vars) == 0) {
    stop("No non-duplicate variables were found")
  }
  # Rename the first non-duplicate
  df <- df %>% rename(!!new_name := all_of(keep_vars[1]))

  # Drop duplicates
  df <- df %>% select(-all_of(remove_vars))

  return(df)
}

#OS
print("This should be the working directory")
wd <- getwd()
print(wd)

fs::dir_create(here::here("output", "measures"))
measures_path <- here::here("output", "measures")
output_path <- here::here("output")

print(here::here)

##IMPORTING FILES
#list.files: lists all the files in a specified directory
#pattern: option, only identifies files that match a specific regular expression
#grepl: an easier way to identify strings, b/c list.files doesn't support full regex in pattern (I think) # nolint

test <- list.files(path = "/workspace/output/measures", full.names = TRUE)
print("This is the test list")
print(test)

#FOR CODE DEVELOPMENT, USE: measures_csv <- list.files(path = measures_path, pattern = "postcovid3\\.csv$", full.names = TRUE)
#OS (incorporates the 'cohort' arguments needed for the .yaml file)
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

##CLEANING THE DATA
#Exposures (cross-sectional):
#"Transform" so that each proportion variable is it's own column. Data should be one row per practice. Then check & merge.

if (
  var_consistency_check(
    exp_measures_csv,
    var_list = c("interval_start", "interval_end")
  )
) {
  message("Variable consistency check passed for each dataset")

  #Pre-allocating objects
  wide_exp_measures <- vector("list", length(exp_measures_csv)) #list containing transformed datasets, set length = length of exp_measures_csv
  rename_list <- c(
    "numerator" = "num",
    "denominator" = "denom",
    "ratio" = "prop"
  ) #Renaming rules for dataset
  print("Pre-allocation done")

  #For-loop of the data management steps
  for (i in seq_along(exp_measures_csv)) {
    print("For-loop started - seq along exp_measures_csv")
    df <- readr::read_csv(exp_measures_csv[[i]])
    print("import csv into df")

    wide_exp_measures[[i]] <- df %>%
      pivot_wider(
        #Transform the dataset to "wide', i.e. one column per measure
        names_from = measure,
        values_from = c(numerator, denominator, ratio)
      ) %>%
      rename_with(~ str_replace_all(., rename_list)) #Renaming the variables

    #Check that each wide dataset now has one row per practice
    if (one_row_check(wide_exp_measures[[i]], "practice_id")) {} else {
      stop()
    }
    #Check that each numerical variable is non-negative
    if (all(positive_var_check(wide_exp_measures[[i]]))) {} else {
      stop()
    }
  }
  #Merging each dataset, dropping repeat variables
  merge_and_drop(
    wide_exp_measures,
    var_list = c("interval_start", "interval_end"),
    c("practice_id"),
    merged_df_name = "merged_exp_measures"
  )
  #Once again, checking that data are one row per practice
  if (one_row_check(merged_exp_measures, "practice_id")) {} else {
    message("merged_exp_measures is NOT one row per practice")
  }
  #Checking that each proportion variable goes between 0 and 1
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


#Exposures (vaccines, cross-sectional)
#"Transform" so that each vax proportion variable is it's own column

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

  #Pre-allocating objects
  wide_exp_vax_measures <- vector("list", length(exp_vax_measures_csv)) #list containing transformed datasets
  rename_list <- c(
    "numerator" = "num",
    "denominator" = "denom",
    "ratio" = "prop"
  ) #Renaming rules for dataset
  print("Pre-allocation done")

  #For-loop of the data management steps
  for (i in seq_along(exp_vax_measures_csv)) {
    print(
      "Data management for-loop started: seq along files in exp_vax_measures_csv"
    )
    df <- readr::read_csv(exp_vax_measures_csv[[i]])
    print(".csv files imported into R as dataframe object,  df")

    print("Starting reshape to wide & rename")
    wide_exp_vax_measures[[i]] <- df %>% #Transform the dataset to "wide', i.e. one column per measure
      pivot_wider(
        names_from = measure,
        values_from = c(numerator, denominator, ratio)
      ) %>%
      rename_with(~ str_replace_all(., rename_list)) #Renaming the variables

    #Check that each wide dataset now has one row per practice
    if (one_row_check(wide_exp_vax_measures[[i]], "practice_id")) {} else {
      stop()
    }

    #Check that each numerical variable is non-negative
    if (all(positive_var_check(wide_exp_vax_measures[[i]]))) {} else {
      stop()
    }
  }

  #Merging each dataset, dropping repeat variables
  print("Starting to merge all the reshaped files in wide_exp_vax_measures")
  merge_and_drop(
    wide_exp_vax_measures,
    var_list = c("interval_start", "interval_end"),
    c("practice_id"),
    merged_df_name = "merged_exp_vax_measures"
  )

  #Once again, checking that data are one row per practice
  if (one_row_check(merged_exp_vax_measures, "practice_id")) {} else {
    message("merged_exp_vax_measures is NOT one row per practice")
  }

  #Checking that each proportion variable goes between 0 and 1
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


#Exposures (consultations, longitudinal)
#Date check & reshape wide (one row per practice, each month is a separate column)
print("date_check_exp_cons for longitudinal exposure - consultation")

start_date_cons = start_date - years(1)
date_check_exp_cons <- date_check_long(
  exp_cons_measures_csv,
  start_date_var_list = c("interval_start"),
  end_date_var_list = c("interval_end"),
  group_vars = NULL,
  start_date = start_date_cons,
  n_expected = 12,
  by = "1 month"
)

if (date_check_exp_cons$date_check_passed) {
  ##Pre-allocating objects
  #wide_exp_cons_measures <- vector("list", length(exp_cons_measures_csv))   #list containing transformed datasets
  rename_list <- c(
    "numerator" = "num_cons",
    "denominator" = "denom_cons",
    "ratio" = "prop_cons"
  ) #Renaming rules for dataset

  #Importing the single .csv (don't need to loop through datasets for this exposure)
  #For-loop of the data management steps
  #Even though GP cpnsultations is only 1 .csv file, put in for loop so checking functions work

  wide_exp_cons_measures <- readr::read_csv(exp_cons_measures_csv)

  #Check that there are multiple rows per practice
  if (!one_row_check(wide_exp_cons_measures, "practice_id")) {
    message(
      "There are multiple rows per practice in dataset: exp_cons_measures"
    )
  } else {
    stop()
  }

  #Check that each numerical variable is non-negative
  if (all(positive_var_check(wide_exp_cons_measures))) {} else {
    stop()
  }
  #Check that all the interval_start dates in the GP consultations .csv are BEFORE the index date for the other exposure vars
  if (
    max(wide_exp_cons_measures$interval_start) <
      min(merged_exp_measures$interval_start)
  ) {
    print(
      "TRUE: All interval_start dates in wide_exp_cons_measures are BEFORE the interval_start dates in merged_exp_measures"
    )
  } else {
    print(
      "FALSE: All interval_start dates in wide_exp_cons_measures are NOT BEFORE the interval_start dates in merged_exp_measures"
    )
  }

  print("Reshaping & renaming exp_cons_measures.csv")
  #Reshaping to wide and renaming
  wide_exp_cons_measures <- wide_exp_cons_measures %>%
    mutate(month_abbr = tolower(format(interval_start, "%b"))) %>%
    pivot_wider(
      id_cols = practice_id,
      names_from = month_abbr,
      values_from = c(numerator, denominator, ratio),
      names_glue = "{.value}_{month_abbr}"
    ) %>%
    rename_with(~ str_replace_all(., rename_list))

  #Check again that there is now ONE row per practice
  if (one_row_check(wide_exp_cons_measures, "practice_id")) {
    message("OK - merged_out_measures is one row per practice")
  } else {
    message(
      "ERROR - something weird happened and merged_exp_measures is STILL multiple rows per practice"
    )
  }
  #Checking that each proportion variable goes between 0 and 1
  prop_vars <- names(wide_exp_cons_measures)[grepl(
    "prop",
    names(wide_exp_cons_measures)
  )]
  range_check(
    wide_exp_cons_measures,
    var_list = prop_vars,
    min = 0.000000000000000000,
    max = 1.00000000000000000000000
  )
  print("if date_check_cons passed")
} else {
  print("Date check NOT passed for wide_exp_cons_measures")
}


#Outcomes-main (longitudinal):
#Just need to date check & merge (structurally, can keep as is: one row per practice per week)
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
  ##Pre-allocating objects
  wide_out_measures <- vector("list", length(out_main_measures_csv)) #list containing transformed datasets
  rename_list <- c(
    "numerator" = "num",
    "denominator" = "denom",
    "ratio" = "prop"
  )

  #For-loop of the data management steps
  for (i in seq_along(out_measures_csv)) {
    file_i <- out_measures_csv[[i]]

    # Processing subgroup files differently (mainly filtering to TRUE subgroup)
    if (file_i %in% out_subgroup_measures_csv) {
      wide_out_measures[[i]] <- process_subgroup_file(file_i)
    } else {
      wide_out_measures[[i]] <- readr::read_csv(file_i)
    }

    #Check that there are multiple rows per practice
    if (!one_row_check(wide_out_measures[[i]], "practice_id")) {
      message("There are multiple rows per practice in dataset: ", i)
    } else {
      stop()
    }

    #Check that each numerical variable is non-negative
    if (all(positive_var_check(wide_out_measures[[i]]))) {} else {
      stop()
    }
    print("Renaming existing variables in wide_out_measures")
    #Using "reshape", but actually we're just renaming the existing variables and dropping the measure var
    wide_out_measures[[i]] <- wide_out_measures[[i]] %>%
      pivot_wider(
        names_from = measure,
        values_from = c(numerator, denominator, ratio)
      ) %>%
      rename_with(~ str_replace_all(., rename_list))
  }

  #Check that interval_start and interval_end are consistent ACROSS datasets
  if (
    identical_vector_check(
      wide_out_measures,
      var_list = c("interval_start", "interval_end")
    )
  ) {} else {
    print("Interval start and interval end are not consistent across datasets ")
    stop()
  }

  #Merge and delete duplicate columns
  merge_and_drop(
    wide_out_measures,
    var_list = c("interval_end"),
    join_var = c("practice_id", "interval_start"),
    merged_df_name = "merged_out_measures"
  )

  #Check again that there are multiple rows per practice
  if (!one_row_check(merged_out_measures, "practice_id")) {
    message("OK - merged_out_measures is multiple rows per practice")
  } else {
    message(
      "ERROR - something weird happened and merged_exp_measures is one row per practice"
    )
  }
  #Checking that each proportion variable goes between 0 and 1
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

#Merging the exposures, exposures_vax, exposures_cons, outcomes, and outcomes_acscs data together
exp_data <- left_join(
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
#Check for duplicate denominator vars - drop the duplicates, highlight any that are unique
denom_vars <- grep("denom_", names(exp_data), value = TRUE)
exp_data <- drop_all_duplicates(
  exp_data,
  df_name = "exp_data",
  denom_vars,
  new_name = "list_size"
)

#Now merge in the wide GP consulations data
#Do in this order so that the denom vars in GP cons are not dropped!
exp_data <- left_join(
  exp_data,
  wide_exp_cons_measures,
  by = "practice_id"
)

out_data <- merged_out_measures

#Check for duplicate denominator vars - drop the duplicates, highlight any that are unique
denom_main_vars <- grep("denom_.*main", names(out_data), value = TRUE)
if (length(denom_main_vars) > 0) {
  out_data <- drop_all_duplicates(
    out_data,
    df_name = "out_data",
    denom_main_vars,
    new_name = "denom_main"
  )
}
#Now check for duplicate denominator vars within each subgroup

subgroups <- c("asth", "copd", "htn", "diab", "sevmh")

for (sg in subgroups) {
  pattern <- paste0("denom_.*sub_", sg)
  denom_sub_vars <- grep(pattern, names(out_data), value = TRUE)

  if (length(denom_sub_vars) > 0) {
    out_data <- drop_all_duplicates(
      out_data,
      df_name = "out_data",
      var_list = denom_sub_vars,
      new_name = paste0("denom_sub_", sg)
    )
  }
}

#Restrict out_data to only the practices in exp_data
out_data <- out_data %>%
  semi_join(exp_data, by = "practice_id")

#Merging the exposure and outcome data together to create the final analytic dataset
analytic_data_long <- left_join(
  out_data,
  exp_data,
  by = "practice_id"
) %>% #Merging the exp data to the longitudinal outcomes
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

#EXPORTING MERGED DATASET
print("Saving merged datasets")

# Long dataset
output_path_long <- paste0(dataclean_dir, "merged_data_long_", cohort, ".csv")
data.table::fwrite(analytic_data_long, here::here(output_path_long))
message(paste0("Long-format merged dataset saved to ", output_path_long))

# Wide dataset
# output_path_wide <- paste0(dataclean_dir, "merged_data_wide_", cohort, ".csv")
# data.table::fwrite(analytic_data_wide, here::here(output_path_wide))
# message(paste0("Wide-format merged dataset saved to ", output_path_wide))

#TO DO:
#Get the positive_var_check function to output a nice dataset (like date_check_long does)
#Maybe: Check that each denom variable is the same, and then drop them (because they should just be the practice list size)
#Only issue is if some patients have missing data on key characteristics.
#Add the number of registered patients used to calculate each proportion variable
#CHECK that this number is consistent within each dataset, and for each category variable
#Create the CMS

#FROM LP:
#I've been thinking about this indicator and what it means.
# I think it is largely an indicator of supply of appts, driven by practice (rather than patient demand).
# However it is not clear cut and could also be somewhat demand driven.
# Thinking about how to better understand the 'demand' element, I wonder if there may be variation between a practice that provides a lot of appts to a small proportion of their population (ie to the high demand and high need cohorts) Vs those who may provide the same average number of appointments but with a more even spread e.g a university practice.  I wonder if how appointments are distributed/managed across the registered patient list therefore may have a knock on effect in secondary care due to unmet demand from patients with different case-mixes. Would it be feasible to develop an indicator that would capture this e.g. the range of number of consultations/pt, the IQR, the SD?

##Code to clear items from memory, and free unused memory
#rm(list = ls())
#gc()
