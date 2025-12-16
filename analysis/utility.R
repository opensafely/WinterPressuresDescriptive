# Rounding function for redaction ----
# Rounding for counts
roundmid_num <- function(x, to = 6) {
  # centers on (integer) midpoint of the rounding points
  x <- as.numeric(x)
  ceiling(x / to) * to - (floor(to / 2) * (x != 0))
}

# Rounding for proportions
roundmid_prop <- function(x) {
  x <- as.numeric(x)
  m <- mean(x, na.rm = TRUE)
  
  # If very rare (<1%), round to 0.0006 (0.06%)
  if (m < 0.01) {
    to <- 0.0006
  } else {
    # Otherwise round to 0.006 (0.6%)
    to <- 0.006
  }
  y <- ceiling(x / to) * to - (floor(to / 2) * (x != 0))
  
  # clamp values to [0,1]
  y <- pmin(pmax(y, 0), 1)
  
  return(y)
}

# Function to make display numbers ----

display <- function(x) {
  ifelse(
    x >= 100,
    sprintf("%.0f", x),
    ifelse(x >= 10, sprintf("%.1f", x), sprintf("%.2f", x))
  )
}

# Function for describing data ----

describe_data <- function(df, name) {
  fs::dir_create(here::here("output/describe/"))
  sink(paste0("output/describe/", name, ".txt"))
  print(Hmisc::describe(df))
  sink()
  message(paste0("output/describe/", name, ".txt written successfully."))
}

# Function for creating a median (iqr_low-iqr_high) string ----

create_median_iqr_string <- function(x)
  return(paste0(
    quantile(x)[3],
    " (",
    quantile(x)[2],
    "-",
    quantile(x)[4],
    ")"
  ))

# Function to convert numerical data to categorical data, following chosen bounds

numerical_to_categorical <- function(
  x,
  bounds = c(1, 100),
  zero_flag = FALSE,
  lower_limit = FALSE,
  upper_limit = FALSE,
  inclusive_bounds = FALSE
) {
  # x <- the numeric input vector
  # bounds <- a vector of bounds (must be ordered low->high)
  # zero_flag <- if TRUE, include an additional category for zero-values
  # lower_limit <- if TRUE, the first value in bounds is a hard lower bound
  #                if FALSE, create a category between 0 and the first value
  # upper_limit <- if TRUE, the last value in bounds is a hard upper bound
  #                if FALSE, create a category for greater than the last value
  # inclusive_bounds <- whether the bounds are inclusive or exclusive (assuming discrete values)
  #                     N.B. will assign borderline cases to upper boundary
  N <- length(bounds)
  gap <- ifelse(inclusive_bounds, 0, 1)
  y <- x

  if (!lower_limit) {
    y <- ifelse(x <= bounds[1] - gap, sprintf("<=%d", bounds[1] - gap), y)
  }
  if (zero_flag) {
    y <- ifelse(x == 0, sprintf("0"), y)
  }
  for (i1 in 1:(N - 1)) {
    y <- ifelse(
      x >= bounds[i1] & x <= bounds[i1 + 1] - gap,
      sprintf("%d-%d", bounds[i1], bounds[i1 + 1] - gap),
      y
    )
  }
  if (!upper_limit) {
    y <- ifelse(x >= bounds[N], sprintf("%d+", bounds[N]), y)
  }
  return(y)
}


#DEFINING FUNCTIONS [used in the analysis/dataset_clean]
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