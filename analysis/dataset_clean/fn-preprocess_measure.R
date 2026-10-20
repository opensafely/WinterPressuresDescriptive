preprocess_measure <- function(cohort) {
  # Get column names ----
  print("Get column names")
  file_path <- paste0("output/dataset_clean/merged_data_long_", cohort, ".csv.gz")
  all_cols <- fread(
    file_path,
    header = TRUE,
    sep = ",",
    nrows = 0,
    stringsAsFactors = FALSE
  ) %>%
    names()
  message("Column names found")
  print(all_cols)

  # Define column classes ----
  print("Define column classes")

  cat_cols <- c(grep("week_number", all_cols, value = TRUE))
  bin_cols <- c(grep("_bin", all_cols, value = TRUE))
  num_cols <- c(
    grep("list_size", all_cols, value = TRUE),
    grep("^num_", all_cols, value = TRUE),
    grep("^denom_", all_cols, value = TRUE),
    grep("^prop_", all_cols, value = TRUE)
  )
  date_cols <- c(
    grep("_start", all_cols, value = TRUE),
    grep("_end", all_cols, value = TRUE)
  )
  message("Column classes identified")

  col_classes <- setNames(
    c(
      rep("c", length(cat_cols)),
      rep("l", length(bin_cols)),
      rep("d", length(num_cols)),
      rep("D", length(date_cols))
    ),
    all_cols[match(c(cat_cols, bin_cols, num_cols, date_cols), all_cols)]
  )
  message("Column classes defined")

  # Load cohort dataset ----
  print("Load cohort dataset")

  input <- read_csv(file_path, col_types = col_classes)
  message(paste0(
    "Dataset has been read successfully with N = ",
    nrow(input),
    " rows"
  ))

  # Format dataset columns ----
  print("Format dataset columns")

  input <- input %>%
    mutate(
      across(
        all_of(date_cols),
        ~ floor_date(as.Date(., format = "%Y-%m-%d"), unit = "days")
      ),
      across(contains("_birth_year"), ~ as.numeric(.)), # ~ year(as.Date(., origin = "1970-01-01"))),
      across(all_of(num_cols), ~ as.numeric(.)),
      across(all_of(cat_cols), ~ as.character(.))
    )

  # Compute mean monthly consultation rates -----------------------------------
  print("Compute mean monthly consultation rates per practice and patient group")

  population_suffixes <- c(
    "",
    "_sub_asth",
    "_sub_copd",
    "_sub_htn",
    "_sub_diab",
    "_sub_sevmh"
  )

  consultation_months <- c(
    "oct", "nov", "dec", "jan", "feb", "mar",
    "apr", "may", "jun", "jul", "aug", "sep"
  )

  for (suffix in population_suffixes) {
    # Select the 12 monthly columns for this population only.
    expected_cols <- paste0(
      "prop_cons_",
      consultation_months,
      suffix
    )

    cons_cols <- intersect(expected_cols, names(input))
    missing_cols <- setdiff(expected_cols, names(input))

    output_var <- paste0("prop_cons_mean", suffix)

    if (length(missing_cols) > 0L) {
      warning(
        "Missing monthly consultation columns for ", output_var, ": ",
        paste(missing_cols, collapse = ", ")
      )
    }

    if (length(cons_cols) == 0L) {
      input[[output_var]] <- NA_real_
      next
    }

    # Preserve the existing approach: average available monthly rates.
    mean_values <- rowMeans(
      input[, cons_cols, drop = FALSE],
      na.rm = TRUE
    )

    # All months missing: return NA rather than NaN.
    mean_values[is.nan(mean_values)] <- NA_real_

    input[[output_var]] <- mean_values

    message(
      "Added ", output_var,
      " (average across ", length(cons_cols),
      " monthly consultation columns)."
    )
  }

  # Compute mean weekly rate for outcomes --------------------------------------
  print("Compute mean weekly rates per practice")

  outcome_vars <- grep("^prop_apc_|^prop_ec_", names(input), value = TRUE)

  if (length(outcome_vars) == 0) {
    message("No proportion variables (prop_apc_ / prop_ec_) found in input.")
  } else {
    input <- input %>%
      group_by(practice_id) %>%
      mutate(
        across(
          all_of(outcome_vars),
          ~ mean(.x, na.rm = TRUE),
          .names = "{.col}_mean" # e.g. prop_apc_acsc_any_main_mean
        )
      ) %>%
      ungroup()

    message(
      "Added mean weekly rate variables for: ",
      paste(outcome_vars, collapse = ", ")
    )
  }

  # Compute cumulative rates ----------------------------------------------------
  print("Compute cumulative rates per practice")

  # Numeric outcome count variables to sum over weeks
  num_vars <- grep("^num_apc_|^num_ec_", names(input), value = TRUE)

  if (length(num_vars) == 0) {
    message("No numeric outcome variables (num_apc_ / num_ec_) found in input.")
  } else {
    input <- input %>%
      group_by(practice_id) %>%
      mutate(
        across(
          all_of(num_vars),
          ~ sum(.x, na.rm = TRUE) / first(list_size),
          .names = "{sub('^num', 'prop', .col)}_cumu"
          # e.g. num_apc_acsc_any_main -> prop_apc_acsc_any_main_cumu
        )
      ) %>%
      ungroup()

    message(
      "Added cumulative outcome variables (per practice) for: ",
      paste(num_vars, collapse = ", "),
      " using denominator list_size."
    )
  }

  # Return the reprocessed practice-level data
  return(input)
}
