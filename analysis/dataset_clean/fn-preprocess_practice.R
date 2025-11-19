preprocess_measure <- function(cohort) {
  # Get column names ----
  print('Get column names')
  file_path <- paste0("output/dataset_clean/merged_data_wide_", cohort, ".csv")
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
  print('Define column classes')

  cat_cols <- c(grep("_cat", all_cols, value = TRUE))
  bin_cols <- c(grep("_bin", all_cols, value = TRUE))
  num_cols <- c(
    grep("_num", all_cols, value = TRUE),
    grep("_denom", all_cols, value = TRUE),
    grep("_prop_", all_cols, value = TRUE)
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
  print('Load cohort dataset')

  input <- read_csv(file_path, col_types = col_classes)
  message(paste0(
    "Dataset has been read successfully with N = ",
    nrow(input),
    " rows"
  ))

  # Format dataset columns ----
  print('Format dataset columns')

  input <- input %>%
    mutate(
      across(
        all_of(date_cols),
        ~ floor_date(as.Date(., format = "%Y-%m-%d"), unit = "days")
      ),
      across(contains('_birth_year'), ~ as.numeric(.)), #~ year(as.Date(., origin = "1970-01-01"))),
      across(all_of(num_cols), ~ as.numeric(.)),
      across(all_of(cat_cols), ~ as.character(.))
    ) %>%
    rename(
      practice_id = practice_pseudo_id, # consistent ID
      exp_denom_total = exp_denom # shared denominator
    )

  # Compute mean consultation proportion ----------------------------
  print("Compute mean consultation proportions per practice")

  cons_cols <- grep("^exp_prop_cons_", names(input), value = TRUE)

  if (length(cons_cols) > 0) {
    input <- input %>%
      rowwise() %>%
      mutate(
        exp_prop_cons_mean = mean(c_across(all_of(cons_cols)), na.rm = TRUE)
      ) %>%
      ungroup()
    message(
      "Added exp_prop_cons_mean (average across exp_prop_cons_YYYYMM columns)."
    )
  } else {
    warning("No consultation proportion columns found (exp_prop_cons_...).")
  }

  # Compute mean weekly rate for outcomes --------------------------------------
  print("Compute mean weekly rates per practice")

  out_prop_prefixes <- c(
    "out_prop_apc_",
    "out_prop_ec_",
    "out_prop_acscs_copd_apc_",
    "out_prop_acscs_copd_ec_",
    "out_prop_acscs_asthma_apc_",
    "out_prop_acscs_asthma_ec_",
    "out_prop_acscs_hypt_apc_",
    "out_prop_acscs_hypt_ec_",
    "out_prop_acscs_diabetes_apc_",
    "out_prop_acscs_diabetes_ec_",
    "out_prop_acscs_angina_apc_",
    "out_prop_acscs_angina_ec_"
  )

  for (p in out_prop_prefixes) {
    # All columns matching the prefix + 1–20
    cols <- grep(paste0("^", p, "w[0-9]+$"), names(input), value = TRUE)

    if (length(cols) > 0) {
      mean_name <- paste0(p, "mean")

      input <- input %>%
        rowwise() %>%
        mutate(!!mean_name := mean(c_across(all_of(cols)), na.rm = TRUE)) %>%
        ungroup()

      message(
        "Added ",
        mean_name,
        " (mean across: ",
        paste(cols, collapse = ", "),
        ")"
      )
    } else {
      message("No columns found for prefix: ", p)
    }
  }

  # Compute cumulative rates ----------------------------------------------------
  print("Compute cumulative rates per practice")

  # prefixes for cumulative calculations using _num_ variables
  out_num_prefixes <- c(
    "out_num_apc_",
    "out_num_ec_",
    "out_num_acscs_copd_apc_",
    "out_num_acscs_copd_ec_",
    "out_num_acscs_asthma_apc_",
    "out_num_acscs_asthma_ec_",
    "out_num_acscs_hypt_apc_",
    "out_num_acscs_hypt_ec_",
    "out_num_acscs_diabetes_apc_",
    "out_num_acscs_diabetes_ec_",
    "out_num_acscs_angina_apc_",
    "out_num_acscs_angina_ec_"
  )

  for (p in out_num_prefixes) {
    # weekly number columns: out_num_apc_w1 ... w20
    cols <- grep(paste0("^", p, "w[0-9]+$"), names(input), value = TRUE)

    if (length(cols) > 0) {
      # create a clean cumulative rate variable name
      prop_prefix <- gsub("num", "prop", p)
      cumulative_rate_name <- paste0(prop_prefix, "total")

      input <- input %>%
        rowwise() %>%
        mutate(
          !!cumulative_rate_name := sum(c_across(all_of(cols)), na.rm = TRUE) /
            exp_denom_total
        ) %>%
        ungroup()

      message(
        "Added ",
        cumulative_rate_name,
        ": sum(",
        paste(cols, collapse = ", "),
        ") / exp_denom_total"
      )
    } else {
      message("No numeric weekly columns found for: ", p)
    }
  }

  # Generate rounded proportions for descriptive tables -----------------------------
  print("Generate rounded proportion variables for descriptive tables")
  prop_cols <- grep("_prop_", names(input), value = TRUE)

  # Return the reprocessed practice-level data
  return(input)
}
