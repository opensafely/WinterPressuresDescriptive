# First function to preprocess patient-level dataset

preprocess_patients <- function(cohort) {
  # Get column names ----
  print('Get column names')

  file_path <- paste0("output/dataset_definition/input_", cohort, ".csv.gz")
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

  cat_cols <- c("patient_id", grep("_cat", all_cols, value = TRUE))
  bin_cols <- c(grep("_bin", all_cols, value = TRUE))
  num_cols <- c(
    grep("_num", all_cols, value = TRUE),
    grep("vax_jcvi_age_", all_cols, value = TRUE)
  )
  date_cols <- grep("_date", all_cols, value = TRUE)
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
    )
  # Apply includsion criteria ----
  print('Remove records with missing patient id or practice id')

  input <- input[!is.na(input$patient_id) & !is.na(input$practice_id), ]

  message("All records with valid patient and practice IDs retained.")

  print("Inclusion criteria: Alive at index")

  input <- subset(input, inex_bin_alive == TRUE)

  message("All records alive at index.")

  print("Inclusion criteria: registered with a practice at index")

  input <- subset(input, inex_bin_reg_cs == TRUE)

  message("All records registered with a practice at index")

  # Restrict columns ----
  print('Restrict columns')

  input <- input %>%
    select(
      patient_id,
      practice_id,
      starts_with("index_date"),
      starts_with("exp_"), # Exposures
      starts_with("inex_"), # Inclusion/exclusion
      starts_with("cens_"), # Censor
      starts_with("vax_date_"), # Vaccination dates and vax type
      starts_with("vax_cat_"), # Vaccination products
      starts_with("vax_bin_") # Vaccination binary flags
    )
  # Return data ----
  print('Return data')

  return(input)
}
