process_measure_output <- function(cohort) {
    # Get column names ----
    print('Get column names')
    file_path <- paste0("output/analytic_data_wide_", cohort, ".csv")
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

    # Return the processed data
    return(input)
}
