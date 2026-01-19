# Function to apply redaction ----------------------------------------
redact <- function(input) {
    message(
        "Applying rounding + generating _mp6 proportion variables using roundmid_num() specified in the utility.R"
    )
    # Apply rounding to list size ------------------------------------------------------------------------
    print("Rounding list_size to midpoint rounding to nearest 6")
    input <- input %>%
        mutate(
            list_size_mp6 = roundmid_num(list_size, to = 6)
        )
    # Check summary
    message("Summary of rounded list_size:")
    summary(input$list_size)
    summary(input$list_size_mp6)

    # Identify numerator/denominator variables ----------------------------------------
    # numerator variables
    num_vars <- names(input)[
        grepl("^num_", names(input)) &
            !grepl("_mean$|_cumu$|_mp6$", names(input))
    ]
    # denominator variables
    denom_vars <- names(input)[
        grepl("^denom_", names(input)) &
            !grepl("_mean$|_cumu$|_mp6$", names(input))
    ]

    # Apply rounding to numerators and denominators, create _mp6 variables ----------------------------------------
    input <- input %>%
        mutate(
            across(
                all_of(c(num_vars, denom_vars)),
                ~ roundmid_num(.x, to = 6),
                .names = "{.col}_mp6"
            )
        )
    # Create proportion variables based on rounded numerators/denominators ----------------------------------------
    # For each rounded numerator, find its matching denominator
    num_mp6_vars <- names(input)[grepl("^num_.*_mp6$", names(input))]

    missing_denom_list <- c() # track missing denominators

    for (num_mp6_var in num_mp6_vars) {
        # infer denominator name
        denom_mp6_var <- sub("^num_", "denom_", num_mp6_var)
        prop_mp6_var <- sub("^num_", "prop_", num_mp6_var)

        # CASE 1: denominator exists → use it
        if (denom_mp6_var %in% names(input)) {
            input[[prop_mp6_var]] <- ifelse(
                input[[denom_mp6_var]] > 0,
                input[[num_mp6_var]] / input[[denom_mp6_var]],
                NA_real_
            )
        } else {
            # CASE 2: denominator missing → use list_size_mp6
            missing_denom_list <- c(missing_denom_list, denom_mp6_var)

            input[[prop_mp6_var]] <- ifelse(
                input$list_size_mp6 > 0,
                input[[num_mp6_var]] / input$list_size_mp6,
                NA_real_
            )
        }
    }
    # Check summaries
    message("Summary of rounded numerator and denominator variables:")
    summary(input[num_vars])
    summary(input[num_mp6_vars])

    # Check proportions are valid
    range(
        unlist(
            input[grep("^prop_.*_mp6$", names(input))]
        ),
        na.rm = TRUE
    )

    # Apply rounding to mean consultation ----------------------------
    print(
        "Compute mean consultation proportions per practice (midepoint6 rounded)"
    )

    cons_cols_mp6 <- grep("^prop_cons_.*_mp6$", names(input), value = TRUE)

    if (length(cons_cols_mp6) > 0) {
        input <- input %>%
            rowwise() %>%
            mutate(
                prop_cons_mean_mp6 = mean(
                    c_across(all_of(cons_cols_mp6)),
                    na.rm = TRUE
                )
            ) %>%
            ungroup()
        message(
            "Added prop_cons_mean_mp6 (average across prop_cons_M_mp6 columns)."
        )
    } else {
        warning(
            "No rounded consultation proportion columns found (prop_cons_M_mp6...)."
        )
    }

    # Compute mean weekly rate for outcomes --------------------------------------
    print("Compute mean weekly rates per practice (midepoint6 rounded)")

    outcome_vars_mp6 <- grep(
        "^prop_(apc|ec)_.*mp6$",
        names(input),
        value = TRUE
    )

    if (length(outcome_vars_mp6) == 0) {
        message("No rounded proportion variables (prop_*_mp6) found.")
    } else {
        input <- input %>%
            group_by(practice_id) %>%
            mutate(
                across(
                    all_of(outcome_vars_mp6),
                    ~ mean(.x, na.rm = TRUE),
                    .names = "{sub('_mp6$', '', .col)}_mean_mp6"
                    # e.g. prop_apc_acsc_any_main_mp6 → prop_apc_acsc_any_main_mean_mp6
                )
            ) %>%
            ungroup()

        message(
            "Added rounded mean weekly rate variables (mean_mp6) for: ",
            paste(outcome_vars_mp6, collapse = ", ")
        )
    }

    # Compute cumulative rates ----------------------------------------------------
    print("Compute cumulative rates (mp6) per practice")

    num_vars_raw <- grep(
        "^num_(apc|ec)_",
        names(input),
        value = TRUE
    )
    num_vars_raw <- num_vars_raw[!grepl("_mp6$|_mean$|_cumu$", num_vars_raw)]

    if (length(num_vars_raw) == 0) {
        message("No raw numeric outcome variables (num_apc_*, num_ec_*) found.")
    } else {
        input <- input %>%
            group_by(practice_id) %>%
            mutate(
                across(
                    all_of(num_vars_raw),
                    ~ {
                        cum_raw <- sum(.x, na.rm = TRUE) # sum of RAW weekly counts
                        cum_round <- roundmid_num(cum_raw, to = 6) # midpoint-6 rounding once
                        cum_round / first(list_size_mp6) # divide by rounded list size
                    },
                    .names = "{sub('^num_', 'prop_', .col)}_cumu_mp6"
                )
            ) %>%
            ungroup()

        message(
            "Added rounded cumulative outcome variables (cumu_mp6) for RAW weekly vars: ",
            paste(num_vars_raw, collapse = ", ")
        )
    }

    message(
        "Finished generating rounded list size, numerators, denominators, proportions, and summary variables for consultation and outcomes."
    )
    return(input)
}
