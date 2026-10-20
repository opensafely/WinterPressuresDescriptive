# Function to apply redaction ----------------------------------------
redact <- function(input) {
    message(
        "Applying rounding + generating _mp6 proportion variables using roundmid_num() specified in the utility.R"
    )
    # Round overall and subgroup baseline population sizes
    list_size_vars <- grep(
        "^list_size(_sub_(asth|copd|htn|diab|sevmh))?$",
        names(input),
        value = TRUE
    )

    input <- input %>%
        mutate(
            across(
                all_of(list_size_vars),
                ~ roundmid_num(.x, to = 6),
                .names = "{.col}_mp6"
            )
        )

    # Check original and rounded list sizes
    for (var in list_size_vars) {
        message("Summary of ", var, ":")
        print(summary(input[[var]]))

        rounded_var <- paste0(var, "_mp6")
        message("Summary of ", rounded_var, ":")
        print(summary(input[[rounded_var]]))
    }

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

    missing_denom_list <- character(0)

    # Identify cross-sectional subgroup characteristic counts only.
    # Hospital outcome counts retain their existing denominator logic.
    cs_subgroup_pattern <- paste0(
        "^num_(age|sex|ethnicity|imd|rurality|carehome|smoking|obesity|cms)",
        "(_.*)?_sub_(asth|copd|htn|diab|sevmh)_mp6$"
    )

    for (num_mp6_var in num_mp6_vars) {
        denom_mp6_var <- sub("^num_", "denom_", num_mp6_var)
        prop_mp6_var <- sub("^num_", "prop_", num_mp6_var)

        # Consultations must use their corresponding monthly denominator.
        if (
            grepl("^num_cons_", num_mp6_var) &&
                !denom_mp6_var %in% names(input)
        ) {
            stop(
                "Missing monthly consultation denominator: ",
                denom_mp6_var
            )
        }

        if (denom_mp6_var %in% names(input)) {
            # Prefer the retained measure-specific denominator
            denominator_var <- denom_mp6_var
        } else if (grepl(cs_subgroup_pattern, num_mp6_var)) {
            # Otherwise use the subgroup baseline list size
            subgroup <- sub(
                "^.*_(sub_(asth|copd|htn|diab|sevmh))_mp6$",
                "\\1",
                num_mp6_var
            )

            denominator_var <- paste0(
                "list_size_", subgroup, "_mp6"
            )

            if (!denominator_var %in% names(input)) {
                stop(
                    "Missing subgroup denominator: ",
                    denominator_var
                )
            }
        } else {
            denominator_var <- "list_size_mp6"

            missing_denom_list <- c(
                missing_denom_list,
                denom_mp6_var
            )
        }

        input[[prop_mp6_var]] <- ifelse(
            input[[denominator_var]] > 0,
            input[[num_mp6_var]] / input[[denominator_var]],
            NA_real_
        )
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

    # Compute mean monthly consultation rates from rounded counts --------------
    print(
        "Compute mean monthly consultation rates per practice and patient group (midpoint-6 rounded)"
    )

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
        # Select the 12 rounded monthly rates for this population only.
        expected_cols <- paste0(
            "prop_cons_",
            consultation_months,
            suffix,
            "_mp6"
        )

        cons_cols_mp6 <- intersect(expected_cols, names(input))
        missing_cols <- setdiff(expected_cols, names(input))

        output_var <- paste0(
            "prop_cons_mean",
            suffix,
            "_mp6"
        )

        if (length(missing_cols) > 0L) {
            warning(
                "Missing rounded monthly consultation columns for ",
                output_var, ": ",
                paste(missing_cols, collapse = ", ")
            )
        }

        if (length(cons_cols_mp6) == 0L) {
            input[[output_var]] <- NA_real_
            next
        }

        # Average available monthly rates, consistent with preprocess_measure().
        mean_values <- rowMeans(
            input[, cons_cols_mp6, drop = FALSE],
            na.rm = TRUE
        )

        # All months missing: return NA rather than NaN.
        mean_values[is.nan(mean_values)] <- NA_real_

        input[[output_var]] <- mean_values

        message(
            "Added ", output_var,
            " (average across ", length(cons_cols_mp6),
            " rounded monthly consultation columns)."
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
