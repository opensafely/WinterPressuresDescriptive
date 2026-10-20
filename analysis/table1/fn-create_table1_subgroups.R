# Generate Table 1 summaries for patient subgroups ------------------------------
create_table1_subgroups <- function(
  input,
  rounded_vars,
  unrounded_vars,
  rounded = FALSE,
  threshold,
  threshold_practice
) {
    # Define patient-group labels ---------------------------------------------
    subgroup_labels <- c(
        sub_asth = "sub_asthma",
        sub_copd = "sub_copd",
        sub_diab = "sub_diabetes",
        sub_htn = "sub_hypertension",
        sub_sevmh = "sub_sev_mental_ill"
    )

    subgroup_pattern <- "_sub_(asth|copd|htn|diab|sevmh)(_mp6)?$"

    # Select subgroup rates ---------------------------------------------------
    rate_vars <- if (rounded) {
        rounded_vars
    } else {
        unrounded_vars
    }

    rate_vars <- rate_vars[
        grepl(subgroup_pattern, rate_vars)
    ]

    # CMS means have no rounded versions; include them only in thresholded output.
    cms_vars <- if (!rounded) {
        grep(
            "^cms_mean_sub_(asth|copd|htn|diab|sevmh)$",
            names(input),
            value = TRUE
        )
    } else {
        character(0)
    }

    rate_vars <- unique(c(rate_vars, cms_vars))

    # Use rounded population sizes in both outputs,
    # consistent with the original create_table1().
    list_size_vars <- grep(
        "^list_size_sub_(asth|copd|htn|diab|sevmh)_mp6$",
        names(input),
        value = TRUE
    )

    summary_vars <- unique(c(list_size_vars, rate_vars))

    if (length(summary_vars) == 0L) {
        stop("No subgroup variables found for Table 1.")
    }

    # Restrict to one row per practice ----------------------------------------
    input_table1 <- input %>%
        select(
            practice_id,
            all_of(summary_vars)
        ) %>%
        distinct(practice_id, .keep_all = TRUE)

    # Reshape and identify patient group --------------------------------------
    table1_long <- input_table1 %>%
        pivot_longer(
            cols = all_of(summary_vars),
            names_to = "variable",
            values_to = "value"
        ) %>%
        mutate(
            # Remove rounding suffix only where present.
            variable = str_remove(variable, "_mp6$"),
            patient_group = str_extract(
                variable,
                "sub_(asth|copd|htn|diab|sevmh)$"
            ),

            # Remove patient-group suffix before splitting the characteristic.
            variable = str_remove(
                variable,
                "_sub_(asth|copd|htn|diab|sevmh)$"
            ),
            characteristic = str_extract(
                variable,
                "^[^_]+"
            ),
            subcharacteristic = if_else(
                str_detect(variable, "_"),
                str_remove(variable, "^[^_]+_"),
                "True"
            )
        )

    if (anyNA(table1_long$patient_group)) {
        stop("Unrecognised patient-group suffix in Table 1 variables.")
    }

    # Summarise each patient group across practices ----------------------------
    # No additional region, rurality or practice strata.
    table1_summary <- table1_long %>%
        group_by(
            patient_group,
            characteristic,
            subcharacteristic
        ) %>%
        summarise(
            summarise_dist(
                value,
                is_outcome = FALSE,
                include_tail_percentiles = TRUE
            ),
            .groups = "drop"
        ) %>%
        mutate(
            n_practices_midpoint6 = roundmid_num(
                n_practices,
                to = threshold
            )
        )

    # Apply the same output rules as create_table1() ---------------------------
    if (!rounded) {
        table1_summary <- table1_summary %>%
            filter(n_practices > threshold_practice) %>%
            select(-n_practices)
    } else {
        table1_summary <- table1_summary %>%
            select(-n_practices) %>%
            rename_with(
                ~ ifelse(
                    grepl("_midpoint6$", .x),
                    .x,
                    paste0(.x, "_midpoint6")
                ),
                where(is.numeric)
            )
    }

    # Order patient groups and use their names as strata -----------------------
    table1_summary <- table1_summary %>%
        mutate(
            patient_group = factor(
                patient_group,
                levels = names(subgroup_labels)
            )
        ) %>%
        arrange(
            patient_group,
            characteristic,
            subcharacteristic
        ) %>%
        mutate(
            strata = unname(
                subgroup_labels[as.character(patient_group)]
            )
        ) %>%
        select(-patient_group) %>%
        relocate(
            characteristic,
            subcharacteristic,
            strata
        )

    return(table1_summary)
}
