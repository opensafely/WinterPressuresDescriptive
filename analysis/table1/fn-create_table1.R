create_table1 <- function(
    input,
    rounded_vars,
    unrounded_vars,
    Strata = TRUE,
    rounded = FALSE,
    threshold
) {
    name_pattern <- if (rounded) {
        "^([^_]+)(?:_(.*))?_mp6$"
    } else {
        "^([^_]+)(?:_(.*))?$"
    }

    rate_vars <- if (rounded) {
        rounded_vars
    } else {
        unrounded_vars
    }

    strata_cols <- if (Strata) {
        grep("^strata_", names(input), value = TRUE)
    } else {
        character(0)
    }

    input_table1 <- input %>%
        select(
            practice_id,
            practice_region,
            list_size_mp6,
            all_of(rate_vars),
            all_of(strata_cols)
        )

    # ---------------------------
    # Create long data
    # ---------------------------
    table1_long <- input_table1 %>%
        pivot_longer(
            cols = c(list_size_mp6, all_of(rate_vars)),
            names_to = c("characteristic", "subcharacteristic"),
            names_pattern = name_pattern,
            values_to = "value"
        ) %>%
        rename(strata_region = practice_region) %>%
        mutate(
            strata_region = coalesce(strata_region, "Unknown"),
            subcharacteristic = ifelse(
                is.na(subcharacteristic) | subcharacteristic == "",
                "True",
                subcharacteristic
            )
        )
    # ---------------------------
    # Overall summary
    # ---------------------------
    table1_summary <- table1_long %>%
        group_by(characteristic, subcharacteristic) %>%
        summarise(summarise_dist(value, is_outcome = FALSE), .groups = "drop") %>%
        mutate(strata = "Overall") %>%
        bind_rows(
            table1_long %>%
                group_by(strata_region, characteristic, subcharacteristic) %>%
                summarise(summarise_dist(value, is_outcome = FALSE), .groups = "drop") %>%
                rename(strata = strata_region)
        ) %>%
        mutate(
            across(
                matches("n_practices"),
                ~ roundmid_num(., to = threshold)
            )
        ) %>%
        rename(n_practices_midpoint6 = n_practices)

    # ---------------------------
    # Strata-specific summary
    # ---------------------------
    if (Strata) {
        practice_strata_long <- input_table1 %>%
            pivot_longer(
                cols = all_of(strata_cols),
                names_to = "strata",
                values_to = "in_stratum"
            ) %>%
            filter(in_stratum == 1) %>%
            select(-in_stratum)

        table1_long_strata <- practice_strata_long %>%
            pivot_longer(
                cols = c(list_size_mp6, all_of(rate_vars)),
                names_to = c("characteristic", "subcharacteristic"),
                names_pattern = name_pattern,
                values_to = "value"
            ) %>%
            mutate(
                subcharacteristic = ifelse(
                    is.na(subcharacteristic) | subcharacteristic == "",
                    "True",
                    subcharacteristic
                )
            )

        table1_summary_strata <- table1_long_strata %>%
            group_by(strata, characteristic, subcharacteristic) %>%
            summarise(summarise_dist(value, is_outcome = FALSE), .groups = "drop") %>%
            mutate(
                across(
                    matches("n_practices"),
                    ~ roundmid_num(., to = threshold)
                )
            ) %>%
            rename(n_practices_midpoint6 = n_practices)

        table1_summary_all <- bind_rows(table1_summary, table1_summary_strata)
    } else {
        table1_summary_all <- table1_summary
    }

    if (rounded) {
        table1_summary_all <- table1_summary_all %>%
            rename_with(
                ~ ifelse(
                    grepl("_midpoint6$", .x),
                    .x,
                    paste0(.x, "_midpoint6")
                ),
                where(is.numeric)
            )
    }

    return(table1_summary_all)
}
