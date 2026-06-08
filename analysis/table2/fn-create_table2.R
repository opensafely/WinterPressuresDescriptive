create_table2 <- function(
  input,
  rounded_vars,
  unrounded_vars,
  Strata = TRUE,
  rounded = FALSE,
  threshold,
  threshold_practice
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

    input_table2 <- input %>%
        select(
            practice_id,
            practice_region,
            all_of(rate_vars),
            all_of(strata_cols)
        )
    # Create long version of overall data ----
    print("Create long version of data")

    table2_long <- input_table2 %>%
        pivot_longer(
            cols = all_of(rate_vars),
            names_to = "outcome_name",
            values_to = "value"
        ) %>%
        rename(strata_region = practice_region) %>%
        mutate(
            strata_region = coalesce(strata_region, "Unknown")
        )

    # Summarise overall data ----
    print("Create summary of overall data")
    table2_summary <- table2_long %>%
        group_by(outcome_name) %>%
        summarise(summarise_dist(value, is_outcome = TRUE), .groups = "drop") %>%
        mutate(strata_region = "Overall") %>%
        bind_rows(
            table2_long %>%
                group_by(strata_region, outcome_name) %>%
                summarise(summarise_dist(value, is_outcome = TRUE), .groups = "drop")
        ) %>%
        mutate(n_practices_midpoint6 = roundmid_num(n_practices, to = threshold)) %>%
        rename(strata = strata_region) %>%
        mutate(
            # sample: main or sub_xxx
            group = str_extract(outcome_name, "(main|sub_[a-z]+)"),

            # source: apc / ec
            source = case_when(
                str_detect(outcome_name, "^apc_") ~ "apc",
                str_detect(outcome_name, "^ec_") ~ "ec",
                TRUE ~ NA_character_
            ),

            # APC plan status
            apc_plan_status = case_when(
                source != "apc" ~ NA_character_,
                str_detect(outcome_name, "^apc_plan_") ~ "planned",
                str_detect(outcome_name, "^apc_unpl_") ~ "unplanned",
                TRUE ~ "all"
            ),

            # ACSC flag
            acsc = str_detect(outcome_name, "_acsc_"),

            # ACSC condition
            acsc_condition = case_when(
                !acsc ~ NA_character_,
                TRUE ~ str_extract(
                    outcome_name,
                    "(?<=_acsc_)(copd|asth|htn|diab|ang|any)"
                )
            ),
            # statistic
            stat = str_extract(outcome_name, "mean|cumu"),
            # redact
            midpoint6 = case_when(
                grepl("mp6$", outcome_name) ~ TRUE,
                TRUE ~ FALSE
            )
        ) %>%
        relocate(
            group,
            source,
            apc_plan_status,
            acsc,
            acsc_condition,
            stat,
            midpoint6,
            .after = outcome_name
        )

    # Create long version of strata data ----
    print("Create long version of strata data")
    if (Strata) {
        # keep long version of strata
        practice_strata_long <- input_table2 %>%
            pivot_longer(
                cols = all_of(strata_cols),
                names_to = "strata",
                values_to = "in_stratum"
            ) %>%
            filter(in_stratum == 1) %>%
            select(-in_stratum)

        # create long version of data for strata
        table2_long_strata <- practice_strata_long %>%
            pivot_longer(
                cols = all_of(rate_vars),
                names_to = "outcome_name",
                values_to = "value"
            )

        # Summarise strata data ----
        print("Summarise strata data")
        table2_summary_strata <- table2_long_strata %>%
            group_by(strata, outcome_name) %>%
            summarise(summarise_dist(value, is_outcome = TRUE), .groups = "drop") %>%
            mutate(
                n_practices_midpoint6 = roundmid_num(n_practices, to = threshold)
            ) %>%
            mutate(
                # sample: main or sub_xxx
                group = str_extract(outcome_name, "(main|sub_[a-z]+)"),

                # source: apc / ec
                source = case_when(
                    str_detect(outcome_name, "^apc_") ~ "apc",
                    str_detect(outcome_name, "^ec_") ~ "ec",
                    TRUE ~ NA_character_
                ),

                # APC plan status
                apc_plan_status = case_when(
                    source != "apc" ~ NA_character_,
                    str_detect(outcome_name, "^apc_plan_") ~ "planned",
                    str_detect(outcome_name, "^apc_unpl_") ~ "unplanned",
                    TRUE ~ "all"
                ),

                # ACSC flag
                acsc = str_detect(outcome_name, "_acsc_"),

                # ACSC condition
                acsc_condition = case_when(
                    !acsc ~ NA_character_,
                    TRUE ~ str_extract(
                        outcome_name,
                        "(?<=_acsc_)(copd|asth|htn|diab|ang|any)"
                    )
                ),
                # statistic
                stat = str_extract(outcome_name, "mean|cumu"),
                # redact
                midpoint6 = case_when(
                    grepl("mp6$", outcome_name) ~ TRUE,
                    TRUE ~ FALSE
                )
            ) %>%
            relocate(
                group,
                source,
                apc_plan_status,
                acsc,
                acsc_condition,
                stat,
                midpoint6,
                .after = outcome_name
            )
        table2_summary_all <- bind_rows(table2_summary, table2_summary_strata)
    } else {
        table2_summary_all <- table2_summary
    }

    # Remove rows with insufficient practices if unrounded, and rename variables if rounded ---------------------------
    if (!rounded) {
        table2_summary_all <- table2_summary_all %>%
            filter(n_practices > threshold_practice) %>%
            select(-n_practices)
    } else {
        table2_summary_all <- table2_summary_all %>%
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

    return(table2_summary_all)
}
