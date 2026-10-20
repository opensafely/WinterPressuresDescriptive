# First function to collapse data to practice level

aggregate_patient <- function(input) {
    # Check for practices with multiple regions ------------------------------------
    region_check <- input %>%
        filter(!is.na(practice_id), !is.na(exp_cat_region)) %>%
        group_by(practice_id) %>%
        summarise(
            n_regions = n_distinct(exp_cat_region[!is.na(exp_cat_region)])
        ) %>%
        filter(n_regions > 1)

    if (nrow(region_check) > 0) {
        warning(paste(
            "Some practices have multiple regions. Using the first non-missing region for each practice. Number of affected practices:",
            nrow(region_check)
        ))
    }

    # Create practice-level summary dataset ----------------------------------------
    print("Create practice-level summary dataset")
    practice_summary <- input %>%
        filter(!is.na(patient_id), !is.na(practice_id)) %>%
        group_by(practice_id) %>%
        # Get the first non-missing region for each practice
        mutate(
            exp_cat_region = exp_cat_region[which(!is.na(exp_cat_region))[1]]
        ) %>%
        summarise(
            exp_cat_region = first(exp_cat_region),
            exp_cat_rurality = get_majority_category(exp_cat_rurality),
            exp_num_listsize = n(), # patient count per practice
            across(
                starts_with("exp_bin_"),
                ~ sum(.x, na.rm = TRUE),
                .names = "{.col}"
            ),
            across(
                contains("_cms"),
                ~ mean_cms_20plus(.x, exp_num_age),   # Mean CMS among patients aged >=20 at cohort start
                .names = "{.col}_mean"
            ),
            .groups = "drop"
        ) %>%
        # rename exp_bin_* to exp_num_*
        rename_with(~ sub("^exp_bin_", "exp_num_", .x), starts_with("exp_bin_"))
    message(paste0(
        "Practice-level summary dataset has N = ",
        nrow(practice_summary),
        " rows"
    ))

    # NOTE:
    # For now we only keep practice_id and exp_cat_region for merging into
    # the measures tables. If we later want to include list size (exp_num_listsize)
    # or the exp_num_* / *_cms variables in the analytic dataset,
    # edit the select() line below to keep additional columns.
    practice_summary <- practice_summary %>%
        select(practice_id, exp_cat_region, exp_cat_rurality, exp_num_cms_mean) %>%
        rename(
            practice_region = exp_cat_region,
            practice_rurality = exp_cat_rurality,
            cms_mean = exp_num_cms_mean
        )

    # Add mean CMS for each patient subgroup ------------------------------------
    subgroup_flags <- c(
        sub_asth = "sub_bin_asthma",
        sub_copd = "sub_bin_copd",
        sub_htn = "sub_bin_hypertension",
        sub_diab = "sub_bin_diabetes",
        sub_sevmh = "sub_bin_sev_mental_ill"
    )

    for (subgroup in names(subgroup_flags)) {
        flag <- unname(subgroup_flags[[subgroup]])
        output_var <- paste0("cms_mean_", subgroup)

        subgroup_summary <- input %>%
            filter(
                !is.na(patient_id),
                !is.na(practice_id),
                .data[[flag]] == TRUE
            ) %>%
            group_by(practice_id) %>%
            summarise(
                !!output_var := mean_cms_20plus(         # Mean CMS among patients aged >=20 at cohort start
                    exp_num_cms,
                    exp_num_age
                ),
                .groups = "drop"
            )

        practice_summary <- practice_summary %>%
            left_join(subgroup_summary, by = "practice_id")
    }

    return(practice_summary)
}
