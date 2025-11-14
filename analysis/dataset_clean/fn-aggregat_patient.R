# First function to collapse data to practice level

aggregat <- function(input) {
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
    print('Create practice-level summary dataset')
    practice_summary <- input %>%
        filter(!is.na(patient_id), !is.na(practice_id)) %>%
        group_by(practice_id) %>%
        # Get the first non-missing region for each practice
        mutate(
            exp_cat_region = exp_cat_region[which(!is.na(exp_cat_region))[1]]
        ) %>%
        summarise(
            exp_cat_region = first(exp_cat_region),
            exp_num_listsize = n(), # patient count per practice
            across(
                starts_with("exp_bin_"),
                ~ sum(.x, na.rm = TRUE),
                .names = "{.col}"
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
    return(practice_summary)
}
