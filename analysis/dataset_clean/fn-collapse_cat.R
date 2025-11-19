# Function to collapse categorical variables ----------------------------------------
collapse_categories <- function(input) {
    message("Collapse categorical variables (age 80+, urban combined)")

    # Variables to remove
    vars_to_remove <- c(
        "exp_num_80_to_84",
        "exp_num_age_85_plus",
        "exp_prop_80_to_84",
        "exp_prop_age_85_plus",
        "exp_num_urb_major",
        "exp_num_urb_minor",
        "exp_prop_urb_major",
        "exp_prop_urb_minor"
    )

    # Collapse age 80–84 + 85+
    input <- input %>%
        mutate(
            exp_num_age_80plus = exp_num_80_to_84 + exp_num_age_85_plus,
            exp_prop_age_80plus = exp_num_age_80plus / exp_denom_total
        )

    # Collapse urban major + minor
    input <- input %>%
        mutate(
            exp_num_urb_combined = exp_num_urb_major + exp_num_urb_minor,
            exp_prop_urb_combined = exp_num_urb_combined / exp_denom_total
        )

    # Determine which variables exist
    removed_vars <- vars_to_remove[vars_to_remove %in% names(input)]

    # Remove them
    input <- input %>% select(-any_of(removed_vars))

    # Report
    message(
        "Removed component variables: ",
        paste(removed_vars, collapse = ", ")
    )

    return(input)
}
