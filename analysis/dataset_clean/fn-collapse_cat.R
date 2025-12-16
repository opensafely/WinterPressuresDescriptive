# Function to collapse categorical variables ----------------------------------------
collapse_categories <- function(input) {
    message("Collapse categorical variables (age 80+, urban combined)")

    # Variables to remove
    vars_to_remove <- c(grep(
        "80_84|85|urban_major|urban_minor",
        names(input),
        value = TRUE
    ))

    # Collapse age 80–84 + 85+
    input <- input %>%
        mutate(
            num_age_80 = num_age_80_84 + num_age_85,
            prop_age_80 = num_age_80 / list_size
        )

    # Collapse urban major + minor
    input <- input %>%
        mutate(
            num_rurality_urban_comb = num_rurality_urban_major +
                num_rurality_urban_minor,
            prop_rurality_urban_comb = num_rurality_urban_comb / list_size
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
