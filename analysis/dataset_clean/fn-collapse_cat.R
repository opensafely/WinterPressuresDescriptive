# Function to collapse categorical variables ----------------------------------------
collapse_categories <- function(input) {
    message("Collapse categorical variables (age 80+, practice rurality, and case-mix)")

    # Practice rurality
    input <- input %>%
        mutate(
            practice_rurality = case_when(
                practice_rurality %in% c("1", "2") ~ "Urban conurbation",
                practice_rurality %in% c("3", "4") ~ "Urban town",
                practice_rurality %in% c("5", "6", "7", "8") ~ "Rural",
                TRUE ~ NA_character_
            ),
            practice_rurality = factor(
                practice_rurality,
                levels = c(
                    "Urban conurbation",
                    "Urban town",
                    "Rural"
                )
            )
        )

    # Practice region
    region_levels <- c(
        "East",
        "East Midlands",
        "London",
        "North East",
        "North West",
        "South East",
        "South West",
        "West Midlands",
        "Yorkshire and The Humber"
    )
    if ("practice_region" %in% names(input)) {
        input$practice_region <- factor(
            input$practice_region,
            levels = region_levels
        )
    }

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
