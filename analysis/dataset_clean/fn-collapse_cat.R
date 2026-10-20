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

    # Collapse categories for the overall population and each subgroup ----------
    population_suffixes <- c(
        "",
        "_sub_asth",
        "_sub_copd",
        "_sub_htn",
        "_sub_diab",
        "_sub_sevmh"
    )

    for (suffix in population_suffixes) {
        denominator <- paste0("list_size", suffix)

        age_components <- paste0(
            c("num_age_80_84", "num_age_85"),
            suffix
        )

        rurality_components <- paste0(
            c("num_rurality_urban_major", "num_rurality_urban_minor"),
            suffix
        )

        required_vars <- c(
            denominator,
            age_components,
            rurality_components
        )

        missing_vars <- setdiff(required_vars, names(input))

        if (length(missing_vars) > 0L) {
            stop(
                "Missing variables for category collapsing: ",
                paste(missing_vars, collapse = ", ")
            )
        }

        # Collapse age 80–84 and 85+
        age_num <- paste0("num_age_80", suffix)
        age_prop <- paste0("prop_age_80", suffix)

        input[[age_num]] <-
            input[[age_components[1]]] +
            input[[age_components[2]]]

        input[[age_prop]] <- ifelse(
            input[[denominator]] > 0,
            input[[age_num]] / input[[denominator]],
            NA_real_
        )

        # Collapse urban major and minor
        rurality_num <- paste0("num_rurality_urban_comb", suffix)
        rurality_prop <- paste0("prop_rurality_urban_comb", suffix)

        input[[rurality_num]] <-
            input[[rurality_components[1]]] +
            input[[rurality_components[2]]]

        input[[rurality_prop]] <- ifelse(
            input[[denominator]] > 0,
            input[[rurality_num]] / input[[denominator]],
            NA_real_
        )
    }

    # Remove components only after creating all collapsed variables
    removed_vars <- grep(
        "^(num|prop|denom)_(age_80_84|age_85|rurality_urban_major|rurality_urban_minor)(|_sub_(asth|copd|htn|diab|sevmh))$",
        names(input),
        value = TRUE
    )

    input <- input %>%
        select(-all_of(removed_vars))

    message(
        "Removed component variables: ",
        paste(removed_vars, collapse = ", ")
    )

    return(input)
}
