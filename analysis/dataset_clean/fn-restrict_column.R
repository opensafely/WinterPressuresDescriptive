# Function to restrict to unrounded variables ----------------------------------------
restrict_column <- function(input) {
    message("Restricting to relevant variables only")

    # ----------------------------------------------------------------------
    # 1. Remove duplicate denominators for outcomes (main and subgroups)
    # ----------------------------------------------------------------------

    #Check for duplicate denominator vars for outcomes - drop the duplicates, highlight any that are unique

    denom_main_vars_mp6 <- grep(
        "^denom_.*main.*_mp6$",
        names(input),
        value = TRUE
    )
    denom_main_vars <- names(input)[
        grepl("^denom_.*main", names(input)) &
            !grepl("_mp6$", names(input))
    ]

    if (length(denom_main_vars_mp6) > 0) {
        input <- drop_all_duplicates(
            input,
            df_name = "input",
            denom_main_vars_mp6,
            new_name = "denom_main_mp6"
        )
    }

    if (length(denom_main_vars) > 0) {
        input <- drop_all_duplicates(
            input,
            df_name = "input",
            denom_main_vars,
            new_name = "denom_main"
        )
    }

    #Check for duplicate denominator vars within each subgroup

    subgroups <- c("asth", "copd", "htn", "diab", "sevmh")

    for (sg in subgroups) {
        pattern_mp6 <- paste0("^denom_.*sub_", sg, ".*_mp6$")
        denom_sub_vars_mp6 <- grep(pattern_mp6, names(input), value = TRUE)
        denom_sub_vars <- names(input)[
            grepl(paste0("^denom_.*sub_", sg), names(input)) &
                !grepl("_mp6$", names(input))
        ]

        if (length(denom_sub_vars_mp6) > 0) {
            input <- drop_all_duplicates(
                input,
                df_name = "input",
                var_list = denom_sub_vars_mp6,
                new_name = paste0("denom_sub_", sg, "_mp6")
            )
        }

        if (length(denom_sub_vars) > 0) {
            input <- drop_all_duplicates(
                input,
                df_name = "input",
                var_list = denom_sub_vars,
                new_name = paste0("denom_sub_", sg)
            )
        }
    }

    # ----------------------------------------------------------------------
    # 2. Remove all rounded numerator/denominator variables
    # ----------------------------------------------------------------------

    drop_rounded_vars <- names(input)[
        grepl("^(num_|denom_)", names(input)) & # any num_ or denom_
            grepl("_mp6$", names(input)) # ending in _mp6
    ]

    # Count how many will be removed
    n_drop <- length(drop_rounded_vars)

    message("--------------------------------------------------")
    message("Dropping mp6 numerator/denominator variables")
    message("Number of variables removed: ", n_drop)

    if (n_drop > 0) {
        message("Variables removed:")
        print(drop_rounded_vars)
    } else {
        message("No variables met criteria for removal.")
    }

    # Remove them from dataset
    input <- input %>%
        select(-all_of(drop_rounded_vars))

    return(input)
}
