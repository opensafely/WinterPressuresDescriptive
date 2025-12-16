# Function to restrict to relevant variables ----------------------------------------
restrict_variables <- function(input) {
    message("Restricting to relevant variables only")

    # ----------------------------------------------------------------------
    # 1. Remove all unrounded numerator/denominator variables
    # ----------------------------------------------------------------------

    drop_raw_num_denom <- names(input)[
        grepl("^(num_|denom_)", names(input)) & # any num_ or denom_
            !grepl("_mp6$", names(input)) # but NOT ending in _mp6
    ]

    # Count how many will be removed
    n_drop <- length(drop_raw_num_denom)

    message("--------------------------------------------------")
    message("Dropping NON-mp6 numerator/denominator variables")
    message("Number of variables removed: ", n_drop)

    if (n_drop > 0) {
        message("Variables removed:")
        print(drop_raw_num_denom)
    } else {
        message("No variables met criteria for removal.")
    }

    # Remove them from dataset
    input <- input %>%
        select(-all_of(drop_raw_num_denom))

    # ----------------------------------------------------------------------
    # 2. Remove duplicate denominators for outcomes (main and subgroups)
    # ----------------------------------------------------------------------

    #Check for duplicate denominator vars for outcomes - drop the duplicates, highlight any that are unique
    denom_main_vars <- grep("^denom_.*main.*_mp6$", names(input), value = TRUE)
    if (length(denom_main_vars) > 0) {
        input <- drop_all_duplicates(
            input,
            df_name = "input",
            denom_main_vars,
            new_name = "denom_main_mp6"
        )
    }
    #Check for duplicate denominator vars within each subgroup

    subgroups <- c("asth", "copd", "htn", "diab", "sevmh")

    for (sg in subgroups) {
        pattern <- paste0("^denom_.*sub_", sg, ".*_mp6$")
        denom_sub_vars <- grep(pattern, names(input), value = TRUE)

        if (length(denom_sub_vars) > 0) {
            input <- drop_all_duplicates(
                input,
                df_name = "input",
                var_list = denom_sub_vars,
                new_name = paste0("denom_sub_", sg, "_mp6")
            )
        }
    }

    return(input)
}
