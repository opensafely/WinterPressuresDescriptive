# Function to restrict to relevant variables ----------------------------------------
restrict_variables <- function(input) {
    message("Restricting to relevant variables only")

    # ----------------------------------------------------------------------
    # 1. Variables explicitly listed for removal
    # ----------------------------------------------------------------------
    vars_to_remove <- c(
        # Age groups
        grep("^exp_num_5_to_11$", names(input), value = TRUE),
        grep("^exp_prop_5_to_11$", names(input), value = TRUE),
        grep("^exp_num_12_to_17$", names(input), value = TRUE),
        grep("^exp_prop_12_to_17$", names(input), value = TRUE),
        grep("^exp_num_18_to_29$", names(input), value = TRUE),
        grep("^exp_prop_18_to_29$", names(input), value = TRUE),
        grep("^exp_num_30_to_44$", names(input), value = TRUE),
        grep("^exp_prop_30_to_44$", names(input), value = TRUE),
        grep("^exp_num_45_to_54$", names(input), value = TRUE),
        grep("^exp_prop_45_to_54$", names(input), value = TRUE),
        grep("^exp_num_55_to_64$", names(input), value = TRUE),
        grep("^exp_prop_55_to_64$", names(input), value = TRUE),

        # Male
        "exp_num_male",
        "exp_prop_male",

        # IMD 2–4
        "exp_num_imd_2",
        "exp_prop_imd_2",
        "exp_num_imd_3",
        "exp_prop_imd_3",
        "exp_num_imd_4",
        "exp_prop_imd_4"
    )

    # ----------------------------------------------------------------------
    # 2. Remove ALL exp_num_* and exp_denom_* EXCEPT denom_total + denom_total_r
    # ----------------------------------------------------------------------
    exp_num_denom_all <- grep(
        "^(exp_num_|exp_denom_)",
        names(input),
        value = TRUE
    )

    vars_to_keep <- c("exp_denom_total", "exp_denom_total_r")

    exp_num_denom_remove <- setdiff(exp_num_denom_all, vars_to_keep)

    # Add to removal list
    vars_to_remove <- c(vars_to_remove, exp_num_denom_remove)

    # ----------------------------------------------------------------------
    # Execute removal
    # ----------------------------------------------------------------------
    removed_vars <- vars_to_remove[vars_to_remove %in% names(input)]

    input <- input %>% select(-any_of(removed_vars))

    # Report
    message("Removed variables: ", paste(removed_vars, collapse = ", "))

    return(input)
}
