# Function to apply redaction ----------------------------------------
redact <- function(input) {
    message(
        "Applying rounding + generating _mp6 proportion variables using roundmid_num() specified in the utility.R"
    )

    # Identify numerator groups -------------------------------------------------

    # Exposures
    exp_num_vars <- grep("^exp_num_", names(input), value = TRUE)

    # Vaccination numerators
    exp_num_vax_vars <- grep("^exp_num_vax_", names(input), value = TRUE)

    # Consultation numerators
    exp_num_cons_vars <- grep("^exp_num_cons_", names(input), value = TRUE)

    # Standard exposure numerators (excluding vaccination and consultation)
    exp_num_standard <- setdiff(
        exp_num_vars,
        c(exp_num_vax_vars, exp_num_cons_vars)
    )

    # Denominators
    exp_denom_total <- "exp_denom_total"
    exp_denom_cons_vars <- grep("^exp_denom_cons_", names(input), value = TRUE)

    # Outcomes
    out_num_vars <- grep("^out_num_", names(input), value = TRUE)
    out_denom_vars <- grep("^out_denom[0-9]+$", names(input), value = TRUE)

    # Create lookup for outcome denominators (1–20)
    denom_index <- as.numeric(sub("^out_denom", "", out_denom_vars))
    denom_lookup <- setNames(out_denom_vars, denom_index)

    # --------------------------------------------------------------------------
    # EXPOSURE: standard numerators (use exp_denom_total)
    # --------------------------------------------------------------------------
    for (v in exp_num_standard) {
        input[[paste0(v, "_r")]] <- roundmid_num(input[[v]])
        input[[paste0(exp_denom_total, "_r")]] <- roundmid_num(input[[
            exp_denom_total
        ]])

        prop_new <- paste0(gsub("^exp_num_", "exp_prop_", v), "_mp6")

        input[[prop_new]] <-
            input[[paste0(v, "_r")]] / input[[paste0(exp_denom_total, "_r")]]
    }

    # --------------------------------------------------------------------------
    # EXPOSURE: vaccination numerators (use matched exp_denom_vax_*)
    # --------------------------------------------------------------------------
    for (v in exp_num_vax_vars) {
        suffix <- sub("^exp_num_vax_", "", v)
        denom_var <- paste0("exp_denom_vax_", suffix)

        if (!(denom_var %in% names(input))) {
            warning("No vaccination denominator for: ", v)
            next
        }

        input[[paste0(v, "_r")]] <- roundmid_num(input[[v]])
        input[[paste0(denom_var, "_r")]] <- roundmid_num(input[[denom_var]])

        prop_new <- paste0(gsub("^exp_num_", "exp_prop_", v), "_mp6")

        input[[prop_new]] <-
            input[[paste0(v, "_r")]] / input[[paste0(denom_var, "_r")]]
    }

    # -----------------------------------------------------------
    # EXPOSURE: consultation (use matched exp_denom_cons_*)
    # -----------------------------------------------------------
    for (v in exp_denom_cons_vars) {
        suffix <- sub("^exp_denom_cons_", "", v)
        num_var <- paste0("exp_num_cons_", suffix)

        if (!(num_var %in% names(input))) {
            next
        }

        input[[paste0(num_var, "_r")]] <- roundmid_num(input[[num_var]])
        input[[paste0(v, "_r")]] <- roundmid_num(input[[v]])

        prop_new <- paste0("exp_prop_cons_", suffix, "_mp6")

        input[[prop_new]] <- input[[paste0(num_var, "_r")]] /
            input[[paste0(v, "_r")]]
    }

    # --------------------------------------------------------------------------
    # OUTCOMES: match numerators to out_denomX by week number
    # --------------------------------------------------------------------------
    for (v in out_num_vars) {
        week <- as.numeric(sub(".*_w", "", v))

        if (!week %in% as.numeric(names(denom_lookup))) {
            warning("No matching denominator for: ", v)
            next
        }

        denom_var <- denom_lookup[[week]]

        input[[paste0(v, "_r")]] <- roundmid_num(input[[v]])
        input[[paste0(denom_var, "_r")]] <- roundmid_num(input[[denom_var]])

        prop_new <- paste0(gsub("^out_num_", "out_prop_", v), "_mp6")

        input[[prop_new]] <-
            input[[paste0(v, "_r")]] / input[[paste0(denom_var, "_r")]]
    }

    message(
        "Finished generating rounded numerators, denominators, and _mp6 proportions."
    )
    return(input)
}
