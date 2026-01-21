prepare_model_input <- function(name) {
    # Load active analyses ---------------------------------------------------------
    print("Load active analyses")

    active_analyses <- readr::read_rds("lib/active_analyses.rds")

    # Filter active_analyses to model inputs to be prepared ------------------------
    print("Filter active_analyses to model inputs to be prepared")

    active_analyses <- active_analyses[active_analyses$name == name, ]

    if (nrow(active_analyses) == 0) {
        stop(paste0("Input: ", name, " does not match any analyses"))
    }

    # Load data ------------------------------------------------------------------
    print(paste0("Load data for ", active_analyses$name))

    input <- readr::read_rds(paste0(
        "output/dataset_clean/input_",
        active_analyses$cohort,
        "_clean.rds"
    ))

    ## ---- Ensure practice_region is a factor ------------------------------------
    region_levels <- c(
        "North East",
        "North West",
        "Yorkshire and The Humber",
        "East Midlands",
        "West Midlands",
        "East",
        "London",
        "South East",
        "South West"
    )

    if ("practice_region" %in% names(input)) {
        input$practice_region <- factor(
            input$practice_region,
            levels = region_levels
        )
    }

    ## ---- Parse covariates -------------------------------------------------------
    cov_core_vars <- if (
        !is.na(active_analyses$covariate_core) &&
            active_analyses$covariate_core != ""
    ) {
        unlist(strsplit(active_analyses$covariate_core, ";"))
    } else {
        character(0)
    }

    cov_other_vars <- if (
        !is.na(active_analyses$covariate_other) &&
            active_analyses$covariate_other != ""
    ) {
        unlist(strsplit(active_analyses$covariate_other, ";"))
    } else {
        character(0)
    }

    # Restrict to required variables for dataset preparation ---------------------
    print("Restrict to required variables for dataset preparation")

    reqvars <- unique(c(
        "practice_id",
        "practice_region",
        "list_size",
        active_analyses$exposure,
        paste0("num_", active_analyses$outcome),
        paste0("denom_", active_analyses$analysis),
        cov_core_vars,
        cov_other_vars,
        "week_number"
    ))

    input <- input[, intersect(reqvars, colnames(input))]

    if (length(setdiff(reqvars, colnames(input))) > 0) {
        message(
            "Variables (",
            setdiff(reqvars, colnames(input)),
            ") not present in dataset"
        )
    }

    ## ---- Rename to standardised names -------------------------------------------
    rename_map <- c(
        setNames(active_analyses$exposure, "exp_prop"),
        setNames(paste0("num_", active_analyses$outcome), "out_num"),
        setNames(paste0("denom_", active_analyses$analysis), "out_denom")
    )

    if (length(cov_core_vars) > 0) {
        rename_map <- c(
            rename_map,
            setNames(cov_core_vars, paste0("cov_core_", cov_core_vars))
        )
    }

    if (length(cov_other_vars) > 0) {
        rename_map <- c(
            rename_map,
            setNames(cov_other_vars, paste0("cov_other_", cov_other_vars))
        )
    }

    input <- dplyr::rename(input, !!!rename_map)

    # Identify final list of variables to keep -----------------------------------
    print("Identify final list of variables to keep")

    keep <- c(
        "practice_id",
        "exp_prop",
        "out_num",
        "out_denom",
        "week_number",
        paste0("cov_core_", cov_core_vars),
        paste0("cov_other_", cov_other_vars)
    )

    input <- input[, intersect(keep, colnames(input))]

    return(input)
}
