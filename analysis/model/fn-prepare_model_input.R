prepare_model_input <- function(name) {
    # Load active analyses ---------------------------------------------------------
    print("Load active analyses")

    active_analyses <- readr::read_rds("lib/active_analyses.rds")

    # Filter active_analyses to model inputs to be prepared ------------------------
    print("Filter active_analyses to model inputs to be prepared")

    active_analysis <- active_analyses[active_analyses$name == name, ]

    if (nrow(active_analysis) == 0) {
        stop(paste0("Input: ", name, " does not match any analyses"))
    }

    if (nrow(active_analysis) > 1) {
        stop(
            paste0(
                "Input: ",
                name,
                " matches more than one analysis"
            )
        )
    }

    # Parse exposures ---------------------------------------------------------

    exposure_vars <- if (
        !is.na(active_analysis$exposure) &&
            active_analysis$exposure != ""
    ) {
        unlist(
            strsplit(
                active_analysis$exposure,
                ";",
                fixed = TRUE
            )
        )
    } else {
        character(0)
    }

    if (length(exposure_vars) == 0) {
        stop(
            paste0(
                "No exposure variables were defined for: ",
                name
            )
        )
    }

    # Identify categorical and numeric exposures --------------------------

    categorical_exposure_vars <- intersect(
        exposure_vars,
        c(
            "practice_region",
            "practice_rurality"
        )
    )

    numeric_exposure_vars <- setdiff(
        exposure_vars,
        categorical_exposure_vars
    )

    # Parse core covariates ---------------------------------------------------

    cov_core_vars <- if (
        !is.na(active_analysis$covariate_core) &&
            active_analysis$covariate_core != ""
    ) {
        unlist(
            strsplit(
                active_analysis$covariate_core,
                ";",
                fixed = TRUE
            )
        )
    } else {
        character(0)
    }

    # Parse other covariates --------------------------------------------------

    cov_other_vars <- if (
        !is.na(active_analysis$covariate_other) &&
            active_analysis$covariate_other != ""
    ) {
        unlist(
            strsplit(
                active_analysis$covariate_other,
                ";",
                fixed = TRUE
            )
        )
    } else {
        character(0)
    }

    # Load data ------------------------------------------------------------------
    print(paste0("Load data for ", active_analysis$name))

    input <- readr::read_rds(paste0(
        "output/dataset_clean/input_",
        active_analysis$cohort,
        "_clean.rds"
    ))

    ## ---- Ensure practice_region is a factor ------------------------------------
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
        input$practice_region <- relevel(input$practice_region, ref = "East")
    }

    ## ---- Ensure practice_rurality is a factor ------------------------------------
    rurality_levels <- c(
        "Urban conurbation",
        "Urban town",
        "Rural"
    )

    if ("practice_rurality" %in% names(input)) {
        input$practice_rurality <- factor(
            input$practice_rurality,
            levels = rurality_levels
        )
        input$practice_rurality <- relevel(input$practice_rurality, ref = "Urban conurbation")
    }

    # Restrict to required variables for dataset preparation ---------------------
    print("Restrict to required variables for dataset preparation")

    outcome_numerator <- paste0(
        "num_",
        active_analysis$outcome
    )

    outcome_denominator <- paste0(
        "denom_",
        active_analysis$analysis
    )

    reqvars <- unique(
        c(
            "practice_id",
            exposure_vars,
            outcome_numerator,
            outcome_denominator,
            cov_core_vars,
            cov_other_vars,
            "week_number"
        )
    )

    input <- input[, intersect(reqvars, colnames(input))]

    if (length(setdiff(reqvars, colnames(input))) > 0) {
        message(
            "Variables (",
            setdiff(reqvars, colnames(input)),
            ") not present in dataset"
        )
    }

    ## ---- Rename to standardised names -------------------------------------------

    # Create renamed exposure names ----------------------------------------------

    numeric_exposure_names <- add_prefix(
        numeric_exposure_vars,
        "exp_num_"
    )

    categorical_exposure_names <- add_prefix(
        categorical_exposure_vars,
        "exp_cat_"
    )

    # Create exposure renaming map -----------------------------------------------

    exposure_rename_map <- c(
        setNames(
            numeric_exposure_vars,
            numeric_exposure_names
        ),
        setNames(
            categorical_exposure_vars,
            categorical_exposure_names
        )
    )

    # Create outcome renaming map --------------------------------------------

    outcome_rename_map <- c(
        out_num = outcome_numerator,
        out_denom = outcome_denominator
    )

    # Combine renaming maps ---------------------------------------------------

    rename_map <- c(
        exposure_rename_map,
        outcome_rename_map
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

    # Rename variables in input dataset ---------------------------------------
    input <- dplyr::rename(input, !!!rename_map)

    # Identify exposure names after renaming -------------------------------------

    model_exposure_vars <- c(
        numeric_exposure_names,
        categorical_exposure_names
    )

    exposures_to_standardise <- numeric_exposure_names

    # Standardise numeric exposures by MAD --------------------------------

    for (exposure_var in exposures_to_standardise) {
        if (!is.numeric(input[[exposure_var]])) {
            stop(
                paste0(
                    "Exposure ",
                    exposure_var,
                    " was expected to be numeric but has class: ",
                    paste(
                        class(input[[exposure_var]]),
                        collapse = ", "
                    )
                )
            )
        }

        exposure_median <- median(
            input[[exposure_var]],
            na.rm = TRUE
        )

        exposure_mad <- mad(
            input[[exposure_var]],
            na.rm = TRUE
        )

        if (
            is.finite(exposure_mad) &&
                exposure_mad > 0
        ) {
            input[[exposure_var]] <-
                (
                    input[[exposure_var]] - exposure_median
                ) / exposure_mad
        } else {
            warning(
                paste0(
                    "Exposure ",
                    exposure_var,
                    " was not standardised because its MAD ",
                    "was zero or missing"
                )
            )
        }
    }

    # Identify final list of variables to keep -----------------------------------
    print("Identify final list of variables to keep")

    keep <- unique(
        c(
            "practice_id",
            model_exposure_vars,
            "out_num",
            "out_denom",
            "week_number",
            add_prefix(
                cov_core_vars,
                "cov_core_"
            ),
            add_prefix(
                cov_other_vars,
                "cov_other_"
            )
        )
    )

    input <- input[, intersect(keep, colnames(input))]

    return(input)
}
