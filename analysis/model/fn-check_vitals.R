check_vitals <- function(input) {
    # Confirm general required variables are present --------------------------

    required_vars <- c(
        "practice_id",
        "out_num",
        "out_denom",
        "week_number"
    )

    missing_vars <- setdiff(
        required_vars,
        names(input)
    )

    if (length(missing_vars) > 0) {
        stop(
            paste0(
                "Missing required variables: ",
                paste(
                    missing_vars,
                    collapse = ", "
                )
            )
        )
    }

    # Confirm practice ID is complete -----------------------------------------

    if (anyNA(input$practice_id)) {
        stop(
            "Practice ID is not present for every observation in the dataset"
        )
    }

    # Identify exposure variables --------------------------------------------

    numeric_exposure_vars <- grep(
        "^exp_num_",
        names(input),
        value = TRUE
    )

    categorical_exposure_vars <- grep(
        "^exp_cat_",
        names(input),
        value = TRUE
    )


    exposure_vars <- c(numeric_exposure_vars, categorical_exposure_vars)

    if (length(exposure_vars) == 0) {
        stop(
            paste(
                "No exposure variables found.",
                "Expected variables beginning with exp_num_",
                "or exp_cat_."
            )
        )
    }

    # Check numeric exposure variables ---------------------------------------

    for (v in numeric_exposure_vars) {
        if (!is.numeric(input[[v]])) {
            stop(
                paste0(
                    v,
                    " must be numeric; current class: ",
                    paste(
                        class(input[[v]]),
                        collapse = ", "
                    )
                )
            )
        }
    }

    # Check categorical exposure variables -----------------------------------

    for (v in categorical_exposure_vars) {
        if (!is.factor(input[[v]])) {
            stop(
                paste0(
                    v,
                    " must be a factor; current class: ",
                    paste(
                        class(input[[v]]),
                        collapse = ", "
                    )
                )
            )
        }

        if (nlevels(input[[v]]) < 2) {
            stop(
                paste0(
                    v,
                    " must have at least two factor levels"
                )
            )
        }
    }

    # Check outcome variables -------------------------------------------------

    for (v in c("out_num", "out_denom")) {
        if (!is.numeric(input[[v]])) {
            stop(
                paste0(
                    v,
                    " must be numeric"
                )
            )
        }
    }

    if (any(input$out_num < 0, na.rm = TRUE)) {
        stop(
            "out_num must be greater than or equal to zero"
        )
    }

    if (any(input$out_denom <= 0, na.rm = TRUE)) {
        stop(
            "out_denom must be greater than zero for all observations"
        )
    }

    ## Week number checks
    if (!is.numeric(input$week_number)) {
        warning("week_number is not numeric, converting")
        input$week_number <- as.numeric(input$week_number)
    }

    # Identify covariates -----------------------------------------------------

    cov_core_vars <- grep(
        "^cov_core_",
        names(input),
        value = TRUE
    )

    cov_other_vars <- grep(
        "^cov_other_",
        names(input),
        value = TRUE
    )

    covariate_vars <- c(
        cov_core_vars,
        cov_other_vars
    )

    # Check covariate types ---------------------------------------------------

    for (v in covariate_vars) {
        if (
            !is.numeric(input[[v]]) &&
                !is.factor(input[[v]])
        ) {
            stop(
                paste0(
                    v,
                    " must be numeric or a factor; current class: ",
                    paste(
                        class(input[[v]]),
                        collapse = ", "
                    )
                )
            )
        }
    }

    # Report model-input structure -------------------------------------------

    if (length(exposure_vars) == 1) {
        input_type <- "single-exposure"
    } else {
        input_type <- "mutually adjusted"
    }

    message(
        paste0(
            "Input passed all vital checks for a ",
            input_type,
            " model"
        )
    )

    return(input)
}
