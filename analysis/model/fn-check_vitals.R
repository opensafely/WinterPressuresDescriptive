check_vitals <- function(input) {
    # Confirm patient ID is complete

    if (nrow(input) != nrow(input[!is.na(input$practice_id), ])) {
        stop("Practice ID is not present for every practice in the dataset")
    }

    # Confirm vital covariates are present in dataset
    required_vars <- c("exp_prop", "out_num", "out_denom", "week_number")
    missing_vars <- setdiff(required_vars, colnames(input))
    if (length(missing_vars) > 0) {
        stop(paste(
            "Missing required variables:",
            paste(missing_vars, collapse = ", ")
        ))
    }

    ## Numeric checks
    for (v in c("out_num", "out_denom")) {
        if (!is.numeric(input[[v]])) {
            stop(v, " must be numeric")
        }
    }

    if (any(input$out_denom <= 0, na.rm = TRUE)) {
        stop("out_denom must be > 0 for all observations")
    }

    ## Week number checks
    if (!is.numeric(input$week_number)) {
        warning("week_number is not numeric, converting")
        input$week_number <- as.numeric(input$week_number)
    }

    ## Exposure and Covariates checks
    cov_core_vars <- grep("^cov_core_", names(input), value = TRUE)
    cov_other_vars <- grep("^cov_other_", names(input), value = TRUE)

    for (i in c("exp_prop", cov_core_vars, cov_other_vars)) {
        if (!(is.numeric(input[[i]]) || is.factor(input[[i]]))) {
            stop(paste0(i, " is not numeric or factor"))
        }
    }

    message("Input passed all vital checks")

    return(input)
}
