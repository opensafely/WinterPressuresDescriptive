# Exclude practices with zero mean consultation rates.
# Missing consultation rates do not trigger exclusion.

exclude_zero_consultation <- function(input, flow) {
    # Check the variables needed for this exclusion ------------------------------
    if (!all(c("practice_id", "cons_mean") %in% names(input))) {
        stop("The input must contain practice_id and original cons_mean.")
    }

    if (anyNA(input$practice_id)) {
        stop("The input contains missing practice_id values.")
    }

    n_before <- n_distinct(input$practice_id)

    # Filter out practices with zero consultation rates ----------------------------
    input <- input %>%
        filter(is.na(cons_mean) | cons_mean > 0.005)

    n_after <- n_distinct(input$practice_id)

    # Append the number of remaining practices to the flow table ------------------
    flow <- bind_rows(
        flow,
        data.frame(
            Description = "Sensitivity analysis: exclude practices with average monthly consultation rates <= 0.005",
            N = n_after,
            stringsAsFactors = FALSE
        )
    )

    message("Practices excluded with consultation rates <= 0.005: ", n_before - n_after)
    message("Practices remaining for sensitivity analysis: ", n_after)
    print(flow[nrow(flow), ])

    return(list(
        input = input,
        flow = flow
    ))
}
