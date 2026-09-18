# Function to apply inclusion/exclusion criteria

inex_practice <- function(
    input,
    flow,
    exclude_small_practices = TRUE
) {
  # Main analysis: exclude practices with <1000 patients or missing list size.
  n_before <- n_distinct(input$practice_id)

  if (exclude_small_practices) {
    input <- filter(input, list_size >= 1000)
    description <- "Main analysis: exclude list size <1000 or missing"
  } else {
    description <- "Main analysis: list-size exclusion skipped"
  }

  n_after <- n_distinct(input$practice_id)

  flow[nrow(flow) + 1, ] <- list(
    description,
    n_after,
    n_before - n_after
  )
  print(flow[nrow(flow), ])

  # Sensitivity analysis: additionally exclude zero consultation rates.
  n_before <- n_after

  input_sensitivity <- filter(
    input,
    is.na(cons_mean) | cons_mean != 0
  )

  n_after <- n_distinct(input_sensitivity$practice_id)

  flow[nrow(flow) + 1, ] <- list(
    "Sensitivity analysis: exclude consultation rate == 0",
    n_after,
    n_before - n_after
  )
  print(flow[nrow(flow), ])

  return(list(
    input = input,
    input_sensitivity = input_sensitivity,
    flow = flow
  ))
}
