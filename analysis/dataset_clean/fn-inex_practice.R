# Function to apply inclusion/exclusion criteria

inex_practice <- function(
    input,
    flow,
    exclude_small_practices = TRUE,
    exclude_unknown_region = TRUE
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

  n_before <- n_distinct(input$practice_id)

  if (exclude_unknown_region) {
    input <- filter(input, !is.na(practice_region))
    description <- "Main analysis: exclude practices with unknown region"
  } else {
    description <- "Main analysis: unknown region exclusion skipped"
  }

  n_after <- n_distinct(input$practice_id)

  flow[nrow(flow) + 1, ] <- list(
    description,
    n_after,
    n_before - n_after
  )
  print(flow[nrow(flow), ])
  

  return(list(
    input = input,
    flow = flow
  ))
}
