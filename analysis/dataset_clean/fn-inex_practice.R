# Function to apply inclusion/exclusion criteria

inex <- function(
    input
) {
    # Remove practices with <1000 patients ----------------------------------------

    print("Remove practices with <1000 patients")

    n_before <- nrow(input)

    input <- input %>%
        filter(exp_denom_total >= 1000)

    n_after <- nrow(input)

    n_removed <- n_before - n_after

    message(paste0("Number of practices with <1000 patients: ", n_removed))
    message(paste0(
        "Practice summary dataset after removing small practices has N = ",
        n_after,
        " rows"
    ))
    return(input)
}
