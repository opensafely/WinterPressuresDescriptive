reshape_outcomes_long <- function(input) {
    message("Reshaping ONLY outcomes to long format (keeping exposures wide)")

    library(dplyr)
    library(tidyr)
    library(stringr)

    # Identify outcome-wide columns
    outcome_cols <- grep("^out_", names(input), value = TRUE)

    # Keep all exposure variables + reshape only these outcome cols
    long <- input %>%
        pivot_longer(
            cols = all_of(outcome_cols),
            names_to = "variable",
            values_to = "value"
        ) %>%
        mutate(
            # Extract week (w1–w20)
            week = as.numeric(
                str_extract(variable, "w[0-9]+") %>% str_remove("w")
            ),

            # APC/EC setting
            setting = case_when(
                str_detect(variable, "_apc_") ~ "APC",
                str_detect(variable, "_ec_") ~ "EC",
                TRUE ~ "standard"
            ),

            # ACSC vs standard
            type = ifelse(str_detect(variable, "acscs"), "ACSC", "standard"),

            # ACSC condition
            condition = case_when(
                str_detect(variable, "copd") ~ "copd",
                str_detect(variable, "asthma") ~ "asthma",
                str_detect(variable, "hypt") ~ "hypt",
                str_detect(variable, "diabetes") ~ "diabetes",
                str_detect(variable, "angina") ~ "angina",
                TRUE ~ NA_character_
            ),

            # Outcome measure type
            measure = case_when(
                str_detect(variable, "_num_") &
                    str_ends(variable, "_r") ~ "num_r",
                str_detect(variable, "_num_") ~ "num",
                str_detect(variable, "^out_denom[0-9]+_r") ~ "denom_r",
                str_detect(variable, "^out_denom[0-9]+$") ~ "denom",
                str_detect(variable, "_prop_") &
                    str_ends(variable, "_mp6") ~ "prop_mp6",
                str_detect(variable, "_prop_") ~ "prop",
                TRUE ~ NA_character_
            )
        )

    return(long)
}
