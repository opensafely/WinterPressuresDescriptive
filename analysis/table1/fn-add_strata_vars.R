add_strata_vars <- function(
    input,
    Strata = TRUE
) {

    if (Strata) {
        message("Creating strata variables for stratification")
        # GP characteristics of interest for making strata and calculating cutoffs ----

        vars_interest <- c(
            "sex_female",
            "imd_1_most",
            "ethnicity_white",
            "obesity",
            "smoking_current",
            "age_0_4",
            "age_80",
            "rurality_urban_comb"
        )
        # safety check (important!)
        missing_vars <- setdiff(vars_interest, names(input))
        if (length(missing_vars) > 0) {
            stop(
                "Missing variables for strata: ",
                paste(missing_vars, collapse = ", ")
            )
        }
        # 75th percentile thresholds for each
        cutoffs_p75 <- input %>%
            summarise(
                across(
                    all_of(vars_interest),
                    ~ quantile(.x, 0.75, na.rm = TRUE)
                )
            ) %>%
            as.list()

        # 25th percentile thresholds for each
        cutoffs_p25 <- input %>%
            summarise(
                across(
                    all_of(vars_interest),
                    ~ quantile(.x, 0.25, na.rm = TRUE)
                )
            ) %>%
            as.list()

        # add strata flags
        input <- input %>%
            mutate(
                strata_female_above_p75 = as.integer(
                    sex_female >= cutoffs_p75$sex_female
                ),
                strata_imd1_above_p75 = as.integer(
                    imd_1_most >= cutoffs_p75$imd_1_most
                ),
                strata_eth_non_white_above_p75 = as.integer(
                    ethnicity_white <= cutoffs_p25$ethnicity_white
                ),
                strata_obesity_above_p75 = as.integer(
                    obesity >= cutoffs_p75$obesity
                ),
                strata_smoker_above_p75 = as.integer(
                    smoking_current >= cutoffs_p75$smoking_current
                ),
                strata_under5y_above_p75 = as.integer(
                    age_0_4 >= cutoffs_p75$age_0_4
                ),
                strata_age80plus_above_p75 = as.integer(
                    age_80 >= cutoffs_p75$age_80
                ),
                strata_urban_comb_above_p75 = as.integer(
                    rurality_urban_comb >= cutoffs_p75$rurality_urban_comb
                )
            )
        message("Strata flags added to dataset")
    }

    return(input)
}
