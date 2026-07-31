library(jsonlite)
library(dplyr)
library(stringr)
library(tidyr)

# Create output directory ----
fs::dir_create(here::here("lib"))

# Define cohorts ----
cohorts <- c("precovid", "postcovid1", "postcovid2", "postcovid3")

cohort_dates <- list(
    precovid = "2018-10-01",
    postcovid1 = "2022-10-01",
    postcovid2 = "2023-10-01",
    postcovid3 = "2024-10-01"
)

# Define subgroups ----
subgroups <- c(
    "sub_asth",
    "sub_copd",
    "sub_htn",
    "sub_diab",
    "sub_sevmh"
)

# Define outcomes ----
outcome_bases <- c(
    # APC – ACSC
    # "apc_acsc_copd",
    # "apc_acsc_asth",
    # "apc_acsc_htn",
    # "apc_acsc_diab",
    # "apc_acsc_ang",
    "apc_acsc_any",
    "apc_unpl_acsc_any",
    "apc_plan_acsc_any",

    # APC – overall
    "apc",
    "apc_unpl",
    "apc_plan",

    # EC – ACSC
    # "ec_acsc_copd",
    # "ec_acsc_asth",
    # "ec_acsc_htn",
    # "ec_acsc_diab",
    # "ec_acsc_ang",
    "ec_acsc_any",

    # EC – overall
    "ec"
)

# Define full outcome variable names ----
outcomes_main <- paste0(outcome_bases, "_main")

outcomes_sub <- as.vector(
    outer(outcome_bases, subgroups, paste, sep = "_")
)

outcome_names <- c(outcomes_main, outcomes_sub)

# Define exposure variable names ----

## Practice characteristics: region
exposure_region <- c(
    "practice_region"
)

## Practice characteristics: rurality
exposure_rurality <- c(
    "practice_rurality"
)

## Practice characteristics: List size
exposure_listsize <- c(
    "list_size"
)

## Practice characteristics: consultation frequency
exposure_consultation <- c(
    "cons_mean"
)

## Patient case-mix characteristics: age, sex
exposure_age <- c(
    "age_0_4",
    # "age_5_11",
    # "age_12_17",
    # "age_18_29",
    # "age_30_44",
    # "age_45_54",
    # "age_55_64",
    "age_65_74",
    "age_75_79",
    "age_80"
    # "age_missing"
)

exposure_sex <- c(
    # "sex_male",
    # "sex_missing",
    "sex_female"
)

## Patient case-mix characteristics: ethnicity

exposure_ethnicity <- c(
    "ethnicity_white",
    "ethnicity_mixed",
    "ethnicity_asian",
    "ethnicity_black",
    "ethnicity_other"
    # "ethnicity_missing"
)

## Patient case-mix characteristics: deprivation
exposure_imd <- c(
    "imd_1_most",
    # "imd_2",
    # "imd_3",
    # "imd_4",
    "imd_5_least"
    # "imd_missing"
)

## Patient case-mix characteristics: long-term conditions
exposure_conditions <- c(
    "cms_af",
    "cms_alcohol",
    "cms_anxdep",
    "cms_asthma",
    "cms_cancer",
    "cms_chd",
    "cms_ckd",
    "cms_constip",
    "cms_copd",
    "cms_ctd",
    "cms_dem",
    "cms_diabetes",
    "cms_epilepsy",
    "cms_hl",
    "cms_hf",
    "cms_htn",
    "cms_ibs",
    "cms_oa",
    "cms_psych",
    "cms_stia"
)

## Patient case-mix characteristics: other health conditions
exposure_other_health <- c(
    "obesity",
    "carehome"
)

## Patient case-mix characteristics: smoking status
exposure_smoking <- c(
    "smoking_current",
    "smoking_ever",
    "smoking_never"
    # "smoking_missing"
)

## Patient case-mix characteristics: vaccination
exposure_vax <- c(
    "vax_flu_y",
    "vax_pneum_y"
)

exposure_names <- c(
    exposure_region,
    exposure_rurality,
    exposure_listsize,
    exposure_consultation,
    exposure_age,
    exposure_sex,
    exposure_ethnicity,
    exposure_imd,
    # exposure_conditions,
    exposure_other_health,
    exposure_smoking
    # exposure_vax,
)

# Practice characteristics
exposure_practice <- c(
    exposure_region,
    exposure_rurality,
    exposure_listsize,
    exposure_consultation
)

# Patient case-mix characteristics
exposure_case_mix <- setdiff(
    exposure_names,
    exposure_practice
)

## Define covariates ----
covariate_age <- c("age_0_4", "age_80")
covariate_sex <- "sex_female"
covariate_region <- "practice_region"
covariate_ethnicity <- c("ethnicity_white")
covariate_imd <- c("imd_1_most")
covariate_rurality <- c("practice_rurality")
covariate_smoking <- c("smoking_current")
covariate_carehome <- c("carehome")
covariate_obesity <- c("obesity")
covariate_consultation <- c("cons_mean")
covariate_other <- c(
    covariate_region,
    covariate_ethnicity,
    covariate_imd,
    covariate_rurality,
    covariate_smoking,
    covariate_carehome,
    covariate_obesity,
    covariate_consultation
)
# Create empty data frame ----
df <- data.frame(
    cohort = character(),
    outcome_start = character(),
    exposure = character(),
    exposure_group = character(),
    outcome = character(),
    covariate_core = character(),
    covariate_other = character(),
    analysis = character(),
    stringsAsFactors = FALSE
)

# Generate analyses ----
for (i in cohorts) {
    outcome_start <- cohort_dates[[i]]
    for (j in exposure_names) {
        exposure_group <- if (j %in% exposure_practice) {
            "practice"
        } else if (j %in% exposure_case_mix) {
            "case_mix"
        } else {
            stop(paste0("ERROR: exposure variable ", j, " not in practice or case-mix group"))
        }
        covariate_core <- if (grepl("^age_", j)) {
            paste0(covariate_sex, collapse = ";")
        } else if (grepl("^sex_", j)) {
            paste0(covariate_age, collapse = ";")
        } else {
            paste0(c(covariate_age, covariate_sex), collapse = ";")
        }

        # Identify other covariates that overlap with the exposure ----------------

        covariates_to_exclude <- if (j %in% exposure_region) {
            covariate_region
        } else if (j %in% exposure_rurality) {
            covariate_rurality
        } else if (j %in% exposure_ethnicity) {
            covariate_ethnicity
        } else if (j %in% exposure_imd) {
            covariate_imd
        } else if (j %in% exposure_smoking) {
            covariate_smoking
        } else if (j %in% exposure_other_health) {
            intersect(j, covariate_other)
        } else if (j %in% exposure_consultation) {
            covariate_consultation
        } else {
            character(0)
        }

        covariate_other_clean <- paste(
            setdiff(
                covariate_other,
                covariates_to_exclude
            ),
            collapse = ";"
        )

        for (k in outcome_names) {
            analysis <- str_extract(k, "(main|sub_[a-z]+)")
            df <- rbind(
                df,
                data.frame(
                    cohort = i,
                    outcome_start = outcome_start,
                    exposure = j,
                    exposure_group = exposure_group,
                    outcome = k,
                    covariate_core = covariate_core,
                    covariate_other = covariate_other_clean,
                    analysis = analysis,
                    stringsAsFactors = FALSE
                )
            )
        }
    }
}

df <- df %>%
    mutate(
        analysis_type = "single_exposure"
    )

# Add the mutually adjusted analyses ----
df_mutually_adjusted <- crossing(
    cohort = cohorts,
    outcome = outcome_names
) %>%
    mutate(
        outcome_start = unname(unlist(cohort_dates[cohort])),
        exposure = paste(
            exposure_names, # we can change this to a subset of exposures if we want to limit the mutually adjusted analyses, also correlation heatmap can use identical subset of exposures to check for collinearity
            collapse = ";"
        ),
        exposure_group = "all",
        covariate_core = "",
        covariate_other = "",
        analysis = stringr::str_extract(
            outcome,
            "(main|sub_[a-z]+)"
        ),
        analysis_type = "mutually_adjusted"
    ) %>%
    select(
        cohort,
        outcome_start,
        exposure,
        exposure_group,
        outcome,
        covariate_core,
        covariate_other,
        analysis,
        analysis_type
    )

# Combine the single exposure and mutually adjusted analyses ----
df <- bind_rows(df, df_mutually_adjusted)

# Add name for each analysis ----

# Add name for each analysis --------------------------------------------------

df <- df %>%
    mutate(
        exposure_name = if_else(
            analysis_type == "mutually_adjusted",
            "all",
            exposure
        ),
        name = paste0(
            "cohort_",
            cohort,
            "-",
            analysis,
            "-",
            exposure_name,
            "-",
            gsub(
                "(_main|_sub_[a-z]+)",
                "",
                outcome
            )
        )
    ) %>%
    select(-exposure_name)

# Check names are unique and save active analyses list ----
if (length(unique(df$name)) == nrow(df)) {
    saveRDS(df, file = "lib/active_analyses.rds", compress = "gzip")
} else {
    stop("ERROR: names must be unique in active analyses table")
}
