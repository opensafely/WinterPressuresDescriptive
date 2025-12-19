library(jsonlite)
library(dplyr)
library(stringr)

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
    # "apc_plan",

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
exposure_listsize <- c(
    "list_size"
)

## Exposure: region
exposure_region <- c(
    "practice_region"
)

## Exposure: demographic exposures
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

exposure_ethnicity <- c(
    "ethnicity_white",
    "ethnicity_mixed",
    "ethnicity_asian",
    "ethnicity_black",
    "ethnicity_other"
    # "ethnicity_missing"
)

## Exposure: socioeconomic exposures
exposure_imd <- c(
    "imd_1_most",
    # "imd_2",
    # "imd_3",
    # "imd_4",
    "imd_5_least"
    # "imd_missing"
)

exposure_rurality <- c(
    # "rurality_urban_town",
    # "rurality_rural_fringe",
    # "rurality_rural_village",
    "rurality_urban_comb"
    # "rurality_missing"
)

# Exposure: health-related exposures
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

exposure_other_health <- c(
    "obesity",
    "carehome"
)

## Exposure: lifestyle exposures
exposure_smoking <- c(
    "smoking_current",
    "smoking_ever",
    "smoking_never"
    # "smoking_missing"
)

## Exposure: consultation frequency
exposure_consultation <- c(
    "cons_mean"
)

## Exposure:vaccination
exposure_vax <- c(
    "vax_flu_y",
    "vax_pneum_y"
)

exposure_names <- c(
    exposure_listsize,
    exposure_region,
    exposure_age,
    exposure_sex,
    exposure_ethnicity,
    exposure_imd,
    exposure_rurality,
    # exposure_conditions,
    exposure_other_health,
    exposure_smoking,
    # exposure_vax,
    exposure_consultation
)

## Define covariates ----
covariate_age <- "age_80"
covariate_sex <- "sex_female"
covariate_region <- "practice_region"
covariate_ethnicity <- c("ethnicity_white")
covariate_imd <- c("imd_1_most")
covariate_rurality <- c("rurality_urban_comb")
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
        covariate_core <- if (grepl("^age_", j)) {
            paste0(covariate_sex, collapse = ";")
        } else if (grepl("^sex_", j)) {
            paste0(covariate_age, collapse = ";")
        } else {
            paste0(c(covariate_age, covariate_sex), collapse = ";")
        }

        characteristic <- sub("_.*$", "", j)

        covariate_other_clean <- paste(
            covariate_other[
                !grepl(paste0("^", characteristic), covariate_other)
            ],
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

# Add name for each analysis ----

df$name <- paste0(
    "cohort_",
    df$cohort,
    "-",
    df$analysis,
    "-",
    df$exposure,
    "-",
    gsub("(_main|_sub_[a-z]+)", "", df$outcome)
)

# Check names are unique and save active analyses list ----
if (length(unique(df$name)) == nrow(df)) {
  saveRDS(df, file = "lib/active_analyses.rds", compress = "gzip")
} else {
  stop("ERROR: names must be unique in active analyses table")
}