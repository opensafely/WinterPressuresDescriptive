# Create a Table 3-style table of Poisson versus negative binomial LR tests.
# Run from the repository root, as with the existing Table 3 script.

# Load libraries ---------------------------------------------------------------
print("Load libraries")
library(tidyverse)

# Specify paths ----------------------------------------------------------------
print("Specify paths")
source("analysis/specify_paths.R")

# Make post-release directory --------------------------------------------------
output_folder <- "output/post_release"
dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)

input_file <- file.path(output_folder, "plot_model_output_lr.csv")
output_file <- file.path(
    output_folder,
    "table3_all_outcomes_lr_age_sex.csv"
)

# Labels and ordering ----------------------------------------------------------
print("Add table labels")
labels <- readr::read_csv("lib/labels.csv", show_col_types = FALSE)

# Order the exposure codes directly, following the categories in Table 3.
# LR tests use the whole variable (e.g. practice_region), so no exposure
# label or group match is required in lib/labels.csv.
exposure_order <- c(
    "practice_region", "rurality_urban_comb", "list_size", "cons_mean",
    "age_0_4", "age_65_74", "age_75_79", "age_80",
    "sex_female",
    "ethnicity_white", "ethnicity_asian",
    "ethnicity_black", "ethnicity_mixed", "ethnicity_other",
    "imd_1_most", "imd_5_least",
    "smoking_never", "smoking_ever", "smoking_current", 
    "obesity", "carehome"
)

cohort_order <- c("precovid", "postcovid1", "postcovid2", "postcovid3")

# Load LR-test output ----------------------------------------------------------
print("Load likelihood-ratio test output")
df <- readr::read_csv(input_file, show_col_types = FALSE)

required_columns <- c(
    "cohort", "analysis", "exposure", "outcome", "model", "term",
    "chi2_lr", "p_lr", "n_obs_midpoint6"
)
missing_columns <- setdiff(required_columns, names(df))
if (length(missing_columns)) {
    stop("Missing input column(s): ", paste(missing_columns, collapse = ", "))
}

# Filter tests; exposure already identifies the tested exposure/model.
# Do not replace exposure with term, or filter on model_type.
df <- df %>%
    filter(model == "mdl_age_sex", term == "poisson_vs_negbin") %>%
    select(
        cohort, analysis, exposure, outcome,
        chi2_lr, p_lr, n_obs_midpoint6
    ) %>%
    mutate(outcome = str_remove(outcome, paste0("_", analysis, "$")))

if (!nrow(df)) {
    stop("No rows match mdl_age_sex and poisson_vs_negbin.")
}
if (anyNA(df[, c("cohort", "analysis", "exposure", "outcome")])) {
    stop("Missing cohort, analysis, exposure or outcome in the selected tests.")
}
if (any(!df$cohort %in% cohort_order)) {
    stop("Unexpected cohort code(s) in the LR-test output.")
}

# Prevent pivot_wider() from creating list columns or combining different tests.
duplicate_tests <- df %>%
    count(analysis, outcome, exposure, cohort) %>%
    filter(n > 1L)
if (nrow(duplicate_tests)) {
    print(duplicate_tests)
    stop("More than one LR test per subgroup/outcome/exposure/cohort; check name.")
}

# Join outcome labels ----------------------------------------------------------
outcome_labels <- labels %>%
    filter(str_detect(term, "^(apc|ec)")) %>%
    select(
        term,
        outcome_label = label,
        outcome_group = group, outcome_ref = ref
    ) %>%
    distinct()

df <- df %>% left_join(outcome_labels, by = c("outcome" = "term"))

# Join subgroup labels ---------------------------------------------------------
analysis_labels <- labels %>%
    filter(term %in% unique(df$analysis)) %>%
    select(
        term,
        analysis_label = label,
        analysis_group = group, analysis_ref = ref
    ) %>%
    distinct()

df <- df %>% left_join(analysis_labels, by = c("analysis" = "term"))

# Use the same cohort display labels, in the same order as Table 3.
cohort_labels <- labels %>%
    filter(term %in% cohort_order) %>%
    transmute(cohort = term, cohort_label = label) %>%
    distinct()

if (anyDuplicated(cohort_labels$cohort)) {
    stop("Conflicting cohort labels in lib/labels.csv.")
}
cohort_display <- setNames(cohort_labels$cohort_label, cohort_labels$cohort)
cohort_display <- cohort_display[cohort_order]
if (anyNA(cohort_display)) {
    stop("A cohort display label is missing from lib/labels.csv.")
}

# A label join should not multiply test rows.
if (anyDuplicated(df[c("analysis", "outcome", "exposure", "cohort")])) {
    stop("Duplicate label entries multiplied test rows; check lib/labels.csv.")
}

# Factor setup and row ordering ------------------------------------------------
# Retain any additional exposure codes, after the explicitly ordered ones.
exposure_levels <- c(exposure_order, setdiff(unique(df$exposure), exposure_order))

df <- df %>%
    mutate(
        # Fall back to the source code if an individual label is unavailable.
        outcome_label = coalesce(outcome_label, outcome),
        analysis_label = coalesce(analysis_label, analysis),
        cohort = factor(cohort, levels = cohort_order),
        exposure = factor(exposure, levels = exposure_levels),
        outcome_group = factor(
            outcome_group,
            levels = c(
                "admitted patient care",
                "acsc admitted patient care",
                "emergency care"
            )
        ),
        analysis_group = factor(analysis_group, levels = c("main", "subgroup")),
        analysis_ref_order = if_else(is.na(analysis_ref), Inf, analysis_ref)
    ) %>%
    arrange(outcome_group, outcome_ref, exposure)

outcome_levels <- df %>%
    distinct(outcome_group, outcome_label, outcome_ref) %>%
    arrange(outcome_group, outcome_ref) %>%
    pull(outcome_label) %>%
    unique()

# Format the reported statistics; do not recalculate LR p values.
df_table3_lr <- df %>%
    mutate(
        outcome_label = factor(outcome_label, levels = outcome_levels),
        chi2_lr = if_else(
            is.na(chi2_lr), NA_character_, sprintf("%.2f", chi2_lr)
        ),
        p_lr = case_when(
            is.na(p_lr) ~ NA_character_,
            p_lr < 0.001 ~ "<0.001",
            TRUE ~ sprintf("%.3f", p_lr)
        )
        # Keep n_obs_midpoint6 exactly as supplied in the released file.
    ) %>%
    select(
        analysis, analysis_label, analysis_group, analysis_ref_order,
        outcome, outcome_label, outcome_group, outcome_ref,
        exposure,
        cohort, chi2_lr, p_lr, n_obs_midpoint6
    ) %>%
    arrange(
        analysis_group, analysis_ref_order,
        outcome_label, exposure
    ) %>%
    tidyr::pivot_wider(
        names_from = cohort,
        values_from = c(chi2_lr, p_lr, n_obs_midpoint6),
        names_glue = "{cohort}_{.value}",
        names_expand = TRUE
    )

# Place the three statistics together under each cohort.
# Header labels come from lib/labels.csv, as in the original Table 3.
statistic_codes <- c("chi2_lr", "p_lr", "n_obs_midpoint6")
statistic_labels <- c("LR chi-square", "LR p value", "N observations")

column_names <- unlist(lapply(cohort_order, function(cohort_code) {
    setNames(
        paste(cohort_display[[cohort_code]], statistic_labels),
        paste(cohort_code, statistic_codes, sep = "_")
    )
}), use.names = TRUE)

df_table3_lr <- df_table3_lr %>%
    select(
        Subgroup = analysis_label,
        Outcome = outcome_label,
        Exposure = exposure,
        all_of(names(column_names))
    ) %>%
    rename_with(
        ~ unname(column_names[.x]),
        all_of(names(column_names))
    )

# Export -----------------------------------------------------------------------
readr::write_csv(df_table3_lr, output_file, na = "-")
message("Saved ", output_file)
