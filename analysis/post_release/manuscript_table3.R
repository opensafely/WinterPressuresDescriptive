# Load libraries ---------------------------------------------------------------
print("Load libraries")

library(magrittr)
library(tidyverse)
library(purrr)
library(data.table)
library(tidyverse)
library(svglite)
library(VennDiagram)
library(grid)
library(gridExtra)
library(ggtext)

# Specify paths ----------------------------------------------------------------
print("Specify paths")

# NOTE:
# This file is used to specify paths and is in the .gitignore to keep your information secret.
# A file called specify_paths_example.R is provided for you to fill in.
# Please remove "_example" from the file name and add your specific file paths before running this script.

source("analysis/specify_paths.R")

# Make post-release directory --------------------------------------------------
print("Make post-release directory")


dir.create("output/post_release/", recursive = TRUE, showWarnings = FALSE)
output_folder <- "output/post_release"


# Add plot labels ---------------------------------------------------------
print("Add plot labels")

labels <- readr::read_csv("lib/labels.csv", show_col_types = FALSE)

# Define group order for plotting
group_order <- c(
  "Practice region",
  "Rurality",
  "List size",
  "Monthly consultation",
  "Age",
  "Sex",
  "Ethnicity",
  "Deprivation",
  "Smoking Status",
  "Obesity",
  "Care home residence"
)

# regression can be negbin or poisson
# outcomes can be apc_main; apc_acsc_any_main; apc_plan_acsc_any_main; apc_unpl_main; apc_unpl_acsc_any_main; ec_main; ec_acsc_any_main

# Load data --------------------------------------------------------------------
print("Load model output")

df <- readr::read_csv(
  "output/post_release/plot_model_output.csv",
  show_col_types = FALSE
)

# Filter data ------------------------------------------------------------------
print("Filter data")

df <- df %>%
  filter(
    model == "mdl_age_sex",
    model_type == "negbin",
    grepl("^exp_prop(_|$)", term)
  ) %>%
  mutate(
    exposure = if_else(
      term == "exp_prop", exposure, term
    )
  ) %>%
  select(
    cohort,
    analysis,
    exposure,
    outcome,
    model_type,
    model,
    irr,
    lci,
    uci,
    n_obs_midpoint6,
    mad
  ) %>%
  mutate(
    # model aesthetics
    model = factor(
      model,
      levels = c("mdl_crude", "mdl_age_sex"),
      labels = c("Crude", "Age–sex adjusted")
    ),
    cohort = factor(
      cohort,
      levels = c(
        "precovid",
        "postcovid1",
        "postcovid2",
        "postcovid3"
      )
    ),
    exposure = factor(exposure) # will control y-axis order later
  )

# --- Join EXPOSURE labels ---
exposure_labels <- labels %>%
  filter(!str_detect(term, "apc|ec")) %>%
  select(term, exposure_label = label, group, ref)

df <- df %>%
  left_join(
    exposure_labels,
    by = c("exposure" = "term")
  )

# --- Join OUTCOME label ---
outcome_labels <- labels %>%
  filter(str_detect(term, "^apc|^ec")) %>%
  select(term, outcome_label = label, outcome_group = group, outcome_ref = ref)

df <- df %>% mutate(
  outcome = str_remove(outcome, paste0("_", analysis))
)

df <- df %>%
  left_join(
    outcome_labels,
    by = c("outcome" = "term")
  )

outcome_levels <- df %>%
  distinct(
    outcome_group,
    outcome_label,
    outcome_ref
  ) %>%
  mutate(
    outcome_group = factor(
      outcome_group,
      levels = c(
        "admitted patient care",
        "acsc admitted patient care",
        "emergency care"
      )
    )
  ) %>%
  arrange(
    outcome_group,
    outcome_ref
  ) %>%
  pull(outcome_label)

df <- df %>%
  mutate(
    outcome_label = factor(
      outcome_label,
      levels = outcome_levels
    )
  )

# --- Join COHORT labels ---
cohort_labels <- labels %>%
  filter(
    term %in% c("precovid", "postcovid1", "postcovid2", "postcovid3")
  ) %>%
  select(term, label)

df <- df %>%
  left_join(
    cohort_labels,
    by = c("cohort" = "term")
  ) %>%
  rename(cohort_label = label)

# --- Join analysis labels ---
subgroups <- unique(df$analysis)
analysis_labels <- labels %>%
  filter(
    term %in% c(subgroups)
  ) %>%
  select(term, analysis_label = label, analysis_group = group, analysis_ref = ref)

df <- df %>%
  left_join(
    analysis_labels,
    by = c("analysis" = "term")
  )

# --- Factor setup ---
df <- df %>%
  mutate(
    cohort_label = factor(
      cohort_label,
      levels = cohort_labels$label
    )
  ) %>%
  mutate(
    analysis_group = factor(
      analysis_group,
      levels = c("main", "subgroup")
    ),
    analysis_ref_order = if_else(is.na(analysis_ref), Inf, analysis_ref)
  ) %>%
  mutate(
    group = factor(
      group,
      levels = group_order
    ),
    ref_order = if_else(is.na(ref), Inf, ref)
  ) %>%
  arrange(group, outcome_group, outcome_ref, ref_order)

df_table3 <- df %>%
  mutate(
    exposure_label = factor(
      exposure_label,
      levels = unique(exposure_label)
    )
  )

format_irr <- function(irr, lci, uci) {
  sprintf("%.2f (%.2f-%.2f)", irr, lci, uci)
}

df_table3 <- df_table3 %>%
  mutate(
    estimate = format_irr(irr, lci, uci)
  )

df_table3 <- df_table3 %>%
  select(
    analysis,
    analysis_label,
    analysis_group,
    analysis_ref_order,
    outcome,
    outcome_label,
    outcome_group,
    outcome_ref,
    group,
    exposure_label,
    ref_order,
    cohort_label,
    estimate,
    mad
  ) %>%
  arrange(
    analysis_group,
    analysis_ref_order,
    outcome_label,
    group,
    ref_order
  )

df_table3 <- df_table3 %>%
  tidyr::pivot_wider(
    names_from = cohort_label,
    values_from = c(estimate, mad),
    names_glue = "{cohort_label}_{.value}"
  )

df_table3 <- df_table3 %>%
  rename(
    "Subgroup" = analysis_label,
    "Outcome" = outcome_label,
    "Exposure" = exposure_label,
    "Pre-COVID19 MAD" = `Pre-COVID19_mad`,
    "Pre-COVID19 IRR (95% CI)" = `Pre-COVID19_estimate`,
    "2022/23 MAD" = `2022/23_mad`,
    "2022/23 IRR (95% CI)" = `2022/23_estimate`,
    "2023/24 MAD" = `2023/24_mad`,
    "2023/24 IRR (95% CI)" = `2023/24_estimate`,
    "2024/25 MAD" = `2024/25_mad`,
    "2024/25 IRR (95% CI)" = `2024/25_estimate`
  ) %>%
  select(
    Subgroup,
    Outcome,
    Exposure,
    `Pre-COVID19 MAD`,
    `Pre-COVID19 IRR (95% CI)`,
    `2022/23 MAD`,
    `2022/23 IRR (95% CI)`,
    `2023/24 MAD`,
    `2023/24 IRR (95% CI)`,
    `2024/25 MAD`,
    `2024/25 IRR (95% CI)`
  )

readr::write_csv(
  df_table3,
  paste0(output_folder, "/table3_all_outcomes_negbin_age_sex.csv"),
  na = "-"
)
