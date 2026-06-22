# Load libraries ---------------------------------------------------------------
print('Load libraries')

library(magrittr)
library(tidyverse)
library(purrr)
library(data.table)
library(tidyverse)
library(svglite)
library(VennDiagram)
library(grid)
library(gridExtra)

# Specify paths ----------------------------------------------------------------
print('Specify paths')

# NOTE:
# This file is used to specify paths and is in the .gitignore to keep your information secret.
# A file called specify_paths_example.R is provided for you to fill in.
# Please remove "_example" from the file name and add your specific file paths before running this script.

source("analysis/specify_paths.R")

# Make post-release directory --------------------------------------------------
print('Make post-release directory')

dir.create("output/post_release/", recursive = TRUE, showWarnings = FALSE)
output_folder <- "output/post_release"

# Load data --------------------------------------------------------------------
print("Load model output")

# List all CSV files matching the pattern
file_list <- list.files(
  path = table2,
  pattern = "^table2-cohort_.*\\.csv$",
  full.names = TRUE
)

# read, add cohort column, and combine
df <- file_list %>%
  lapply(function(f) {
    df <- read_csv(f)
    cohort <- str_match(basename(f), "^table2-cohort_(.*)\\.csv$")[, 2]
    df %>% mutate(cohort = cohort)
  }) %>%
  bind_rows()

readr::write_csv(df, paste0(output_folder, "/table2_raw.csv"), na = "-")

# Load data --------------------------------------------------------------------
print("Load data")

df <- readr::read_csv(
  "output/post_release/table2_raw.csv",
  show_col_types = FALSE
)

perpeople_cumu <- 1000
perpeople_mean <- 1000

# Filter outcomes to whole population; all practices; unplanned apc and/or ec due to all causes or any acsc conditions
df <- df %>%
  filter(
    group == "main",
    strata == "Overall",
    acsc_condition %in% c("-", "any")
  ) %>%
  mutate(
    outcome_name = str_remove(outcome_name, "_mp6.*")
  ) %>%
  select(
    -group,
    -source,
    -apc_plan_status,
    -acsc,
    -acsc_condition,
    -midpoint6,
    -stat,
    -strata,
    -prop_zero
  )

# Add readable labels --------------------------------------------------------------
print("Add readable labels")

labels <- readr::read_csv("lib/labels.csv", show_col_types = FALSE)

df <- merge(
  df,
  labels,
  by.x = "outcome_name",
  by.y = "term",
  all.x = TRUE
)
df <- dplyr::rename(df, "outcome_label" = "label")

group_order <- c(
  "cumulative hospital use",
  "cumulative ACSC-related hospital use",
  "average weekly hospital use",
  "average weekly ACSC-related hospital use"
)

df <- df %>%
  mutate(
    group = factor(group, levels = group_order)
  )

df_table2 <- df %>%
  mutate(
    median = case_when(
      str_detect(group, "cumulative") ~ median * perpeople_cumu,
      str_detect(group, "average") ~ median * perpeople_mean,
      TRUE ~ median * 1000
    ),
    q1 = case_when(
      str_detect(group, "cumulative") ~ q1 * perpeople_cumu,
      str_detect(group, "average") ~ q1 * perpeople_mean,
      TRUE ~ q1 * 1000
    ),
    q3 = case_when(
      str_detect(group, "cumulative") ~ q3 * perpeople_cumu,
      str_detect(group, "average") ~ q3 * perpeople_mean,
      TRUE ~ q3 * 1000
    ),
    mean = case_when(
      str_detect(group, "cumulative") ~ mean * perpeople_cumu,
      str_detect(group, "average") ~ mean * perpeople_mean,
      TRUE ~ mean * 1000
    ),
    sd = case_when(
      str_detect(group, "cumulative") ~ sd * perpeople_cumu,
      str_detect(group, "average") ~ sd * perpeople_mean,
      TRUE ~ sd * 1000
    )
  ) %>%
  mutate(
    `Median (IQR)` = sprintf(
      "%.1f (%.1f-%.1f)",
      median,
      q1,
      q3
    ),
    `Mean (SD)` = sprintf(
      "%.1f (%.1f)",
      mean,
      sd
    )
  ) %>%
  select(
    group,
    ref,
    outcome_label,
    cohort,
    n_practices,
    "Median (IQR)",
    "Mean (SD)"
  )

df_table2 <- df_table2 %>%
  mutate(
    cohort = factor(
      cohort,
      levels = c("precovid", "postcovid1", "postcovid2", "postcovid3"),
      labels = c(
        "Pre-COVID",
        "Post-COVID 1",
        "Post-COVID 2",
        "Post-COVID 3"
      )
    )
  )

df_table2_wide <- df_table2 %>%
  pivot_wider(
    names_from = cohort,
    values_from = c("n_practices", "Median (IQR)", "Mean (SD)"),
    names_glue = "{.value} [{cohort}]"
  ) %>%
  arrange(group, ref)

# # Add distribution of region and practice counts ------------------------------------------------------------

# region_df <- df %>%
#     filter(
#         strata != "Overall",
#         !str_detect(strata, "^strata_"),
#         category == "list_size"
#     )

# total_practices <- df %>%
#     filter(strata == "Overall", category == "list_size") %>%
#     select(cohort, n_practices_midpoint6) %>%
#     distinct()

# df_n_practice_row <- total_practices %>%
#     mutate(
#         group = "N",
#         category_label = "Number of practices",
#         value = as.character(n_practices_midpoint6),
#         cohort = factor(
#             cohort,
#             levels = c("precovid", "postcovid1", "postcovid2", "postcovid3"),
#             labels = c(
#                 "Pre-COVID",
#                 "Post-COVID 1",
#                 "Post-COVID 2",
#                 "Post-COVID 3"
#             )
#         )
#     ) %>%
#     select(group, category_label, cohort, value) %>%
#     pivot_wider(
#         names_from = cohort,
#         values_from = value,
#         names_glue = "Median (IQR) [{cohort}]"
#     )

# df_region_wide <- region_df %>%
#     select(
#         cohort,
#         category_label = strata,
#         n_practices_midpoint6
#     ) %>%
#     distinct(category_label, cohort, .keep_all = TRUE) %>%
#     left_join(
#         total_practices,
#         by = "cohort",
#         suffix = c("", "_total")
#     ) %>%
#     mutate(
#         value = sprintf(
#             "%d (%.1f%%)",
#             n_practices_midpoint6,
#             100 * n_practices_midpoint6 / n_practices_midpoint6_total
#         )
#     ) %>%
#     select(
#         category_label,
#         cohort,
#         value
#     ) %>%
#     mutate(
#         cohort = factor(
#             cohort,
#             levels = c("precovid", "postcovid1", "postcovid2", "postcovid3"),
#             labels = c(
#                 "Pre-COVID",
#                 "Post-COVID 1",
#                 "Post-COVID 2",
#                 "Post-COVID 3"
#             )
#         )
#     ) %>%
#     pivot_wider(
#         names_from = cohort,
#         values_from = value,
#         names_glue = "Median (IQR) [{cohort}]"
#     ) %>%
#     mutate(
#         group = "Region"
#     ) %>%
#     mutate(
#         category_label = factor(
#             category_label,
#             levels = region_order
#         )
#     ) %>%
#     arrange(category_label)

# df_table2_wide <- bind_rows(
#     df_n_practice_row,
#     df_region_wide,
#     df_table2_wide
# )

df_table2_wide <- df_table2_wide %>%
  select(
    group,
    outcome_label,
    "n_practices [Pre-COVID]",
    "Median (IQR) [Pre-COVID]",
    "Mean (SD) [Pre-COVID]",
    "n_practices [Post-COVID 1]",
    "Median (IQR) [Post-COVID 1]",
    "Mean (SD) [Post-COVID 1]",
    "n_practices [Post-COVID 2]",
    "Median (IQR) [Post-COVID 2]",
    "Mean (SD) [Post-COVID 2]",
    "n_practices [Post-COVID 3]",
    "Median (IQR) [Post-COVID 3]",
    "Mean (SD) [Post-COVID 3]"
  )

readr::write_csv(df_table2_wide, paste0(output_folder, "/table2.csv"), na = "-")
