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

# Load data --------------------------------------------------------------------
print("Load model output")

# List all CSV files matching the pattern
file_list <- list.files(
  path = table1,
  pattern = "^table1-cohort_.*\\.csv$",
  full.names = TRUE
)

# read, add cohort column, and combine
df <- file_list %>%
  lapply(function(f) {
    df <- read_csv(f)
    cohort <- str_match(basename(f), "^table1-cohort_(.*)\\.csv$")[, 2]
    df %>% mutate(cohort = cohort)
  }) %>%
  bind_rows()

readr::write_csv(df, paste0(output_folder, "/table1.csv"), na = "-")


# Load data --------------------------------------------------------------------
print("Load data")

df <- readr::read_csv(
  "output/post_release/table1.csv",
  show_col_types = FALSE
)

df <- df %>%
  mutate(category = paste(characteristic, subcharacteristic, sep = "_")) %>%
  select(-characteristic, -subcharacteristic)

# Add readable labels --------------------------------------------------------------
print("Add readable labels")

labels <- readr::read_csv("lib/labels.csv", show_col_types = FALSE)

df <- merge(
  df,
  labels,
  by.x = "category",
  by.y = "term",
  all.x = TRUE
)
df <- dplyr::rename(df, "category_label" = "label")

group_order <- c(
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

region_order <- c(
  "East",
  "East Midlands",
  "London",
  "North East",
  "North West",
  "South East",
  "South West",
  "West Midlands",
  "Yorkshire and The Humber",
  "Unknown"
)

rurality_order <- c(
  "Urban conurbation",
  "Urban town",
  "Rural"
)

df <- df %>%
  mutate(
    group = factor(group, levels = group_order)
  )

df_table1 <- df %>%
  filter(
    strata == "Overall",
    group != "Consultations"
  ) %>%
  mutate(
    across(
      c(
        median,
        q1,
        q3,
        matches("^p(10|20|30|40|50|60|70|80|90)$")
      ),
      ~ case_when(
        group == "List size" ~ .x,
        group == "Monthly consultation" ~ .x * 1000,
        TRUE ~ .x * 100
      )
    )
  )

readr::write_csv(
  df_table1,
  paste0(output_folder, "/table1_plot_data.csv"),
  na = "-"
)

df_table1 <- df_table1 %>%
  mutate(
    mean = case_when(
      group == "List size" ~ mean,
      group == "Monthly consultation" ~ mean * 1000,
      TRUE ~ mean * 100
    ),
    sd = case_when(
      group == "List size" ~ sd,
      group == "Monthly consultation" ~ sd * 1000,
      TRUE ~ sd * 100
    )
  ) %>%
  mutate(
    `Median (IQR)` = case_when(
      group == "List size" ~ sprintf(
        "%d (%d-%d)",
        round(median),
        round(q1),
        round(q3)
      ),
      group == "Monthly consultation" ~ sprintf(
        "%.1f (%.1f-%.1f)",
        median,
        q1,
        q3
      ),
      TRUE ~ sprintf(
        "%.2f (%.2f-%.2f)",
        median,
        q1,
        q3
      )
    ),
    `Mean (SD)` = case_when(
      group == "List size" ~ sprintf(
        "%d (%d)",
        round(mean),
        round(sd)
      ),
      group == "Monthly consultation" ~ sprintf(
        "%.1f (%.1f)",
        mean,
        sd
      ),
      TRUE ~ sprintf(
        "%.2f (%.2f)",
        mean,
        sd
      )
    ),
    `Median (MAD)` = case_when(
      group == "List size" ~ sprintf(
        "%d (%d)",
        round(median),
        round(mad)
      ),
      group == "Monthly consultation" ~ sprintf(
        "%.1f (%.1f)",
        median,
        mad * 1000
      ),
      TRUE ~ sprintf(
        "%.2f (%.2f)",
        median,
        mad * 100
      )
    )
  ) %>%
  select(
    group,
    ref,
    category_label,
    cohort,
    "Median (IQR)",
    "Mean (SD)",
    "Median (MAD)"
  )

df_table1 <- df_table1 %>%
  mutate(
    cohort = factor(
      cohort,
      levels = c("precovid", "postcovid1", "postcovid2", "postcovid3"),
      labels = c(
        "Pre-COVID 19",
        "Oct 2022 - Feb 2023",
        "Oct 2023 - Feb 2024",
        "Oct 2024 - Feb 2025"
      )
    )
  )

df_table1_wide <- df_table1 %>%
  pivot_wider(
    names_from = cohort,
    values_from = c("Median (IQR)", "Mean (SD)", "Median (MAD)"),
    names_glue = "{.value} [{cohort}]"
  ) %>%
  arrange(group, ref)

# Add distribution of practice region/rurality and practice counts ------------------------------------------------------------

practice_cat_lookup <- tibble(
  category_label = c(region_order, rurality_order),
  group = c(
    rep("Region", length(region_order)),
    rep("Rurality", length(rurality_order))
  ),
  order = c(
    seq_along(region_order),
    seq_along(rurality_order)
  )
)

practice_cat_df <- df %>%
  filter(
    strata %in% practice_cat_lookup$category_label,
    category == "list_size_mp6"
  ) %>%
  select(
    cohort,
    category_label = strata,
    n_practices_midpoint6
  ) %>%
  left_join(practice_cat_lookup, by = "category_label")

total_practices <- df %>%
  filter(strata == "Overall", category == "list_size_mp6") %>%
  select(cohort, n_practices_midpoint6) %>%
  distinct()

df_n_practice_row <- total_practices %>%
  mutate(
    group = "N",
    category_label = "Number of practices",
    value = as.character(n_practices_midpoint6),
    cohort = factor(
      cohort,
      levels = c("precovid", "postcovid1", "postcovid2", "postcovid3"),
      labels = c(
        "Pre-COVID 19",
        "Oct 2022 - Feb 2023",
        "Oct 2023 - Feb 2024",
        "Oct 2024 - Feb 2025"
      )
    )
  ) %>%
  select(group, category_label, cohort, value) %>%
  pivot_wider(
    names_from = cohort,
    values_from = value,
    names_glue = "Median (IQR) [{cohort}]"
  )

df_practivce_cat_wide <- practice_cat_df %>%
  select(
    cohort,
    group,
    order,
    category_label,
    n_practices_midpoint6
  ) %>%
  distinct(category_label, cohort, .keep_all = TRUE) %>%
  left_join(
    total_practices,
    by = "cohort",
    suffix = c("", "_total")
  ) %>%
  mutate(
    value = sprintf(
      "%d (%.1f%%)",
      n_practices_midpoint6,
      100 * n_practices_midpoint6 /
        n_practices_midpoint6_total
    )
  ) %>%
  select(
    group,
    order,
    category_label,
    cohort,
    value
  ) %>%
  mutate(
    cohort = factor(
      cohort,
      levels = c("precovid", "postcovid1", "postcovid2", "postcovid3"),
      labels = c(
        "Pre-COVID 19",
        "Oct 2022 - Feb 2023",
        "Oct 2023 - Feb 2024",
        "Oct 2024 - Feb 2025"
      )
    )
  ) %>%
  pivot_wider(
    names_from = cohort,
    values_from = value,
    names_glue = "Median (IQR) [{cohort}]"
  ) %>%
  arrange(group, order) %>%
  select(-order)

# Add distribution of rurality and practice counts ------------------------------------------------------------


df_table1_wide <- bind_rows(
  df_n_practice_row,
  df_practivce_cat_wide,
  df_table1_wide
)

df_table1_wide <- df_table1_wide %>%
  select(
    group,
    category_label,
    "Median (IQR) [Pre-COVID 19]",
    "Mean (SD) [Pre-COVID 19]",
    "Median (MAD) [Pre-COVID 19]",
    "Median (IQR) [Oct 2022 - Feb 2023]",
    "Mean (SD) [Oct 2022 - Feb 2023]",
    "Median (MAD) [Oct 2022 - Feb 2023]",
    "Median (IQR) [Oct 2023 - Feb 2024]",
    "Mean (SD) [Oct 2023 - Feb 2024]",
    "Median (MAD) [Oct 2023 - Feb 2024]",
    "Median (IQR) [Oct 2024 - Feb 2025]",
    "Mean (SD) [Oct 2024 - Feb 2025]",
    "Median (MAD) [Oct 2024 - Feb 2025]"
  )

readr::write_csv(df_table1_wide, paste0(output_folder, "/table1.csv"), na = "-")
