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
  path = table1,
  pattern = "^table1-.*-midpoint6\\.csv$",
  full.names = TRUE
)

# read, add cohort column, and combine
df <- file_list %>%
  lapply(function(f) {
    df <- read_csv(f)
    cohort <- str_match(basename(f), "table1-cohort_(.*)-midpoint6")[, 2]
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
  "Age",
  "Sex",
  "Ethnicity",
  "Deprivation",
  "Rurality",
  "Smoking Status",
  "Obesity",
  "Care home residence",
  "Monthly consultation"
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
    median = case_when(
      group == "List size" ~ median_midpoint6,
      group == "Monthly consultation" ~ median_midpoint6 * 1000,
      TRUE ~ median_midpoint6 * 100
    ),
    q1 = case_when(
      group == "List size" ~ q1_midpoint6,
      group == "Monthly consultation" ~ q1_midpoint6 * 1000,
      TRUE ~ q1_midpoint6 * 100
    ),
    q3 = case_when(
      group == "List size" ~ q3_midpoint6,
      group == "Monthly consultation" ~ q3_midpoint6 * 1000,
      TRUE ~ q3_midpoint6 * 100
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
      group == "List size" ~ mean_midpoint6,
      group == "Monthly consultation" ~ mean_midpoint6 * 1000,
      TRUE ~ mean_midpoint6 * 100
    ),
    sd = case_when(
      group == "List size" ~ sd_midpoint6,
      group == "Monthly consultation" ~ sd_midpoint6 * 1000,
      TRUE ~ sd_midpoint6 * 100
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
    )
  ) %>%
  select(
    group,
    ref,
    category_label,
    cohort,
    "Median (IQR)",
    "Mean (SD)"
  )

df_table1 <- df_table1 %>%
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

df_table1_wide <- df_table1 %>%
  pivot_wider(
    names_from = cohort,
    values_from = c("Median (IQR)", "Mean (SD)"),
    names_glue = "{.value} [{cohort}]"
  ) %>%
  arrange(group, ref)

# Add distribution of region and practice counts ------------------------------------------------------------

region_df <- df %>%
  filter(
    strata != "Overall",
    !str_detect(strata, "^strata_"),
    category == "list_size"
  )

total_practices <- df %>%
  filter(strata == "Overall", category == "list_size") %>%
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
        "Pre-COVID",
        "Post-COVID 1",
        "Post-COVID 2",
        "Post-COVID 3"
      )
    )
  ) %>%
  select(group, category_label, cohort, value) %>%
  pivot_wider(
    names_from = cohort,
    values_from = value,
    names_glue = "Median (IQR) [{cohort}]"
  )

df_region_wide <- region_df %>%
  select(
    cohort,
    category_label = strata,
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
      100 * n_practices_midpoint6 / n_practices_midpoint6_total
    )
  ) %>%
  select(
    category_label,
    cohort,
    value
  ) %>%
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
  ) %>%
  pivot_wider(
    names_from = cohort,
    values_from = value,
    names_glue = "Median (IQR) [{cohort}]"
  ) %>%
  mutate(
    group = "Region"
  ) %>%
  mutate(
    category_label = factor(
      category_label,
      levels = region_order
    )
  ) %>%
  arrange(category_label)

df_table1_wide <- bind_rows(
  df_n_practice_row,
  df_region_wide,
  df_table1_wide
)

df_table1_wide <- df_table1_wide %>%
  select(
    group,
    category_label,
    "Median (IQR) [Pre-COVID]",
    "Mean (SD) [Pre-COVID]",
    "Median (IQR) [Post-COVID 1]",
    "Mean (SD) [Post-COVID 1]",
    "Median (IQR) [Post-COVID 2]",
    "Mean (SD) [Post-COVID 2]",
    "Median (IQR) [Post-COVID 3]",
    "Mean (SD) [Post-COVID 3]"
  )

readr::write_csv(df_table1_wide, paste0(output_folder, "/table1.csv"), na = "-")
