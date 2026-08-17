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

# List all poisson/negbin CSV files
file_list <- list.files(
  path = table3,
  pattern = "^model_output-.*-midpoint6\\.csv$",
  full.names = TRUE
)

# List all LR test CSV files
file_list_lr <- list.files(
  path = table3,
  pattern = "^model_output_lrtest.*-midpoint6\\.csv$",
  full.names = TRUE
)

# Read and combine all CSV files into one data frame
df <- file_list %>%
  lapply(read_csv, show_col_types = FALSE) %>%
  bind_rows() %>%
  distinct()

df <- df %>%
  mutate(
    exposure = if_else(
      str_detect(exposure, fixed(";")),
      str_remove(term, "^exp_(num|cat)_"),
      exposure,
      missing = exposure
    )
  )

df_lr <- file_list_lr %>%
  lapply(read_csv, show_col_types = FALSE) %>%
  bind_rows()

# Add MAD for each exposure
df_mad <- readr::read_csv(
  "output/post_release/table1_raw.csv",
  show_col_types = FALSE
)
# Add exposure name matching plot_model_output
df_mad <- df_mad %>%
  mutate(
    exposure = paste(characteristic, subcharacteristic, sep = "_"),
    exposure = str_remove(exposure, "_True$"),
    exposure = str_remove(exposure, "_mp6$")
  ) %>%
  filter(strata == "Overall") %>%
  mutate(
    mad = case_when(
      exposure == "list_size" ~ sprintf("%d", round(mad)),
      exposure == "cons_mean" ~ sprintf("%.1f", mad * 1000),
      TRUE ~ sprintf("%.1f", mad * 100)
    )
  ) %>%
  select(exposure, mad, cohort) %>%
  distinct(exposure, cohort, .keep_all = TRUE)

df <- df %>%
  left_join(
    df_mad,
    by = c("exposure", "cohort")
  )

readr::write_csv(df, paste0(output_folder, "/plot_model_output.csv"))
readr::write_csv(df_lr, paste0(output_folder, "/plot_model_output_lr.csv"))
