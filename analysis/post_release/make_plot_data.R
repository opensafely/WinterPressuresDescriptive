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
  bind_rows()

df_lr <- file_list_lr %>%
  lapply(read_csv, show_col_types = FALSE) %>%
  bind_rows()

readr::write_csv(df, paste0(output_folder, "/plot_model_output.csv"))
readr::write_csv(df_lr, paste0(output_folder, "/plot_model_output_lr.csv"))