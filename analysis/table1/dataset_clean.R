# Load libraries --------------------------------------------------------------
print('Load libraries')

library(dplyr)
library(tidyverse)
library(lubridate)
library(data.table)
library(readr)
library(jsonlite)
library(here)
library(fs)
library(base)
library(stats)

# Define clean dataset output folder -------------------------------------------
print("Creating output/dataset_clean output folder")

dataclean_dir <- "output/dataset_clean/"
dir_create(here::here(dataclean_dir))

file_path <- paste0("output/dataset_definition/input_", cohort, ".csv.gz")


# Load cohort dataset ----
print('Load cohort dataset')

input <- read_csv(file_path, col_types = col_classes)
message(paste0(
"Dataset has been read successfully with N = ",
nrow(input),
" rows"
))

