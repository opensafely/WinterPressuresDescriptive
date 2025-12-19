# Load packages ----------------------------------------------------------------
print("Load packages")

library(magrittr)
library(data.table)
library(stringr)
library(lubridate)

# Source functions -------------------------------------------------------------
print("Source functions")

lapply(
  list.files("analysis/model", full.names = TRUE, pattern = "fn-"),
  source
)

# Specify arguments ------------------------------------------------------------
print("Specify arguments")

args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
  name <- "cohort_precovid-main-ethnicity_white-apc_unpl"
} else {
  name <- args[[1]]
}

# Define model output folder ---------------------------------------
print("Creating output/model output folder")

# setting up the sub directory
model_dir <- "output/model/"

# check if sub directory exists, create if not
fs::dir_create(here::here(model_dir))

# Prepare data by selecting model relevant columns ------------------------
model_input <- prepare_model_input(name = name)

# Check vital covariates ------------------------------------------------------
model_input <- check_vitals(model_input)

# Save prepared model input ----------------------------------------------------
print("Save prepared model input")

foreign::write.dta(model_input, paste0(model_dir, "model_input-", name, ".dta"))