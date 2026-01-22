# Load packages ----------------------------------------------------------------
print('Load packages')

library(magrittr)
library(dplyr)
library(readr)
library(plyr)
library(fs)
library(here)

# Source common functions ------------------------------------------------------
print('Source common functions')

source("analysis/utility.R")

# Specify arguments ------------------------------------------------------------
print("Specify arguments")

args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
    cohort <- "precovid"
    subgroup <- "main"
} else {
    cohort <- args[[1]]
    subgroup <- args[[2]]
}

# Define model output folder ---------------------------------------
print("Creating output/model output folder")

# setting up the sub directory
makeout_dir <- "output/make_output/"
model_dir <- "output/model/"

# check if sub directory exists, create if not
fs::dir_create(here::here(makeout_dir))

# Load active analyses ---------------------------------------------------------
print('Load active analyses')

active_analyses <- readr::read_rds("lib/active_analyses.rds")

# List available regression model outputs -----------------------------------------------
print('List available regression model outputs')

files_reg <- list.files(
    model_dir,
    pattern = "^model_output_(poisson|negbin)-.*\\.csv$",
    full.names = FALSE
)

# Combine regression model output  -------------------------------------------------------
print('Combine model output')

df_reg <- NULL

for (i in files_reg) {
    ## Load model output
    print('Load model output')

    tmp <- readr::read_csv(paste0(model_dir, i))

    ## Add model type (poisson/negbin)
    tmp$model_type <- sub("^model_output_([^\\-]+)-.*", "\\1", i)

    ## Add source file name
    tmp$name <- gsub(
        "^model_output_(poisson|negbin)-",
        "",
        gsub("\\.csv$", "", i)
    )

    ## Add source column (Stata)
    tmp$source <- "Stata"

    ## Append to master dataframe
    df_reg <- plyr::rbind.fill(df_reg, tmp)
}

# Add details from active analyses ---------------------------------------------
print('Add details from active analyses')

df_reg <- merge(
  df_reg,
  active_analyses[, c("name", "cohort", "exposure", "outcome", "analysis")],
  by = "name",
  all.x = TRUE
)

if (any(is.na(df_reg$cohort))) {
  warning(
    "Some model outputs did not match active_analyses$name:\n",
    paste(unique(df_reg$name[is.na(df_reg$cohort)]), collapse = "\n")
  )
}

# Save model output ------------------------------------------------------------
print('Save model output')

df_reg <- df_reg[, c(
    "name",
    "cohort",
    "analysis",
    "exposure",
    "outcome",
    "model_type",
    "model",
    "term",
    "irr",
    "lci",
    "uci",
    "se_coef",
    "p_value",
    "ri_variance",
    "ri_se",
    "ri_lci",
    "ri_uci",
    "ri_lb",
    "ri_ub",
    "n_obs",
    "aic",
    "bic",
    "source",
    "error"
)]

readr::write_csv(
    df_reg,
    paste0(makeout_dir, "model_output-", cohort, "-", subgroup, ".csv")
)

# List available LR test model outputs -----------------------------------------------
print('List available LR test model outputs')
files_lrtest <- list.files(model_dir, pattern = "model_output_lrtest-")

# Combine LR test model output  -------------------------------------------------------
print('Combine LR test model output')

df_lrtest <- NULL

for (i in files_lrtest) {
    ## Load model output
    print('Load model output')

    tmp_lr <- readr::read_csv(paste0(model_dir, i))

    ## Add source file name
    tmp_lr$name <- gsub(
        "^model_output_lrtest-",
        "",
        gsub("\\.csv$", "", i)
    )

    ## Add source column (Stata)
    tmp_lr$source <- "Stata"

    ## Append to master dataframe
    df_lrtest <- plyr::rbind.fill(df_lrtest, tmp_lr)
}

# Add details from active analyses ---------------------------------------------
print('Add details from active analyses')

df_lrtest <- merge(
  df_lrtest,
  active_analyses[, c("name", "cohort", "exposure", "outcome", "analysis")],
  by = "name",
  all.x = TRUE
)

if (any(is.na(df_lrtest$cohort))) {
  warning(
    "Some model outputs did not match active_analyses$name:\n",
    paste(unique(df_lrtest$name[is.na(df_lrtest$cohort)]), collapse = "\n")
  )
}

# Save LR test model output ------------------------------------------------------------
print('Save LR test model output')

df_lrtest <- df_lrtest[, c(
    "name",
    "cohort",
    "analysis",
    "exposure",
    "outcome",
    "model",
    "term",
    "chi2_lr",
    "p_lr",
    "n_obs"
)]

readr::write_csv(
    df_lrtest,
    paste0(makeout_dir, "model_output_lrtest-", cohort, "-", subgroup, ".csv")
)


# Perform redaction ------------------------------------------------------------
print('Perform redaction')

df_reg$n_obs_midpoint6 <- roundmid_num(df_reg$n_obs, to = 6)

df_reg[, c("n_obs")] <- NULL

df_lrtest$n_obs_midpoint6 <- roundmid_num(df_lrtest$n_obs, to = 6)

df_lrtest[, c("n_obs")] <- NULL

# Save model output ------------------------------------------------------------
print('Save model output')

readr::write_csv(
    df_reg,
    paste0(
        makeout_dir,
        "model_output-",
        cohort,
        "-",
        subgroup,
        "-midpoint6.csv"
    )
)

# Save LR test model output ------------------------------------------------------------
print('Save LR test model output')

readr::write_csv(
    df_lrtest,
    paste0(
        makeout_dir,
        "model_output_lrtest-",
        cohort,
        "-",
        subgroup,
        "-midpoint6.csv"
    )
)
