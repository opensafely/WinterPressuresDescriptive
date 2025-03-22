# Load libraries ---------------------------------------------------------------
library(tidyverse)
library(yaml)
library(here)
library(glue)
library(readr)
library(dplyr)

# Specify defaults -------------------------------------------------------------
defaults_list <- list(
  version = "3.0",
  expectations = list(population_size = 1000L)
)

# Define cohorts and cohort start dates
cohorts <- c("precovid", "postcovid1", "postcovid2")
cohort_dates <- list(
  precovid = "2018-10-01",
  postcovid1 = "2022-10-01",
  postcovid2 = "2023-10-01"
)

# Define subgroups
cs_args <- c("Age", "Sex", "Ethnicity", "IMD", "Rurality", "Smoking", "Multimorbidity")
long_args <- c("Consultation", "ec", "apc", "ec_ACSCs", "apc_ACSCs")

# Create generic action function -----------------------------------------------

action <- function(
    name,
    run,
    dummy_data_file      = NULL,
    arguments            = NULL,
    needs                = NULL,
    highly_sensitive     = NULL,
    moderately_sensitive = NULL
){
  # Only append arguments to run if not NULL
  run_full <- if (!is.null(arguments)) {
    paste0(run, "\n  ", paste(arguments, collapse = "\n  "))
  } else {
    run
  }
  outputs <- list(
    moderately_sensitive = moderately_sensitive,
    highly_sensitive     = highly_sensitive
  )
  outputs[sapply(outputs, is.null)] <- NULL

  actions <- list(
    run             = run_full,
    dummy_data_file = dummy_data_file,
    needs           = needs,
    outputs         = outputs
  )
  actions[sapply(actions, is.null)] <- NULL

  action_list        <- list(name = actions)
  names(action_list) <- name

  action_list
}

# Create generic comment function ----------------------------------------------

comment <- function(...) {
  list_comments <- list(...)
  comments      <- map(list_comments, ~paste0("## ", ., " ##"))
  comments
}


# Create function to convert comment "actions" in a yaml string into proper comments

convert_comment_actions <- function(yaml.txt) {
  yaml.txt %>%
    str_replace_all("\\\n(\\s*)\\'\\'\\:(\\s*)\\'", "\n\\1")  %>%
    #str_replace_all("\\\n(\\s*)\\'", "\n\\1") %>%
    str_replace_all("([^\\'])\\\n(\\s*)\\#\\#", "\\1\n\n\\2\\#\\#") %>%
    str_replace_all("\\#\\#\\'\\\n", "\n")
}

# Add cohort-specific measure actions ------------------------------------------
generate_cohort <- function(cohort) {
  date <- cohort_dates[[cohort]]  # extract date for the cohort
  splice(
    comment(glue("Generate cohort - {cohort}")),
    action(
      name  = glue("generate_cohort_{cohort}"),
      run   = glue("ehrql:v1 generate-dataset analysis/dataset_definition/measures_cohorts.py --output output/dataset_definition/input_{cohort}.csv.gz"),
      needs = list("study_dates"),
      arguments = c("--", "--patient_measures", glue("--start_cohort {date}")),
      highly_sensitive = list(
        dataset = glue("output/dataset_definition/input_{cohort}.csv.gz")
      )
    )
  )
}
# Start building the actions list ----------------------------------------------
actions_list <- c(
  
  ## Post YAML disclaimer ------------------------------------------------------
  comment("# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #",
          "DO NOT EDIT project.yaml DIRECTLY",
          "This file is created by create_project_actions.R",
          "Edit and run create_project_actions.R to update the project.yaml",
          "# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #"
  ),
  
  ## Define study dates --------------------------------------------------------
  comment("Define study dates"),
  
  action(
    name = "study_dates",
    run  = "r:latest analysis/dataset_definition/study_dates.R",
    highly_sensitive = list(
      study_dates_json = "output/dataset_definition/study_dates.json"
    )
  )
)

# Add cohort generation actions ------------------------------------------------
for (cohort in cohorts) {
  actions_list <- c(actions_list, generate_cohort(cohort))
}

# Add measure generation actions -----------------------------------------------
measure_actions <- list()

for (cohort in cohorts) {
  date <- cohort_dates[[cohort]]
  
  for (flag in cs_args) {
    comment_text <- glue("Generate measures for {flag} (cross-sectional) - {cohort}")
    name <- glue("generate_measures_{cohort}_{date}_{tolower(flag)}")
    file <- glue("output/measures/measures_{tolower(flag)}_{cohort}.csv.gz")
    arguments <- c(
      "--", 
      "--practice_measures", 
      "--CS", 
      glue("--{flag}"), 
      glue("--start_cohort {date}")
    )
    
    act <- c(
      comment(comment_text),
      action(
        name = name,
        run = glue("ehrql:v1 generate-measures analysis/dataset_definition/measures_cohorts.py --output {file}"),
        arguments = arguments,
        highly_sensitive = list(
          dataset = file)
      )
    )
    measure_actions <- append(measure_actions, act)
  }
  
  for (flag in long_args) {
    comment_text <- glue("Generate measures for {flag} (longitudinal) - {cohort}")
    name <- glue("generate_measures_{cohort}_{date}_{tolower(flag)}")
    file <- glue("output/measures/measures_{tolower(flag)}_{cohort}.csv.gz")
    arguments <- c(
      "--", 
      "--practice_measures", 
      "--Long", 
      glue("--{flag}"), 
      glue("--start_cohort {date}")
    )
    
    act <- c(
      comment(comment_text),
      action(
        name = name,
        run = glue("ehrql:v1 generate-measures analysis/dataset_definition/measures_cohorts.py --output {file}"),
        arguments = arguments,
        highly_sensitive = list(
          dataset = file)
      )
    )
    measure_actions <- append(measure_actions, act)
  }
}

# Append measure actions to main action list -----------------------------------
actions_list <- c(actions_list, measure_actions)

# Combine actions into project list --------------------------------------------

project_list <- splice(
  defaults_list,
  list(actions = actions_list)
)

# Convert list to yaml, reformat, and output a .yaml file ----------------------

as.yaml(project_list, indent = 2) %>%
  # convert comment actions to comments
  convert_comment_actions() %>%
  # add one blank line before level 1 and level 2 keys
  str_replace_all("\\\n(\\w)", "\n\n\\1") %>%
  str_replace_all("\\\n\\s\\s(\\w)", "\n\n  \\1") %>%
  writeLines("project.yaml")

# Return number of actions -----------------------------------------------------

count_run_elements <- function(x) {

  if (!is.list(x)) {
    return(0)
  }

  # Check if any names of this list are "run"
  current_count <- sum(names(x) == "run", na.rm = TRUE)

  # Recursively check all elements in the list
  return(current_count + sum(sapply(x, count_run_elements)))

}

print(paste0("YAML created with ", count_run_elements(actions_list), " actions."))