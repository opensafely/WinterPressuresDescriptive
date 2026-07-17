# Load libraries ---------------------------------------------------------------
library(tidyverse)
library(yaml)
library(here)
library(glue)
library(readr)
library(dplyr)

# Specify defaults -------------------------------------------------------------
defaults_list <- list(
  version = "5.0"
)

active_analyses <- read_rds("lib/active_analyses.rds")

# Define cohorts and cohort start dates
cohorts_all <- unique(active_analyses$cohort)
cohorts_postcovid <- unique(
  active_analyses$cohort[grepl("^post", active_analyses$cohort)]
)

# Define exposure groups (practice vs case-mix)
exposure_groups <- unique(active_analyses$exposure_group)

cohort_dates <- active_analyses |>
  dplyr::select(cohort, outcome_start) |>
  dplyr::distinct() |>
  tibble::deframe()


# Define subgroups (This is consistent with the study definition measure generation, see /analysis/dataset_definition/config_setup.py)
subgroups <- c(
  "sub_asthma",
  "sub_copd",
  "sub_hypertension",
  "sub_diabetes",
  "sub_sev_mental_ill"
)

subgroups_short <- unique(active_analyses$analysis)

# Define arguments for measure generation actions
cs_args <- c(
  "Age",
  "Sex",
  "Ethnicity",
  "IMD",
  "Rurality",
  "Carehome",
  "Smoking",
  "Obesity",
  "Multimorbidity"
)

long_args_postcovid <- c("vax_covid")

long_args_outcomes <- c(
  "ec_all",
  "apc_all",
  "ec_ACSCs",
  "apc_ACSCs"
)

long_args_all <- c(
  long_args_postcovid,
  "vax_flu",
  "vax_pneum",
  "Consultation",
  long_args_outcomes
)

# Define regression categories [only for the old codes when unadjsuted and adjusted models were run separately in different actions]
covariates_all <- c("unadjusted", "adjusted")

# Create generic action function -----------------------------------------------

action <- function(
  name,
  run,
  dummy_data_file = NULL,
  arguments = NULL,
  needs = NULL,
  highly_sensitive = NULL,
  moderately_sensitive = NULL
) {
  # Only append arguments to run if not NULL
  run_full <- if (!is.null(arguments)) {
    paste0(run, "\n  ", paste(arguments, collapse = "\n  "))
  } else {
    run
  }
  outputs <- list(
    moderately_sensitive = moderately_sensitive,
    highly_sensitive = highly_sensitive
  )
  outputs[sapply(outputs, is.null)] <- NULL

  actions <- list(
    run = run_full,
    dummy_data_file = dummy_data_file,
    needs = needs,
    outputs = outputs
  )
  actions[sapply(actions, is.null)] <- NULL

  action_list <- list(name = actions)
  names(action_list) <- name

  action_list
}

# Create generic comment function ----------------------------------------------

comment <- function(...) {
  list_comments <- list(...)
  comments <- map(list_comments, ~ paste0("## ", ., " ##"))
  comments
}


# Create function to convert comment "actions" in a yaml string into proper comments

convert_comment_actions <- function(yaml.txt) {
  yaml.txt %>%
    str_replace_all("\\\n(\\s*)\\'\\'\\:(\\s*)\\'", "\n\\1") %>%
    # str_replace_all("\\\n(\\s*)\\'", "\n\\1") %>%
    str_replace_all("([^\\'])\\\n(\\s*)\\#\\#", "\\1\n\n\\2\\#\\#") %>%
    str_replace_all("\\#\\#\\'\\\n", "\n")
}

# Add cohort-specific measure actions ------------------------------------------
generate_cohort <- function(cohort) {
  date <- cohort_dates[[cohort]] # extract date for the cohort
  splice(
    comment(glue("Generate cohort - {cohort}")),
    action(
      name = glue("generate_cohort_{cohort}"),
      run = glue(
        "ehrql:v1 generate-dataset analysis/dataset_definition/measures_cohorts.py --output output/dataset_definition/input_{cohort}.csv.gz"
      ),
      needs = list("study_dates"),
      arguments = c("--", "--patient_measures", glue("--start_cohort {date}")),
      highly_sensitive = list(
        dataset = glue("output/dataset_definition/input_{cohort}.csv.gz")
      )
    )
  )
}

# Generate cleaned input
generate_input_clean <- function(cohort) {
  splice(
    comment(glue("Generate cleaned input dataset - {cohort}")),
    action(
      name = glue("generate_input_{cohort}_clean"),
      run = glue("r:v2 analysis/dataset_clean/dataset_clean.R {cohort}"),
      needs = list(
        glue("generate_cohort_{cohort}"),
        glue("generate_merged_{cohort}")
      ),
      highly_sensitive = list(
        cohort_clean = glue("output/dataset_clean/input_{cohort}_clean.rds"),
        icc_input = glue("output/dataset_clean/icc_input-{cohort}.dta")
      )
    )
  )
}

# Generate icc_outcome output
generate_icc_outcome <- function(cohort) {
  splice(
    comment(glue("Generate icc_outcome - {cohort}")),
    action(
      name = glue("generate_icc_outcome_{cohort}"),
      run = glue("stata-mp:v1 analysis/icc/icc_outcome.do {cohort}"),
      needs = list(
        glue("generate_input_{cohort}_clean")
      ),
      moderately_sensitive = list(
        icc_outcome = glue("output/icc_outcome/icc_outcome-{cohort}.csv")
      )
    )
  )
}

# Generate input for trajectory outcome graphs
generate_input_trajectory_outcomes <- function() {
  splice(
    comment("Generate input for trajectory outcome graphs"),
    action(
      name = "generate_input_trajectory_outcomes",
      run = "r:v2 analysis/graphs/trajectory_outcomes.R",
      needs = as.list(glue("generate_input_{cohorts_all}_clean")),
      moderately_sensitive = list(
        input_trajectory_outcomes =
          "output/graphs/input_trajectory_outcomes.csv"
      )
    )
  )
}

# Generate Table 1
generate_table1 <- function(cohort) {
  splice(
    comment(glue("Generate Table 1 summary statistics - {cohort}")),
    action(
      name = glue("generate_table1_{cohort}"),
      run = glue("r:v2 analysis/table1/table1.R {cohort}"),
      needs = list(
        glue("generate_input_{cohort}_clean")
      ),
      moderately_sensitive = list(
        table1_midpoint6_TRUE = glue(
          "output/table1/table1-cohort_{cohort}-midpoint6.csv"
        ),
        table1_midpoint6_FALSE = glue(
          "output/table1/table1-cohort_{cohort}.csv"
        )
      )
    )
  )
}

# Generate Table 2
generate_table2 <- function(cohort) {
  splice(
    comment(glue("Generate Table 2 summary statistics - {cohort}")),
    action(
      name = glue("generate_table2_{cohort}"),
      run = glue("r:v2 analysis/table2/table2.R {cohort}"),
      needs = list(
        glue("generate_input_{cohort}_clean")
      ),
      moderately_sensitive = list(
        table2_midpoint6_TRUE = glue(
          "output/table2/table2-cohort_{cohort}-midpoint6.csv"
        ),
        table2_midpoint6_FALSE = glue(
          "output/table2/table2-cohort_{cohort}.csv"
        )
      )
    )
  )
}

# Create function to run a model -----------------------------------------------
apply_model_function <- function(
  name,
  cohort
) {
  splice(
    action(
      name = glue("make_model_input-{name}"),
      run = glue("r:v2 analysis/model/make_model_input.R {name}"),
      needs = as.list(glue("generate_input_{cohort}_clean")),
      highly_sensitive = list(
        model_input = glue("output/model/model_input-{name}.dta")
      )
    ),
    action(
      name = glue("run_regression_model-{name}"),
      run = glue("stata-mp:v1 analysis/model/regression_model.do {name}"),
      needs = c(as.list(glue("make_model_input-{name}"))),
      moderately_sensitive = list(
        model_output_poisson = glue(
          "output/model/model_output_poisson-{name}.csv"
        ),
        model_output_negbin = glue(
          "output/model/model_output_negbin-{name}.csv"
        ),
        model_output_lrtest = glue(
          "output/model/model_output_lrtest-{name}.csv"
        )
      )
    )
  )
}

# Create function for making model outputs --------------------------------------

make_model_output <- function(cohort, subgroup, exposure_group) {
  splice(
    comment(glue("Generate model_output for {cohort} - {subgroup} - {exposure_group}")),
    action(
      name = glue(
        "make_model_output-{cohort}-{subgroup}-{exposure_group}"
      ),
      run = glue(
        "r:v2 analysis/make_output/make_model_output.R {cohort} {subgroup} {exposure_group}"
      ),
      needs = as.list(c(
        paste0(
          "run_regression_model-",
          active_analyses$name[
            active_analyses$cohort == cohort &
              active_analyses$analysis == subgroup &
              active_analyses$exposure_group == exposure_group
          ]
        )
      )),
      moderately_sensitive = list(
        model_output_regression = glue(
          "output/make_output/model_output-{cohort}-{subgroup}-{exposure_group}.csv"
        ),
        model_output_lrtest = paste0(
          "output/make_output/",
          glue(
            "model_output_lrtest-{cohort}-{subgroup}-{exposure_group}.csv"
          )
        ),
        model_output_regression_midpoint6 = glue(
          "output/make_output/model_output-{cohort}-{subgroup}-{exposure_group}-midpoint6.csv"
        ),
        model_output_lrtest_midpoint6 = paste0(
          "output/make_output/",
          glue(
            "model_output_lrtest-{cohort}-{subgroup}-{exposure_group}-midpoint6.csv"
          )
        )
      )
    )
  )
}

# Start building the actions list ----------------------------------------------
actions_list <- c(
  ## Post YAML disclaimer ------------------------------------------------------
  comment(
    "# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #",
    "DO NOT EDIT project.yaml DIRECTLY",
    "This file is created by create_project_actions.R",
    "Edit and run create_project_actions.R to update the project.yaml",
    "# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #"
  ),

  ## Define study dates --------------------------------------------------------
  comment("Define study dates"),
  action(
    name = "study_dates",
    run = "r:v2 analysis/dataset_definition/study_dates.R",
    highly_sensitive = list(
      study_dates_json = "output/dataset_definition/study_dates.json"
    )
  )
)

# Add cohort generation actions ------------------------------------------------
for (cohort in cohorts_all) {
  actions_list <- c(actions_list, generate_cohort(cohort))
}

# Add measure generation actions -----------------------------------------------
measure_actions <- list()

for (flag in cs_args) {
  for (cohort in cohorts_all) {
    date <- cohort_dates[[cohort]]
    comment_text <- glue(
      "Generate measures for {flag} (cross-sectional) - {cohort}"
    )
    name <- glue("generate_measures_{cohort}_{date}_{tolower(flag)}")
    file <- glue("output/measures/measures_{tolower(flag)}_{cohort}.csv")
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
        run = glue(
          "ehrql:v1 generate-measures analysis/dataset_definition/measures_cohorts.py --output {file}"
        ),
        arguments = arguments,
        moderately_sensitive = list(
          dataset = file
        )
      )
    )
    measure_actions <- append(measure_actions, act)
  }
}

for (flag in long_args_all) {
  cohorts <- if (flag %in% long_args_postcovid) {
    cohorts_postcovid
  } else {
    cohorts_all
  }
  for (cohort in cohorts) {
    date <- cohort_dates[[cohort]]
    if (flag %in% long_args_outcomes) {
      comment_text <- glue(
        "Generate measures for {flag} - {cohort} - main"
      )
      name <- glue("generate_measures_{cohort}_{date}-main-{tolower(flag)}")
      file <- glue("output/measures/measures_{tolower(flag)}_{cohort}_main.csv")
    } else {
      comment_text <- glue(
        "Generate measures for {flag} (longitudinal) - {cohort}"
      )
      name <- glue("generate_measures_{cohort}_{date}-{tolower(flag)}")
      file <- glue("output/measures/measures_{tolower(flag)}_{cohort}.csv")
    }

    arguments <- c(
      "--",
      "--practice_measures",
      "--Long_all",
      glue("--{flag}"),
      glue("--start_cohort {date}")
    )

    act <- c(
      comment(comment_text),
      action(
        name = name,
        run = glue(
          "ehrql:v1 generate-measures analysis/dataset_definition/measures_cohorts.py --output {file}"
        ),
        arguments = arguments,
        moderately_sensitive = list(
          dataset = file
        )
      )
    )
    measure_actions <- append(measure_actions, act)
  }
}

for (subgroup in subgroups) {
  for (flag in long_args_outcomes) {
    for (cohort in cohorts_all) {
      date <- cohort_dates[[cohort]]
      comment_text <- glue(
        "Generate measures for {flag} - {cohort} - {subgroup}"
      )
      name <- glue(
        "generate_measures_{cohort}_{date}-{subgroup}-{tolower(flag)}"
      )
      file <- glue(
        "output/measures/measures_{tolower(flag)}_{cohort}_{subgroup}.csv"
      )
      arguments <- c(
        "--",
        "--practice_measures",
        glue("--Long_{subgroup}"),
        glue("--{flag}"),
        glue("--start_cohort {date}")
      )

      act <- c(
        comment(comment_text),
        action(
          name = name,
          run = glue(
            "ehrql:v1 generate-measures analysis/dataset_definition/measures_cohorts.py --output {file}"
          ),
          arguments = arguments,
          moderately_sensitive = list(
            dataset = file
          )
        )
      )
      measure_actions <- append(measure_actions, act)
    }
  }
}
# Append measure actions to main action list -----------------------------------
actions_list <- c(actions_list, measure_actions)

# Add action to check + merge the .csvs generated by the measure actions
for (cohort in cohorts_all) {
  date <- cohort_dates[[cohort]] # Pull in the date for the cohort

  # Define a list of all "generate_measures" actions for the relevant cohort
  # This is what goes into the needs argument in the .yaml action
  measure_action_names <- names(measure_actions)[names(measure_actions) != ""] # Remove all list elements that are empty strings
  generate_measures_list <- measure_action_names[str_detect(
    measure_action_names,
    glue("^generate_measures_{cohort}_")
  )]

  # Actually defining the action to run the analysis/datset_clean/measures_merge.R script for each cohort
  check_and_merge_action <- c(
    comment(glue(
      "Check measures files & generate merged datasets for cohort: {cohort}"
    )),
    action(
      name = glue("generate_merged_{cohort}"),
      run = glue(
        "r:v2 analysis/dataset_clean/measures_merge.R {cohort} {date}"
      ),
      needs = generate_measures_list,
      moderately_sensitive = list(
        dataset1 = glue("output/dataset_clean/merged_data_long_{cohort}.csv")
      )
    )
  )
  # Append check_and_merge actions to main action list
  actions_list <- c(actions_list, check_and_merge_action)
}

# Append input_clean + Table 1 + Table 2 actions -------------------------------------------
for (cohort in cohorts_all) {
  actions_list <- c(actions_list, generate_input_clean(cohort))
  actions_list <- c(actions_list, generate_table1(cohort))
  actions_list <- c(actions_list, generate_table2(cohort))
  actions_list <- c(actions_list, generate_icc_outcome(cohort))
}
actions_list <- c(actions_list, generate_input_trajectory_outcomes())

# Run models for all active analyses ----------------------------------------------
actions_list <- c(
  actions_list,
  comment("Run regression models for all active analyses")
)

run_models_action <- lapply(
  1:nrow(active_analyses),
  function(x) {
    apply_model_function(
      name = active_analyses$name[x],
      cohort = active_analyses$cohort[x]
    )
  }
)

# Append run models action to main action list -------------------------------
actions_list <- c(
  actions_list,
  unlist(run_models_action, recursive = FALSE)
)

# Generate model outputs for all cohort-subgroup combinations -----------------
for (subgroup in c(subgroups_short)) {
  for (cohort in cohorts_all) {
    for (exposure_group in exposure_groups) {
      actions_list <- c(
        actions_list,
        make_model_output(cohort, subgroup, exposure_group)
      )
    }
  }
}

# Add action to generate correlation figures for exposures
for (cohort in cohorts_all) {
  generate_exposure_correlations <- c(
    comment(glue("Generates exposure correlation figures - {cohort}")),
    action(
      name = glue("generate_exposure_correlation_figures_{cohort}"),
      run = glue("r:v2 analysis/graphs/correlations_exposures.R {cohort}"),
      needs = list(glue("generate_input_{cohort}_clean")),
      moderately_sensitive = list(
        heatmap_age = glue("output/correlations/heatmap_age_{cohort}.png"),
        heatmap_sex = glue("output/correlations/heatmap_sex_{cohort}.png"),
        heatmap_eth = glue(
          "output/correlations/heatmap_ethnicity_{cohort}.png"
        ),
        heatmap_imd = glue("output/correlations/heatmap_imd_{cohort}.png"),
        heatmap_rur = glue("output/correlations/heatmap_rurality_{cohort}.png"),
        heatmap_smk = glue("output/correlations/heatmap_smoking_{cohort}.png"),
        heatmap_cons = glue(
          "output/correlations/heatmap_consultation_{cohort}.png"
        ),
        heatmap_morb = glue(
          "output/correlations/heatmap_morbidity_{cohort}.png"
        ),
        heatmap_all = glue(
          "output/correlations/heatmap_all_exposures_{cohort}.png"
        ),
        corr_tab_age = glue(
          "output/correlations/correlations_age_{cohort}.csv"
        ),
        corr_tab_sex = glue(
          "output/correlations/correlations_sex_{cohort}.csv"
        ),
        corr_tab_eth = glue(
          "output/correlations/correlations_ethnicity_{cohort}.csv"
        ),
        corr_tab_imd = glue(
          "output/correlations/correlations_imd_{cohort}.csv"
        ),
        corr_tab_rur = glue(
          "output/correlations/correlations_rurality_{cohort}.csv"
        ),
        corr_tab_smk = glue(
          "output/correlations/correlations_smoking_{cohort}.csv"
        ),
        corr_tab_cons = glue(
          "output/correlations/correlations_consultation_{cohort}.csv"
        ),
        corr_tab_morb = glue(
          "output/correlations/correlations_morbidity_{cohort}.csv"
        ),
        scatter_cons = glue(
          "output/correlations/scatter_cons_sep_vs_mean_{cohort}.png"
        )
      )
    )
  )

  # Appending action to the list of all actions for this .yaml
  actions_list <- c(actions_list, generate_exposure_correlations)
}

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

print(paste0(
  "YAML created with ",
  count_run_elements(actions_list),
  " actions."
))
