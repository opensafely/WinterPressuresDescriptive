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
        cohort_clean = glue(
          "output/dataset_clean/input_{cohort}_clean.rds"
        ),
        icc_input = glue(
          "output/dataset_clean/icc_input-{cohort}.dta"
        ),
        cohort_clean_sensitivity = glue(
          "output/dataset_clean/input_{cohort}_clean_sensitivity.rds"
        ),
        icc_input_sensitivity = glue(
          "output/dataset_clean/icc_input-{cohort}_sensitivity.dta"
        )
      ),
      moderately_sensitive = list(
        flow = glue(
          "output/dataset_clean/flow-cohort_{cohort}.csv"
        ),
        flow_midpoint6 = glue(
          "output/dataset_clean/flow-cohort_{cohort}-midpoint6.csv"
        )
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

# Generate Table 1 -------------------------------------------------------------
generate_table1 <- function(cohort, input_type = "main") {
  output_suffix <- if (input_type == "main") {
    ""
  } else {
    paste0("_", input_type)
  }

  splice(
    comment(glue(
      "Generate Table 1 summary statistics - {cohort} - {input_type}"
    )),
    action(
      name = glue("generate_table1_{cohort}_{input_type}"),
      run = glue(
        "r:v2 analysis/table1/table1.R {cohort} {input_type}"
      ),
      needs = list(
        glue("generate_input_{cohort}_clean")
      ),
      moderately_sensitive = list(
        table1 = glue(
          "output/table1/table1-cohort_{cohort}{output_suffix}.csv"
        ),
        table1_midpoint6 = glue(
          "output/table1/table1-cohort_{cohort}{output_suffix}-midpoint6.csv"
        )
      )
    )
  )
}


# Generate Table 2 -------------------------------------------------------------
generate_table2 <- function(cohort, input_type = "main") {
  output_suffix <- if (input_type == "main") {
    ""
  } else {
    paste0("_", input_type)
  }

  splice(
    comment(glue(
      "Generate Table 2 summary statistics - {cohort} - {input_type}"
    )),
    action(
      name = glue("generate_table2_{cohort}_{input_type}"),
      run = glue(
        "r:v2 analysis/table2/table2.R {cohort} {input_type}"
      ),
      needs = list(
        glue("generate_input_{cohort}_clean")
      ),
      moderately_sensitive = list(
        table2 = glue(
          "output/table2/table2-cohort_{cohort}{output_suffix}.csv"
        ),
        table2_midpoint6 = glue(
          "output/table2/table2-cohort_{cohort}{output_suffix}-midpoint6.csv"
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
  # Divide patient case-mix exposures into two output groups --------------------
  case_mix1_exposures <- active_analyses %>%
    filter(
      exposure_group == "case_mix",
      str_detect(
        exposure,
        "^(age_|sex_|ethnicity_)"
      )
    ) %>%
    pull(exposure) %>%
    unique()

  case_mix2_exposures <- active_analyses %>%
    filter(
      exposure_group == "case_mix",
      !exposure %in% case_mix1_exposures
    ) %>%
    pull(exposure) %>%
    unique()

  # Select analyses for the cohort -------------------------------------------

  selected_analyses <- active_analyses %>%
    filter(
      .data$cohort == .env$cohort
    )

  # Select exposure group -----------------------------------------------------

  if (exposure_group == "case_mix1") {
    selected_analyses <- selected_analyses %>%
      filter(
        .data$exposure_group == "case_mix",
        .data$exposure %in% case_mix1_exposures
      )
  } else if (exposure_group == "case_mix2") {
    selected_analyses <- selected_analyses %>%
      filter(
        .data$exposure_group == "case_mix",
        .data$exposure %in% case_mix2_exposures
      )
  } else {
    selected_analyses <- selected_analyses %>%
      filter(
        .data$exposure_group == .env$exposure_group
      )
  }

  # Select subgroup -----------------------------------------------------------

  if (subgroup != "all") {
    selected_analyses <- selected_analyses %>%
      filter(
        .data$analysis == .env$subgroup
      )
  }

  # Check that analyses were selected ----------------------------------------

  if (nrow(selected_analyses) == 0) {
    stop(
      paste0(
        "No active analyses found for cohort = ",
        cohort,
        ", subgroup = ",
        subgroup,
        ", exposure_group = ",
        exposure_group
      )
    )
  }

  splice(
    comment(glue("Generate model_output {cohort} - {exposure_group}_characteristic - {subgroup}")),
    action(
      name = glue(
        "make_model_output-{cohort}-{subgroup}-{exposure_group}"
      ),
      run = glue(
        "r:v2 analysis/make_output/make_model_output.R {cohort} {subgroup} {exposure_group}"
      ),
      needs = as.list(
        paste0(
          "run_regression_model-",
          selected_analyses$name
        )
      ),
      moderately_sensitive = list(
        model_output_regression = glue(
          "output/make_output/model_output-{cohort}-subgroup_{subgroup}-exposure_{exposure_group}.csv"
        ),
        model_output_lrtest = paste0(
          "output/make_output/",
          glue(
            "model_output_lrtest-{cohort}-subgroup_{subgroup}-exposure_{exposure_group}.csv"
          )
        ),
        model_output_regression_midpoint6 = glue(
          "output/make_output/model_output-{cohort}-subgroup_{subgroup}-exposure_{exposure_group}-midpoint6.csv"
        ),
        model_output_lrtest_midpoint6 = paste0(
          "output/make_output/",
          glue(
            "model_output_lrtest-{cohort}-subgroup_{subgroup}-exposure_{exposure_group}-midpoint6.csv"
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
  actions_list <- c(actions_list, generate_table1(cohort, "sensitivity_consultation"))
  actions_list <- c(actions_list, generate_table2(cohort, "sensitivity_consultation"))
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

for (cohort in cohorts_all) {
  for (subgroup in c(subgroups_short)) {
    actions_list <- c(
      actions_list,
      make_model_output(cohort, subgroup, "practice")
    )
  }
}

for (exposure_group in c("case_mix1", "case_mix2")) {
  for (cohort in cohorts_all) {
    for (subgroup in c(subgroups_short)) {
      actions_list <- c(
        actions_list,
        make_model_output(cohort, subgroup, exposure_group)
      )
    }
  }
}

for (cohort in cohorts_all) {
  actions_list <- c(
    actions_list,
    make_model_output(cohort, "all", "all")
  )
}

# Add action to generate correlation figures for exposures
# Add actions to generate exposure-correlation outputs ------------------------
for (cohort in cohorts_all) {
  actions_list <- c(
    actions_list,
    comment(
      glue("Generate exposure-correlation outputs - {cohort}")
    ),
    action(
      name = glue("generate_exposure_correlations_{cohort}"),
      run = glue(
        "r:v2 analysis/graphs/correlations_exposures.R {cohort}"
      ),
      needs = list(
        glue("generate_input_{cohort}_clean")
      ),
      moderately_sensitive = list(
        # Domain-specific heatmaps
        heatmap_practice = glue(
          "output/correlations/{cohort}/",
          "heatmap_practice_{cohort}.png"
        ),
        heatmap_age_and_sex = glue(
          "output/correlations/{cohort}/",
          "heatmap_age_and_sex_{cohort}.png"
        ),
        heatmap_ethnicity = glue(
          "output/correlations/{cohort}/",
          "heatmap_ethnicity_{cohort}.png"
        ),
        heatmap_deprivation = glue(
          "output/correlations/{cohort}/",
          "heatmap_deprivation_{cohort}.png"
        ),
        heatmap_other_health = glue(
          "output/correlations/{cohort}/",
          "heatmap_other_health_{cohort}.png"
        ),
        heatmap_smoking = glue(
          "output/correlations/{cohort}/",
          "heatmap_smoking_{cohort}.png"
        ),

        # Domain-specific correlation matrices
        correlations_practice = glue(
          "output/correlations/{cohort}/",
          "correlations_practice_{cohort}.csv"
        ),
        correlations_age_and_sex = glue(
          "output/correlations/{cohort}/",
          "correlations_age_and_sex_{cohort}.csv"
        ),
        correlations_ethnicity = glue(
          "output/correlations/{cohort}/",
          "correlations_ethnicity_{cohort}.csv"
        ),
        correlations_deprivation = glue(
          "output/correlations/{cohort}/",
          "correlations_deprivation_{cohort}.csv"
        ),
        correlations_other_health = glue(
          "output/correlations/{cohort}/",
          "correlations_other_health_{cohort}.csv"
        ),
        correlations_smoking = glue(
          "output/correlations/{cohort}/",
          "correlations_smoking_{cohort}.csv"
        ),

        # Overall correlation outputs
        heatmap_all_exposures = glue(
          "output/correlations/{cohort}/",
          "heatmap_all_exposures_{cohort}.png"
        ),
        correlations_all_exposures = glue(
          "output/correlations/{cohort}/",
          "correlations_all_exposures_{cohort}.csv"
        ),
        heatmap_mutually_adjusted_exposures = glue(
          "output/correlations/{cohort}/",
          "heatmap_mutually_adjusted_exposures_{cohort}.png"
        ),
        correlations_mutually_adjusted_exposures = glue(
          "output/correlations/{cohort}/",
          "correlations_mutually_adjusted_exposures_{cohort}.csv"
        ),
        vif_mutually_adjusted_exposures = glue(
          "output/correlations/{cohort}/",
          "vif_mutually_adjusted_exposures_{cohort}.csv"
        ),
        high_correlation_pairs = glue(
          "output/correlations/{cohort}/",
          "high_correlation_pairs_07_{cohort}.csv"
        ),
        pairwise_n_all_exposures = glue(
          "output/correlations/{cohort}/",
          "pairwise_n_all_exposures_{cohort}.csv"
        ),
        pairwise_n_all_exposures_midpoint6 = glue(
          "output/correlations/{cohort}/",
          "pairwise_n_all_exposures_{cohort}-midpoint6.csv"
        ),
        correlation_pairs_all_exposures = glue(
          "output/correlations/{cohort}/",
          "correlation_pairs_all_exposures_{cohort}.csv"
        )
      )
    )
  )
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
