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
cohorts_postcovid <- c("postcovid1", "postcovid2", "postcovid3")
cohorts_all <- c("precovid", cohorts_postcovid)

cohort_dates <- list(
  precovid = "2018-10-01",
  postcovid1 = "2022-10-01",
  postcovid2 = "2023-10-01",
  postcovid3 = "2024-10-01"
)

# Define subgroups
cs_args <- c(
  "Age",
  "Sex",
  "Ethnicity",
  "IMD",
  "Rurality",
  "Smoking",
  "Obesity",
  "Multimorbidity"
)

long_args_postcovid <- c("vax_covid")

long_args_all <- c(
  long_args_postcovid,
  "vax_flu",
  "vax_pneum",
  "Consultation",
  "ec_all",
  "apc_all",
  "ec_ACSCs",
  "apc_ACSCs"
)

#Define regression categories
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
    #str_replace_all("\\\n(\\s*)\\'", "\n\\1") %>%
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
      run = glue("r:latest analysis/table1/dataset_clean.R {cohort}"),
      needs = list(
        glue("generate_cohort_{cohort}"),
        glue("generate_merged_{cohort}")
      ),
      moderately_sensitive = list(
        cohort_clean = glue("output/dataset_clean/input_{cohort}_clean.csv")
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
      run = glue("r:latest analysis/table1/table1.R {cohort}"),
      needs = list(
        glue("generate_input_{cohort}_clean")
      ),
      moderately_sensitive = list(
        table1 = glue("output/table1/table1-cohort_{cohort}.csv"),
        table1_midpoint6 = glue(
          "output/table1/table1-cohort_{cohort}-midpoint6.csv"
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
    run = "r:latest analysis/dataset_definition/study_dates.R",
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
    comment_text <- glue(
      "Generate measures for {flag} (longitudinal) - {cohort}"
    )
    name <- glue("generate_measures_{cohort}_{date}_{tolower(flag)}")
    file <- glue("output/measures/measures_{tolower(flag)}_{cohort}.csv")
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

# Append measure actions to main action list -----------------------------------
actions_list <- c(actions_list, measure_actions)


# Add action to check + merge the .csvs generated by the measure actions
for (cohort in cohorts_all) {
  date <- cohort_dates[[cohort]] #Pull in the date for the cohort

  #Define a list of all "generate_measures" actions for the relevant cohort
  #This is what goes into the needs argument in the .yaml action
  measure_action_names <- names(measure_actions)[names(measure_actions) != ""] #Remove all list elements that are empty strings
  generate_measures_list <- measure_action_names[str_detect(
    measure_action_names,
    glue("^generate_measures_{cohort}_")
  )]

  #Actually defining the action to run the data_cleaning.R script for each cohort
  check_and_merge_action <- c(
    comment(glue(
      "Check measures files & generate merged datasets for cohort: {cohort}"
    )),
    action(
      name = glue("generate_merged_{cohort}"),
      run = glue("r:latest analysis/data_cleaning.R {cohort} {date}"),
      needs = generate_measures_list,
      moderately_sensitive = list(
        dataset1 = glue("output/analytic_data_long_{cohort}.csv"),
        dataset2 = glue("output/analytic_data_wide_{cohort}.csv")
      )
    )
  )
  #Append check_and_merge actions to main action list
  actions_list <- c(actions_list, check_and_merge_action)
}
# Append input_clean + Table 1 actions -------------------------------------------
for (cohort in cohorts_all) {
  actions_list <- c(actions_list, generate_input_clean(cohort))
  actions_list <- c(actions_list, generate_table1(cohort))
}
#Add action: generate the tables used for the descriptive outcome graphs
for (cohort in cohorts_all) {
  date <- cohort_dates[[cohort]] #Pull in the date for the cohort

  #Defining the action
  table_for_output_graphs <- c(
    comment(glue("Generates dataset for the outcome graphs, cohort: {cohort}")),
    action(
      name = glue("generate_table_for_outcome_graph_{cohort}"),
      run = glue(
        "stata-mp:latest analysis/figures_graphs_out.do {cohort} {date}"
      ),
      needs = list(glue("generate_merged_{cohort}")),
      moderately_sensitive = list(
        dataset1 = glue(
          "output/temp_figure1/temp_mp6_prop_out_vars_{cohort}.csv"
        ),
        dataset2 = glue(
          "output/temp_figure1/temp_mp6_prop_u5y_out_vars_{cohort}.csv"
        ),
        dataset3 = glue(
          "output/temp_figure1/temp_mp6_prop_65_74_out_vars_{cohort}.csv"
        ),
        dataset4 = glue(
          "output/temp_figure1/temp_mp6_prop_75_79_out_vars_{cohort}.csv"
        ),
        dataset5 = glue(
          "output/temp_figure1/temp_mp6_prop_80_84_out_vars_{cohort}.csv"
        ),
        dataset6 = glue(
          "output/temp_figure1/temp_mp6_prop_85p_out_vars_{cohort}.csv"
        ),
        dataset7 = glue(
          "output/temp_figure1/temp_mp6_prop_asian_out_vars_{cohort}.csv"
        ),
        dataset8 = glue(
          "output/temp_figure1/temp_mp6_prop_black_out_vars_{cohort}.csv"
        ),
        dataset9 = glue(
          "output/temp_figure1/temp_mp6_prop_white_out_vars_{cohort}.csv"
        ),
        dataset10 = glue(
          "output/temp_figure1/temp_mp6_prop_imd1_out_vars_{cohort}.csv"
        ),
        dataset11 = glue(
          "output/temp_figure1/temp_mp6_prop_imd2_out_vars_{cohort}.csv"
        ),
        dataset12 = glue(
          "output/temp_figure1/temp_mp6_prop_ast_out_vars_{cohort}.csv"
        ),
        dataset13 = glue(
          "output/temp_figure1/temp_mp6_prop_dbts_out_vars_{cohort}.csv"
        ),
        dataset14 = glue(
          "output/temp_figure1/temp_mp6_prop_hypt_out_vars_{cohort}.csv"
        ),
        dataset15 = glue(
          "output/temp_figure1/temp_mp6_prop_obs_out_vars_{cohort}.csv"
        ),
        dataset16 = glue(
          "output/temp_figure1/temp_mp6_prop_urb1_out_vars_{cohort}.csv"
        ),
        dataset17 = glue(
          "output/temp_figure1/temp_mp6_prop_urb2_out_vars_{cohort}.csv"
        ),
        dataset18 = glue(
          "output/temp_figure1/temp_mp6_prop_female_out_vars_{cohort}.csv"
        ),
        dataset19 = glue(
          "output/temp_figure1/temp_mp6_prop_smoker_out_vars_{cohort}.csv"
        ),
        dataset20 = glue(
          "output/temp_figure1/temp_mp6_md_out_vars_{cohort}.csv"
        ),
        dataset21 = glue(
          "output/temp_figure1/temp_mp6_md_u5y_out_vars_{cohort}.csv"
        ),
        dataset22 = glue(
          "output/temp_figure1/temp_mp6_md_65_74_out_vars_{cohort}.csv"
        ),
        dataset23 = glue(
          "output/temp_figure1/temp_mp6_md_75_79_out_vars_{cohort}.csv"
        ),
        dataset24 = glue(
          "output/temp_figure1/temp_mp6_md_80_84_out_vars_{cohort}.csv"
        ),
        dataset25 = glue(
          "output/temp_figure1/temp_mp6_md_85p_out_vars_{cohort}.csv"
        ),
        dataset26 = glue(
          "output/temp_figure1/temp_mp6_md_asian_out_vars_{cohort}.csv"
        ),
        dataset27 = glue(
          "output/temp_figure1/temp_mp6_md_black_out_vars_{cohort}.csv"
        ),
        dataset28 = glue(
          "output/temp_figure1/temp_mp6_md_white_out_vars_{cohort}.csv"
        ),
        dataset29 = glue(
          "output/temp_figure1/temp_mp6_md_imd1_out_vars_{cohort}.csv"
        ),
        dataset30 = glue(
          "output/temp_figure1/temp_mp6_md_imd2_out_vars_{cohort}.csv"
        ),
        dataset31 = glue(
          "output/temp_figure1/temp_mp6_md_ast_out_vars_{cohort}.csv"
        ),
        dataset32 = glue(
          "output/temp_figure1/temp_mp6_md_dbts_out_vars_{cohort}.csv"
        ),
        dataset33 = glue(
          "output/temp_figure1/temp_mp6_md_hypt_out_vars_{cohort}.csv"
        ),
        dataset34 = glue(
          "output/temp_figure1/temp_mp6_md_obs_out_vars_{cohort}.csv"
        ),
        dataset35 = glue(
          "output/temp_figure1/temp_mp6_md_urb1_out_vars_{cohort}.csv"
        ),
        dataset36 = glue(
          "output/temp_figure1/temp_mp6_md_urb2_out_vars_{cohort}.csv"
        ),
        dataset37 = glue(
          "output/temp_figure1/temp_mp6_md_female_out_vars_{cohort}.csv"
        ),
        dataset38 = glue(
          "output/temp_figure1/temp_mp6_md_smoker_out_vars_{cohort}.csv"
        ),
        dataset39 = glue("output/figure1/mp6_md_out_apc_all_{cohort}.csv"),
        dataset40 = glue("output/figure1/mp6_prop_out_apc_all_{cohort}.csv"),
        dataset41 = glue("output/figure1/mp6_md_out_ec_all_{cohort}.csv"),
        dataset42 = glue("output/figure1/mp6_prop_out_ec_all_{cohort}.csv")
      )
    )
  )
  #Appending action to the list of all actions for this .yaml
  actions_list <- c(actions_list, table_for_output_graphs)
}
#Add action to generate the histograms of the outcome
for (cohort in cohorts_all) {
  outcome_histograms <- c(
    comment(glue("Generates histograms of the outcome, cohort: {cohort}")),
    action(
      name = glue("generate_outcome_histograms_{cohort}"),
      run = glue(
        "stata-mp:latest analysis/figures_graphs_outcome_histogram.do {cohort} {date}"
      ),
      needs = list(glue("generate_merged_{cohort}")),
      moderately_sensitive = list(
        histogram1 = glue("output/figure1/apc_hg_{cohort}.svg"),
        histogram2 = glue("output/figure1/apc_hg_acscs_{cohort}.svg"),
        histogram3 = glue("output/figure1/ec_hg_{cohort}.svg"),
        histogram4 = glue("output/figure1/ec_hg_acscs_{cohort}.svg")
      )
    )
  )
  #Appending action to the list of all actions for this .yaml
  actions_list <- c(actions_list, outcome_histograms)
}


#Add action to generate correlation figures for exposures
for (cohort in cohorts_all) {
  generate_exposure_correlations <- c(
    comment(glue("Generates exposure correlation figures - {cohort}")),
    action(
      name = glue("generate_exposure_correlation_figures_{cohort}"),
      run = glue("r:latest analysis/graphs/correlations_exposures.R {cohort}"),
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

  #Appending action to the list of all actions for this .yaml
  actions_list <- c(actions_list, generate_exposure_correlations)
}

# Combine actions into project list --------------------------------------------
project_list <- splice(
  defaults_list,
  list(actions = actions_list)
)
#Add action to generate the variables for the outcome decile graphs
for (cohort in cohorts_all){
  out_dec_vars <- c(
    comment(glue("Generates variables for the outcome decile graphs, cohort: {cohort}")),
    action(
      name = glue("generate_out_dec_vars_{cohort}"),
      run = glue("stata-mp:latest analysis/f1_out_dec_variables.do {cohort} {date}"),
      needs = list(glue("generate_merged_{cohort}")), 
      moderately_sensitive = list(
        out_dec_data_long_csv = glue("output/f1_out_dec/out_dec_data_long_{cohort}.csv")
      )
    )
  )
  #Appending action to the list of all actions for this .yaml 
  actions_list <- c(actions_list, out_dec_vars)
} 
#Add action to generate the data + graphs for the SIMPLE outcome decile plots by week
  out_dec_week_simple_graphs <- c(
    comment(glue("Generates the SIMPLE outcome decile graphs by week")),
    action(
      name = glue("generate_out_dec_week_simple_graphs"),
      run = glue("stata-mp:latest analysis/f1_out_dec_week_simple_graphs.do"),
      needs = list(
        glue("generate_out_dec_vars_precovid"),
        glue("generate_out_dec_vars_postcovid1"),
        glue("generate_out_dec_vars_postcovid2"),
        glue("generate_out_dec_vars_postcovid3")
      ),
      moderately_sensitive = list(
        out_dec_week_simple_all_csv = glue("output/f1_out_dec/out_dec_week_simple_all.csv"),
        graph_all_cond = glue("output/f1_out_dec/x_all_cond.svg"),
        graph_ang = glue("output/f1_out_dec/x_ang.svg"),
        graph_ast = glue("output/f1_out_dec/x_ast.svg"),
        graph_copd = glue("output/f1_out_dec/x_copd.svg"),
        graph_dbts = glue("output/f1_out_dec/x_dbts.svg"),
        graph_hypt = glue("output/f1_out_dec/x_hypt.svg")
      )
    )
  )
  #Appending action to the list of all actions for this .yaml 
  actions_list <- c(actions_list, out_dec_week_simple_graphs ) 
  
#Add action to generate the data + graphs for the CUMULATIVE outcome decile plots by week
  out_dec_week_cumulative_graphs <- c(
    comment(glue("Generates the CUMULATIVE outcome decile graphs by week")),
    action(
      name = glue("generate_out_dec_week_cumulative_graphs"),
      run = glue("stata-mp:latest analysis/f1_out_dec_week_cumulative_graphs.do"),
      needs = list(
        glue("generate_out_dec_vars_precovid"),
        glue("generate_out_dec_vars_postcovid1"),
        glue("generate_out_dec_vars_postcovid2"),
        glue("generate_out_dec_vars_postcovid3")
      ),
      moderately_sensitive = list(
        out_dec_week_simple_all_csv = glue("output/f1_out_dec/out_dec_week_cumulative_all.csv"),
        graph_all_cond = glue("output/f1_out_dec/c_all_cond.svg"),
        graph_ang = glue("output/f1_out_dec/c_ang.svg"),
        graph_ast = glue("output/f1_out_dec/c_ast.svg"),
        graph_copd = glue("output/f1_out_dec/c_copd.svg"),
        graph_dbts = glue("output/f1_out_dec/c_dbts.svg"),
        graph_hypt = glue("output/f1_out_dec/c_hypt.svg")
      )
    )
  )
  #Appending action to the list of all actions for this .yaml 
  actions_list <- c(actions_list, out_dec_week_cumulative_graphs)
  
#Add action to generate the model variances 
  model_variance <- c(
    comment(glue("Runs simple regressions to estimate practice variance ")),
    action(
      name = glue("generate_model_variance"),
      run = glue("stata-mp:latest analysis/simple_model_variance.do"),
      needs = list(
        glue("generate_merged_precovid"),
        glue("generate_merged_postcovid1"),
        glue("generate_merged_postcovid2"),
        glue("generate_merged_postcovid3")
      ),
      moderately_sensitive = list(
        out_dec_week_simple_all_csv = glue("output/model_variance/model_variance.csv")
      )
    )
  )
  #Appending action to the list of all actions for this .yaml 
  actions_list <- c(actions_list, model_variance)  

#Add action to generate a descriptive table for the outcome vars 
  outcome_summary <- c(
    comment(glue("Generate a descriptive table for the outcome vars")),
    action(
      name = glue("generate_outcome_summary"),
      run = glue("stata-mp:latest analysis/outcome_time_var/outcome_summary_stats.do"),
      needs = list(
        glue("generate_merged_precovid"),
        glue("generate_merged_postcovid1"),
        glue("generate_merged_postcovid2"),
        glue("generate_merged_postcovid3")
      ),
      moderately_sensitive = list(
        outcome_summary_stats_csv = glue("output/regressions/outcome_summary_stats.csv")
      )
    )
  )
  #Appending action to the list of all actions for this .yaml 
  actions_list <- c(actions_list, outcome_summary) 
  
#Add action to run the RI Poisson/NB regressions on APC/EC all cond
for (covariate in covariates_all) {
  for (cohort in cohorts_all){
    regress_all_cond <- c(
      comment(glue("Runs {covariate} RI poisson & nb regs for APC/EC, {cohort}")),
      action(
        name = glue("generate_reg_all_cond_{covariate}_{cohort}"),
        run = glue("stata-mp:latest analysis/outcome_time_var/reg_all_cond.do {cohort} {covariate}"),
        needs = list(glue("generate_merged_{cohort}")), 
        moderately_sensitive = list(
          results_all_cond_csv = glue("output/regressions/results_all_cond_{covariate}_{cohort}.csv")
        )
      )
    )
    #Appending action to the list of all actions for this .yaml 
    actions_list <- c(actions_list, regress_all_cond)
  }
}    

#Add action to run the RI Poisson/NB regressions on APC/EC ACSC
for (cohort in cohorts_all){
  regress_acsc <- c(
    comment(glue("Runs poisson, nb, zinb regressions for ACSCs, cohort: {cohort}")),
    action(
      name = glue("generate_regressions_acsc_{cohort}"),
      run = glue("stata-mp:latest analysis/outcome_time_var/regressions_acsc.do {cohort}"),
      needs = list(glue("generate_merged_{cohort}")), 
      moderately_sensitive = list(
        results_acsc_csv = glue("output/regressions/results_acsc_{cohort}.csv")
      )
    )
  )
  #Appending action to the list of all actions for this .yaml 
  actions_list <- c(actions_list, regress_acsc)
} 

#Add action creating the forest plots  
  forest_plots <- c(
    comment(glue("Create the forest plots")),
    action(
      name = glue("generate_forest_plots"),
      run = glue("r:latest analysis/outcome_time_var/plots.R"),
      needs = list(
        glue("generate_reg_all_cond_unadjusted_precovid"),
        glue("generate_reg_all_cond_unadjusted_postcovid1"),
        glue("generate_reg_all_cond_unadjusted_postcovid2"),
        glue("generate_reg_all_cond_unadjusted_postcovid3"),
        glue("generate_reg_all_cond_adjusted_precovid"),
        glue("generate_reg_all_cond_adjusted_postcovid1"),
        glue("generate_reg_all_cond_adjusted_postcovid2"),
        glue("generate_reg_all_cond_adjusted_postcovid3"),
        glue("generate_regressions_acsc_precovid"),
        glue("generate_regressions_acsc_postcovid1"),
        glue("generate_regressions_acsc_postcovid2"),
        glue("generate_regressions_acsc_postcovid3")
      ),
      moderately_sensitive = list(
        fp_apc_m1_svg = glue("output/regressions/fp_apc_m1.svg"),
        fp_apc_m2_svg = glue("output/regressions/fp_apc_m2.svg"),
        fp_ec_m1_svg = glue("output/regressions/fp_ec_m1.svg"),
        fp_ec_m2_svg = glue("output/regressions/fp_ec_m2.svg")
      )
    )
  )
  #Appending action to the list of all actions for this .yaml 
  actions_list <- c(actions_list, forest_plots)  
  
#Add action creating the plots checking for a linear relationship bw each exp and the outcome  
for (cohort in cohorts_all){
  linear_check_corr_plots <- c(
    comment(glue("Correlation plots checking a linear exp & out relationship, {cohort}")),
    action(
      name = glue("linear_check_corr_plots_{cohort}"),
      run = glue("stata-mp:latest analysis/outcome_time_var/exp_out_plots.do {cohort}"),
      needs = list(
        glue("generate_merged_{cohort}")
      ),
      moderately_sensitive = list(
        graph1 = glue("output/regressions/exp_all_{cohort}.svg")
      )
    )
  )
  #Appending action to the list of all actions for this .yaml 
  actions_list <- c(actions_list, linear_check_corr_plots)     
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
