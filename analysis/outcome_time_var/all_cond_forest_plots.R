#Loading in packages
library(tidyverse)
library(gt)
library(here)
library(ggplot2)
library(dplyr)
library(patchwork)
library(svglite)


#Importing data
outdir <- here("output", "regressions")
fs::dir_create(outdir)

# Creating a list of file names
file_names <- c(
  "results_all_cond_adjusted_precovid",
  "results_all_cond_adjusted_postcovid1",
  "results_all_cond_adjusted_postcovid2",
  "results_all_cond_adjusted_postcovid3",
  "results_all_cond_unadjusted_precovid",
  "results_all_cond_unadjusted_postcovid1",
  "results_all_cond_unadjusted_postcovid2",
  "results_all_cond_unadjusted_postcovid3"
)

# Creating a list? of column specifications
col_spec <- cols(
  cohort = col_character(),
  hosp_type = col_character(),
  acscs = col_character(),
  model_form = col_character(),
  out_var = col_character(),
  exp_var = col_character(),
  obs = col_double(),
  irr_exp = col_double(),
  se_exp = col_double(),
  p_exp = col_double(),
  lci_exp = col_double(),
  uci_exp = col_double(),
  irr_cons = col_double(),
  se_cons = col_double(),
  p_cons = col_double(),
  lci_cons = col_double(),
  uci_cons = col_double(),
  variance_ri = col_double(),
  se_ri = col_double(),
  lci_ri = col_double(),
  uci_ri = col_double(),
  lrtest_comparing = col_character(),
  chi2_lrtest = col_double(),
  p_lrtest = col_double(),
  ll = col_double(),
  p_ll = col_double(),
  aic = col_double(),
  bic = col_double(),
  error = col_double(),
  check_p = col_character(),
  check_irr = col_character(),
  check_chi2 = col_character(),
  model_form_num = col_double(),
  exp_var_long = col_character(),
  exp_var_num = col_double()
)


#Uploading and "merging" all the datasets we created in STATA 
datasets <- list(
  adjusted = map(c("precovid", "postcovid1", "postcovid2", "postcovid3"), function(cohort) {
    read_csv(here("output", "regressions", paste0("results_all_cond_adjusted_", cohort, ".csv")), col_types = col_spec)
  }) %>% setNames(c("precovid", "postcovid1", "postcovid2", "postcovid3")),
  
  unadjusted = map(c("precovid", "postcovid1", "postcovid2", "postcovid3"), function(cohort) {
    read_csv(here("output", "regressions", paste0("results_all_cond_unadjusted_", cohort, ".csv")), col_types = col_spec)
  }) %>% setNames(c("precovid", "postcovid1", "postcovid2", "postcovid3"))
)


# Creating objects 
outcomes <- c("apc", "ec")
models <- c("Negative binomial random intercepts", "Poisson random intercepts")


#Function to create the components of the forest plots
# FUNCTION: make the text-plot
make_text_plot <- function(data, cohort_name, outcome_name, model_name) {
  plot_data <- data %>% 
    filter(cohort == cohort_name, 
           out_var == outcome_name, 
           model_form == model_name)
  
  p <- ggplot(
    plot_data, aes(y= fct_rev(exp_var_long))) + 
    theme_void() +
    theme(
      plot.margin = margin(1, 1, 1, 1),
      plot.title = element_text(face = "bold", size = 6, hjust = 0)) +
    scale_x_continuous(limits = c(1, 1.1)) +
    geom_text(aes(x=1, label = irr_lab), hjust = 0, size = 2) +
    geom_text(aes(x=1.04, label = var_lab), hjust = 0, size = 2, color = "blue") +
    labs(title = "IRR (95% CI); Lower & upper random-intercept bounds (blue)")
  
  return(p)
}

# FUNCTION: make the forest plot
make_forest_plot <- function(data, cohort_name, outcome_name, model_name, show_yaxis = FALSE) {
  plot_data <- data %>% 
    filter(cohort == cohort_name, 
           out_var == outcome_name, 
           model_form == model_name)
  
  p <- ggplot(plot_data, aes(y = fct_rev(exp_var_long))) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "gray50", linewidth = 0.4) +
    geom_errorbar(aes(xmin = lci_exp, xmax = uci_exp), width = 0.4, linewidth = 0.4) +
    geom_point(aes(x = irr_exp), size = 1, shape = 18) +
    labs(title = cohort_name, x = "IRR") +
    theme_classic() +
    theme(
      plot.margin = margin(1, 1, 1, 1),
      panel.grid.major.x = element_line(color = "gray90", linewidth = 0.3),
      plot.title = element_text(face = "bold", size = 6, hjust = 0.5),
      axis.title.x = element_text(size = 6)
    )
  if (!show_yaxis) {
    p <- p + theme(
      axis.line.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank(),
      axis.title.y = element_blank()
    )
  } else {
    p <- p + labs(y = "") + 
      theme(axis.text.y = element_text(size = 5))
  }
  
  return(p)
}


# A loop to go through the combined datasets, by cohort and adjustment type
# And then create the plots for each  

for (adjustment_type in c("adjusted", "unadjusted")) {
  # Combine all cohorts for this adjustment type
  all_cond <- bind_rows(datasets[[adjustment_type]], .id = "cohort_id")
  
  #Order cohorts
  cohorts <- unique(all_cond$cohort)
  
  # Adding formatted labels to the data
  all_cond <- all_cond %>%
    arrange(exp_var_num) %>%
    mutate(
      exp_var_long = fct_reorder(exp_var_long, exp_var_num),
      irr_formatted = sprintf("%.2f", irr_exp),
      lci_formatted = sprintf("%.2f", lci_exp),
      uci_formatted = sprintf("%.2f", uci_exp),
      irr_lab = paste0(irr_formatted, " (", lci_formatted, "-", uci_formatted, ")"),
      var_lb_ri_formatted = sprintf("%.2f", lb_ri_irr),
      var_ub_ri_formatted = sprintf("%.2f", ub_ri_irr),
      var_lab = paste0(var_lb_ri_formatted, "-", var_ub_ri_formatted)
    )

  # Create plots for all cohorts, outcomes, and models 
  plot_list <- list()
  
  for (cohort in cohorts) {
    for (outcome in outcomes) {
      for (model in models) {
        model_short <- ifelse(model == "Negative binomial random intercepts", "m1", "m2")
        
        text_name <- paste0("text_", outcome, "_", gsub(" ", "_", model_short), "_", cohort)
        fp_name <- paste0("fp_", outcome, "_", gsub(" ", "_", model_short), "_", cohort)
        
        plot_list[[text_name]] <- make_text_plot(all_cond, cohort, outcome, model)
        plot_list[[fp_name]] <- make_forest_plot(all_cond, cohort, outcome, model, show_yaxis = TRUE)
      }
    }
  }
  
  # Combining the plots
  # APC 
  apc_m1 <- plot_list[["text_apc_m1_precovid"]] + plot_list[["fp_apc_m1_precovid"]] +
    plot_list[["text_apc_m1_postcovid1"]] + plot_list[["fp_apc_m1_postcovid1"]] +
    plot_list[["text_apc_m1_postcovid2"]] + plot_list[["fp_apc_m1_postcovid2"]] +
    plot_list[["text_apc_m1_postcovid3"]] + plot_list[["fp_apc_m1_postcovid3"]] + 
    plot_layout(ncol = 2, nrow=4, widths = c(2, 3, 2, 3, 2, 3, 2, 3)) +
    plot_annotation(
      title = paste0("APC admissions - negative binomial model with random-intercepts (", adjustment_type, ")"),
      subtitle = "Forest plots show the incidence rate ratios for each exposure, obtained from one-to-one regressions",
      theme = theme(
        plot.title = element_text(size = 10, face = "bold", hjust = 0),
        plot.subtitle = element_text(size = 8, hjust = 0)))    
  
  apc_m2 <- plot_list[["text_apc_m2_precovid"]] + plot_list[["fp_apc_m2_precovid"]] +
    plot_list[["text_apc_m2_postcovid1"]] + plot_list[["fp_apc_m2_postcovid1"]] +
    plot_list[["text_apc_m2_postcovid2"]] + plot_list[["fp_apc_m2_postcovid2"]] +
    plot_list[["text_apc_m2_postcovid3"]] + plot_list[["fp_apc_m2_postcovid3"]] +
    plot_layout(ncol = 2, nrow = 4, widths = c(2, 3, 2, 3, 2, 3, 2, 3)) +
    plot_annotation(
      title = paste0("APC admissions - Poisson model with random-intercepts (", adjustment_type, ")"),
      subtitle = "Forest plots show the incidence rate ratios for each exposure, obtained from one-to-one regressions",
      theme = theme(
        plot.title = element_text(size = 10, face = "bold", hjust = 0),
        plot.subtitle = element_text(size = 8, hjust = 0)))    
  
  # EC
  ec_m1 <- plot_list[["text_ec_m1_precovid"]] + plot_list[["fp_ec_m1_precovid"]] +
    plot_list[["text_ec_m1_postcovid1"]] + plot_list[["fp_ec_m1_postcovid1"]] +
    plot_list[["text_ec_m1_postcovid2"]] + plot_list[["fp_ec_m1_postcovid2"]] +
    plot_list[["text_ec_m1_postcovid3"]] + plot_list[["fp_ec_m1_postcovid3"]] +
    plot_layout(ncol = 2, widths = c(2, 3, 2, 3, 2, 3, 2, 3)) +
    plot_annotation(
      title = paste0("EC admissions - negative binomial model with random-intercepts (", adjustment_type, ")"),
      subtitle = "Forest plots show the incidence rate ratios for each exposure, obtained from one-to-one regressions",
      theme = theme(
        plot.title = element_text(size = 10, face = "bold", hjust = 0),
        plot.subtitle = element_text(size = 8, hjust = 0)))    
  
  ec_m2 <- plot_list[["text_ec_m2_precovid"]] + plot_list[["fp_ec_m2_precovid"]] +
    plot_list[["text_ec_m2_postcovid1"]] + plot_list[["fp_ec_m2_postcovid1"]] +
    plot_list[["text_ec_m2_postcovid2"]] + plot_list[["fp_ec_m2_postcovid2"]] +
    plot_list[["text_ec_m2_postcovid3"]] + plot_list[["fp_ec_m2_postcovid3"]] +
    plot_layout(ncol = 2, widths = c(2, 3, 2, 3, 2, 3, 2, 3)) +
    plot_annotation(
      title = paste0("EC admissions - Poisson model with random-intercepts (", adjustment_type, ")"),
      subtitle = "Forest plots show the incidence rate ratios for each exposure, obtained from one-to-one regressions",
      theme = theme(
        plot.title = element_text(size = 10, face = "bold", hjust = 0),
        plot.subtitle = element_text(size = 8, hjust = 0)))    
  
  # Saving the graphs with adjustment type in filename
  ggsave(filename = paste0("fp_apc_m1_", adjustment_type, ".svg"), path=outdir, plot= apc_m1, width = 7, height = 10)    
  ggsave(filename = paste0("fp_apc_m2_", adjustment_type, ".svg"), path=outdir, plot= apc_m2, width = 7, height = 10)    
  ggsave(filename = paste0("fp_ec_m1_", adjustment_type, ".svg"), path=outdir, plot= ec_m1, width = 7, height = 10)    
  ggsave(filename = paste0("fp_ec_m2_", adjustment_type, ".svg"), path=outdir, plot= ec_m2, width = 7, height = 10)    
}