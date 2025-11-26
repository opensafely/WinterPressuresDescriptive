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

all_cond <- read_csv(
  here("output", "regressions", "results_all_cond.csv"),
  col_types = cols(
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
)




#Creating objects 
outcomes <- c("apc", "ec")
models <- c("Negative binomial random intercepts", "Poisson random intercepts")
cohorts <- unique(all_cond$cohort) # luckily, alphabetical order = time-order for cohorts


#Adding formatted labels to the data
all_cond <- all_cond %>%
  arrange(exp_var_num) %>% #Orders DATA according to exp_var_num
  mutate( #Creating "variables" in the dataset that are the text we'll display on the plots 
    exp_var_long = fct_reorder(exp_var_long, exp_var_num), #Convert exp_var_long into a factor w/20 levels, ordered by exp_var_num
    irr_formatted = sprintf("%.2f", irr_exp),     # Create formatted string versions the stats we'll display as text
    lci_formatted = sprintf("%.2f", lci_exp),
    uci_formatted = sprintf("%.2f", uci_exp),
    irr_lab = paste0(irr_formatted, " (", lci_formatted, "-", uci_formatted, ")"), #Combine strings as: IRR (LCI - UCI)
    var_formatted = sprintf("%.2f", variance_ri),   # Format variance and CIs
    var_lci_formatted = sprintf("%.2f", lci_ri),
    var_uci_formatted = sprintf("%.2f", uci_ri),
    var_lab = paste0(var_formatted, " (", var_lci_formatted, "-", var_uci_formatted, ")")
  )


# FUNCTION: make the text-plot, will pass to each cohort
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
    labs(title = "IRR (95% CI); Random-intercept variance (95% CI) in blue")
  
  return(p)
}


#FUNCTION: make the forest plot for each cohort
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


#Combining the plots - doing manually for now 
#APC 
  apc_m1 <- plot_list[["text_apc_m1_precovid"]] + plot_list[["fp_apc_m1_precovid"]] +
            plot_list[["text_apc_m1_postcovid1"]] + plot_list[["fp_apc_m1_postcovid1"]] +
            plot_list[["text_apc_m1_postcovid2"]] + plot_list[["fp_apc_m1_postcovid2"]] +
            plot_list[["text_apc_m1_postcovid3"]] + plot_list[["fp_apc_m1_postcovid3"]] + 
          plot_layout(ncol = 2, nrow=4, widths = c(2, 3, 2, 3, 2, 3, 2, 3)) +
          plot_annotation(
            title = "APC admissions - negative binomial model with random-intercepts",
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
            title = "APC admissions - Poisson model with random-intercepts",
            subtitle = "Forest plots show the incidence rate ratios for each exposure, obtained from one-to-one regressions",
            theme = theme(
              plot.title = element_text(size = 10, face = "bold", hjust = 0),
              plot.subtitle = element_text(size = 8, hjust = 0)))    

  
#EC
  ec_m1 <- plot_list[["text_ec_m1_precovid"]] + plot_list[["fp_ec_m1_precovid"]] +
           plot_list[["text_ec_m1_postcovid1"]] + plot_list[["fp_ec_m1_postcovid1"]] +
           plot_list[["text_ec_m1_postcovid2"]] + plot_list[["fp_ec_m1_postcovid2"]] +
           plot_list[["text_ec_m1_postcovid3"]] + plot_list[["fp_ec_m1_postcovid3"]] +
        plot_layout(ncol = 2, widths = c(2, 3, 2, 3, 2, 3, 2, 3)) +
        plot_annotation(
          title = "EC admissions - negative binomial model with random-intercepts",
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
          title = "EC admissions - Poisson model with random-intercepts",
          subtitle = "Forest plots show the incidence rate ratios for each exposure, obtained from one-to-one regressions",
          theme = theme(
            plot.title = element_text(size = 10, face = "bold", hjust = 0),
            plot.subtitle = element_text(size = 8, hjust = 0)))    

    
#Saving the graphs
  ggsave(filename = "fp_apc_m1.svg", path=outdir, plot= apc_m1, width = 7, height = 10)    
  ggsave(filename = "fp_apc_m2.svg", path=outdir, plot= apc_m2, width = 7, height = 10)    
  ggsave(filename = "fp_ec_m1.svg", path=outdir, plot= ec_m1, width = 7, height = 10)    
  ggsave(filename = "fp_ec_m2.svg", path=outdir, plot= ec_m2, width = 7, height = 10)    


    