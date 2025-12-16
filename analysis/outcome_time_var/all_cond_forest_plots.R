#Loading in packages
library(tidyverse)
library(gt)
library(here)
library(ggplot2)
library(dplyr)
library(patchwork)
library(svglite)
library(stringr)
library(viridis)

#Setting directories
outdir <- here("output", "regressions")
fs::dir_create(outdir)

# Creating objects 
outcomes <- c("apc", "ec")
models <- c("Negative binomial random intercepts", "Poisson random intercepts")
covariates <- c("yes", "no")

#Putting the relevant csv files into a list 
results <- list.files(path = outdir, pattern = paste0("all_cond.*\\.csv$"), full.names = TRUE) 
  
  print("This is the list of the csv files")
  print(results)

# Creating a function to import each .csv as a dataframe, applying this to all the list elements
  all_cond_list <- lapply(results, function(file) {
    readr::read_csv(file)
  })
  
  #Shorten the individual dataframe names 
    file_name <- tools::file_path_sans_ext(basename(results))
      file_name <- gsub("results_", "", file_name)  #shortening the file names
      file_name <- gsub("adjusted", "adj", file_name)
      file_name <- gsub("unadjusted", "unadj", file_name)
      
      names(all_cond_list) <- file_name #Apply new file names to list 
  
      
#Combining all into one dataset, some data management to make graphing easier
  all_cond <- bind_rows(all_cond_list, .id = "cohort") %>%
    mutate(cohort_year = str_remove_all(cohort, "all_cond_adj_|all_cond_unadj_"),
           cohort_year = str_replace_all(
                            cohort_year, c("postcovid1" = "2018/19", "postcovid2" = "2022/23", 
                            "postcovid3" = "2023/24","precovid" = "2024/25")),
           cohort_num = as.numeric(factor(cohort_year, 
                            levels = c("2018/19", "2022/23", "2023/24", "2024/25"))),
           adjusted = case_when(!is.na(co_var) ~ "yes", is.na(co_var) ~"no")) 
    

#FUNCTION: make the forest plot
make_forest_plot <- function(data, outcome, model, covariates) {
  #First create the dataframe that will populate the forest plot 
    plot_data <- data %>% 
      filter(out_var == outcome, 
             model_form == model,
             adjusted == covariates) %>%
      arrange(exp_var_num, cohort_num) %>%
      mutate(row_id = row_number()) 
    
  #Then create a dataframe containg the y-axis label positions & text
    y_axis <- plot_data  %>% 
      group_by(exp_var_long) %>% 
      summarise(hline_pos = last(row_id)+0.5,
                lab_pos = mean(row_id))
  #Vector to hold clean titles 
    title1 <- ifelse(model == "Negative binomial random intercepts", "Negative binomial", "Poisson") 
    title2 <- ifelse(covariates == "yes", "adjusted", "not adjusted")
    
  #Finally, create the forest plot 
    p <- ggplot(plot_data, aes(x = irr_exp, y = row_id, color = cohort_year )) + 
      geom_vline(xintercept = 1, linetype = "dashed", color = "gray50", linewidth = 0.4) +
      geom_hline(yintercept = y_axis$hline_pos,linetype = "dashed", color = "gray75", linewidth = 0.2) +
      geom_point(aes(x = irr_exp), size = 1, shape = 18) +
      geom_errorbar(aes(xmin = lci_exp, xmax = uci_exp), width = 0.4, linewidth = 0.4) +
      scale_y_continuous(breaks = y_axis$lab_pos, labels = y_axis$exp_var_long) +
      scale_color_viridis(discrete=TRUE, option="viridis") +
      labs(x = "IRR (95%CI)", 
           title = paste0(title1, ", ", title2),
           color = "Cohort") +
      theme_classic() +
      theme(plot.title = element_text(hjust = 0.5, size = 7),
            axis.title.x = element_text(size = 5),
            axis.text.x = element_text(size = 5),
            axis.title.y = element_blank(),
            axis.text.y = element_text(size = 5),
            plot.margin = margin(1, 1, 1, 1),
            aspect.ratio = 2.5)
    
    return(p)
}

#Looping through all model forms, outcomes, and adjustment types & creating forest plots for each 
# Create plots for all cohorts, outcomes, and models 
plot_list <- list()
  for (c in covariates) {
    for (o in outcomes) {
      for (m in models) {

        m2 <- ifelse(m == "Negative binomial random intercepts", "nb", "pois")
        c2 <- ifelse(c == "yes", "adj", "unadj")
        
        #text_name <- paste0("text_", outcome, "_", gsub(" ", "_", model_short), "_", cohort)
        #plot_list[[text_name]] <- make_text_plot(all_cond, cohort, outcome, model)
        
        fp_name <- paste0("fp_", m2, "_", o, "_",  c2)
        plot_list[[fp_name]] <- make_forest_plot(all_cond, o, m, c)
      }
    }
  }

#Combining the plots
#APC 
  fp_apc <-(plot_list$fp_nb_apc_adj +plot_list$fp_nb_apc_unadj +   
            plot_list$fp_pois_apc_adj + plot_list$fp_pois_apc_unadj + 
            plot_layout(ncol = 4, nrow = 1, 
                        heights = c(4), widths = c(5), guides = "collect") & 
                        theme(legend.position = "bottom")) +
            plot_annotation(
              title = "APC admissions - results of one-to-one regressions with random-intercepts",
              theme = theme(plot.title = element_text(size = 9, face = "bold", hjust = 0.5)))
  
  ggsave(filename = paste0("fp_apc.svg"), path=outdir, plot= fp_apc, width = 10, height = 5)    

#EC
  fp_ec <-(plot_list$fp_nb_ec_adj + plot_list$fp_nb_ec_unadj + 
           plot_list$fp_pois_ec_adj +  plot_list$fp_pois_ec_unadj + 
           plot_layout(ncol = 4, nrow = 1, 
                       heights = c(4), widths = c(5), guides = "collect") & 
                       theme(legend.position = "bottom")) +
           plot_annotation(
              title = "EC admissions - results of one-to-one regressions with random-intercepts",
              theme = theme(plot.title = element_text(size = 9, face = "bold", hjust = 0.5)))
  
  ggsave(filename = paste0("fp_ec.svg"), path=outdir, plot= fp_ec, width = 10, height = 5)    
