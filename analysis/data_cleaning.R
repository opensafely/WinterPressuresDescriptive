## IMPORTING R PACKAGES
  #install.packages("dplyr")
  #install.packages("tidy")
  #install.packages("readr")
  #install.packages("ggplot2")
  #install.packages("haven")
  #install.packages("stringr")
  #install.packages("tidyverse")
  #install.packages("glue")
  #install.packages("lubridate")
  #install.packages("here")

library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(haven)      # Allows you to import STATA .dta files 
library(stringr)    # Allows you to replace strings, useful for renaming vars
library(tidyverse)
library(glue)
library(lubridate)
library(here)
#library(arrow)




#DEFINING FUNCTIONS   
##var_consistency_check: 
    ## checks if the VALUES of a specific variable are the same:
        #1. For the entire variable WITHIN a file & 2. For the variable if it exists across multiple files 
    ## Arg 1: file_list: list of files you want to loop over 
    ## Arg 2: var_list: list of variables you want to check 
    #Other functions used when defining "var_consistency_check"
        #message: apparently a better way to print text than "print"  

var_consistency_check <- function(file_list, var_list) {
    #Makes sure the second argument in the function is a character vector of column names
        if (!is.character(var_list)) stop("`var_list` must be a character vector of column names.") 
    
    #Store reference values (from the first file)
        ref_values <- list()
        result <- TRUE #Add a function "result", start assuming the result = TRUE
   
    #Sequentially upload each .csv file in the file_list
        for (i in seq_along(file_list)) {
          df <-  readr::read_csv(file_list[[i]])
          for (var in var_list) {             #Then, sequentially go through each variable in the var_list
              if (!var %in% colnames(df)) {   #Check whether each variable is actually in the dataset
                  warning("Variable '", var, "' not found in dataset: ", i)
                  result <- FALSE
                  next                        #Allows the loop to continue?
              }
          # Variable - internal consistency: Check values are the same for each ROW in the dataset
              if (length(unique(df[[var]])) != 1) { #If the number of unique values in this var !=1
                  warning("Error - var: '", var, "' has multiple values in dataset: ", i)
                  result <- FALSE
              }
          #Variable - cross-file consistency, as compared to reference: 
              if (i == 1) {   #If it's the first file, set the reference values = the values from the first file
                  ref_values[[var]] <- unique(df[[var]])
              } else {        #If it's not the first file
                  if (!setequal(unique(df[[var]]), ref_values[[var]])) { #Compare the values from this file to the reference values
                  warning("Variable '", var, "' is inconsistent across datasets: ", i)
                  result <- FALSE
                  }
              }
    }
    }
return(result)
}


##date_check_long
  #Check date variable exists
  #Check that the date values = specific SET of SEQUENTIAL values, BY certain variables
  #Check that the date values are the same across datasets

date_check_long <- function(file_list, date_var_list, group_vars = NULL,
                            start_date = NULL, n_expected = NULL, by = "1 week") {
  #Makes sure the 'var_list' argument is a character vector of column names
  if (!is.character(date_var_list)) stop("`var_list` must be a character vector of column names.") 
  
  #Store reference values (from the first file)
  ref_values <- list()
  result <- TRUE #Add a function "result", start assuming the result = TRUE
  result_list <- list()  # store all results
  
  #Sequentially upload each .csv file in the file_list
  for (i in seq_along(file_list)) {
    df <- readr::read_csv(file_list[[i]])
    for (date_var in date_var_list) {             #Then, sequentially go through each variable in `var_list'
      if (!date_var %in% colnames(df)) {       #Check that the variable exists in the dataset
        warning("Variable '", date_var, "' not found in dataset: ", i)
        result <- FALSE
        next                            #Allows the loop to continue?
      }
      
      df[[date_var]] <- as.Date(df[[date_var]]) #Converts date_var into date format (extra step - most dates will already be in date format, but just in case)
      
      date_check_summary <- df %>% group_by(across(all_of(group_vars))) %>% #groups the data by the variables specified in the group_vars argument
        summarise(   #Summarise creates a summary dataset containing the below specified columns (min_date, max_date, etc)  
          dataset = i,
          min_date = min(.data[[date_var]]),       #The earliest date in the group. Note '.data' is from rlang (used by dyplr) and says "Look inside whatever the current data frame is"
          max_date = max(.data[[date_var]]),       #The latest date in the group
          n_dates = n_distinct(.data[[date_var]]), #Distinct rows aka dates in each group
          expected_dates = list(seq(               #Creates an item-list column to store the multiple values . Specifying that the multiple values are a list allows dyplyr You need to store them as a list bc it helps summarise!? 
                                    min(.data[[date_var]]), 
                                    max(.data[[date_var]]), 
                                    by = by)),
          actual_dates = list(sort(unique(.data[[date_var]]))),
          date_check = identical(    #The actual check - are the actual dates in the data = the expected dates?
            sort(unique(.data[[date_var]])), #Sorts the dataset, retrieves the distinct dates
            seq(min(.data[[date_var]]), max(.data[[date_var]]), by = by)
          ),
          start_check = if (!is.null(start_date)) min(.data[[date_var]]) == as.Date(start_date) else NA, #Checks whether the earliest date in the group is the same as the expected start date
          n_check = if (!is.null(n_expected)) n_distinct(.data[[date_var]]) == n_expected else NA, #Checks if the number of unique weekly dates is what you'd expect
          .groups = "drop"
        )
      result_list[[length(result_list) + 1]] <- date_check_summary
    }
  }  
  final_result <- dplyr::bind_rows(result_list)
    date_check_passed  = all(final_result$date_check,  na.rm = TRUE)
    start_check_passed = all(final_result$start_check, na.rm = TRUE)
    n_check_passed     = all(final_result$n_check,     na.rm = TRUE)
  
  return(list(
    final_result = final_result,
    date_check_passed = date_check_passed, 
    start_check_passed = start_check_passed, 
    n_check_passed = n_check_passed)) #Assigning names to the items so the R viewer will refer to them by name, not [1], [2], etc
} 

##Identical_vector_check: 
##Checks whether variables are COMPLETELY IDENTICAL across multiple dataframes 
identical_vector_check <- function(df_list, var_list) {
  result <- TRUE
  for (i in seq_along(df_list)) {
    for (var in var_list) {
      result <- identical(df_list[[1]][[var]], df_list[[i]][[var]])
      if (!(result == "TRUE")) {
        warning("Variable '", var, "'in dataset '", i , "' is NOT identical to dataset 1")
        result <- FALSE
      }
      else {
        print("All good!")
      }
    }
  }
  return(result)
}

##one_row_check
##Checks whether the dataframe has one row per id (unit of interest)
one_row_check <- function(df, id) {
  df_name <- deparse(substitute(df))  # captures the name of the data frame
  total_rows <- nrow(df)
  unique_ids <- dplyr::n_distinct(df[[id]])
  result <- total_rows == unique_ids
  
  if (result) {
    message("One row check = TRUE in dataset: ", df_name)
  } else {
      message("Dataset is NOT one row per practice. Total rows: ", total_rows, 
              ", Unique IDs: ", unique_ids,
              ", Dataset: ", df_name )
  }
  return(result)
}

##positive_var_check
#Checks whether the values of numeric variables are positive
positive_var_check <- function(df) {
  df_name <- deparse(substitute(df))  # captures the name of the data frame
  
  numeric_vars <- sapply(df, is.numeric)
    if (!any(numeric_vars)) {
      warning("ERROR! No numeric variables found in the dataset.")
      return(NULL)
    }
  
  result <- sapply(df[numeric_vars], function(col) all(col >= 0, na.rm = TRUE))
    if(all(result)){
      message("Positive var check = TRUE in dataset: ", df_name)
    } else {
      negative_vars <- names(result)[!result]
      warning("ERROR! The following variables contain negative values: ", paste(negative_vars, collapse = ", "))
    }
  return(result)
}

##range_check
#Checks whether the value of a specific variable fits within a certain range
range_check <- function(df, var_list, min, max) {
  for (var in var_list){
    if (!var %in% names(df)) {
      stop("Variable '", var, "' not found in the dataset.")
    }
    if (!is.numeric(df[[var]])) {
      stop("Variable '", var, "' is not numeric.")
    }
    
    #Now doing the actual check to see if all values of var are within the range (inclusive)
      values <-  df[[var]]
      result <- all(values >= min & values <= max, na.rm = TRUE)
    
      if (result) {
        message("All values in '", var, "' are within the range [", min, ", ", max, "]")
      } else {
        warning("Some values in '", var, "' are outside the range [", min, ", ", max, "]")
      }
  }
  return(result)
}

##Rename if var exists: renames multiple variables across multiple files
rename_if_var_exists <- function(file_list, rename_rules) {
  lapply(file_list, function(df) {
    df %>% rename_with(~ str_replace_all(., rename_rules))
  })
}

##merge_and_drop:
##Merges dataset, and drops duplicate variables 
merge_and_drop <- function(df_list, var_list, join_var, merged_df_name = "merged_df") {
    merged_df <- reduce(df_list, full_join, by = join_var) 
    
    #Identify if any of the vars in var_list are now duplicates, post-merge
      duplicate_vars <- grep(paste0("^(", paste(var_list, collapse = "|"), ")"), colnames(merged_df), value = TRUE)
    
    #Create a list of vars to remove, but make sure to KEEP the first instance of each var
      remove_vars <- unlist(lapply(var_list, function(v) {
        matches <- grep(paste0("^", v), duplicate_vars, value = TRUE)
        if (length(matches) > 1) matches[-1] else character(0)
      }))      
  
    #Keeping the first instance of the var, and removing the .x suffix
      merged_df <- merged_df %>% 
        select(-all_of(remove_vars)) %>%
        rename_with(~ str_replace(., "\\.x$", "")) #Removing the .x suffix
    message("Dataset merged and duplicates removed successfully")  
      assign(merged_df_name, merged_df, envir = .GlobalEnv) #adds new dataframe to the global environment
  }


##SETTING DIRECTORIES & PATHS
##SD own laptop, locally
  #setwd("C:/Users/61487/Documents/GitHub/WinterPressuresDescriptive/output")
  #measures_path <-"C:/Users/61487/Documents/GitHub/WinterPressuresDescriptive/output/measures"
##SD own laptop, through OneDrive
  #setwd("C:/Users/61487/OneDrive - London School of Hygiene and Tropical Medicine/GitHub/WinterPressuresDescriptive/output")
  #measures_path <- "C:/Users/61487/OneDrive - London School of Hygiene and Tropical Medicine/GitHub/WinterPressuresDescriptive/output/measures"

#SD work laptop
  #setwd("C:/Users/ShrinkhalaDawadi/OneDrive - London School of Hygiene and Tropical Medicine/GitHub/WinterPressuresDescriptive/output")
  #measures_path <-"C:/Users/ShrinkhalaDawadi/OneDrive - London School of Hygiene and Tropical Medicine/GitHub/WinterPressuresDescriptive/output/measures"

#OS 
  fs::dir_create(here::here("output", "measures"))
  measures_path <-"output/measures"
  
  print("This should be the working directory")
  wd <- getwd()
  print(wd)

  print("This is what here::here('output', 'measures')shows")
  print(here::here("output", "measures"))


##IMPORTING FILES
  #list.files: lists all the files in a specified directory
    #pattern: option, only identifies files that match a specific regular expression
  #grepl: an easier way to identify strings, b/c list.files doesn't support full regex in pattern (I think)

test <- list.files(path = "/workspace/output/measures", full.names = TRUE) 
print("This is the test list")
print(test)

measures_csv <- list.files(path = "/workspace/output/measures", pattern = "precovid\\.csv$", full.names = TRUE) 
  exp_measures_csv <- grep("_apc|_ec|_consultation|_vax", measures_csv, invert=TRUE, value=TRUE) 
  exp_vax_measures_csv <-grep("_vax", measures_csv,value=TRUE) 
  exp_cons_measures_csv <-grep("_consultation", measures_csv,value=TRUE) 
  
  out_measures_csv <- grep("acscs", (grep("_apc|_ec", measures_csv, value=TRUE)), invert=TRUE, value = TRUE)
  out_acscs_measures_csv <- grep("acscs", (grep("_apc|_ec", measures_csv, value=TRUE)), value = TRUE)
  
  print("This should be the list of ALL the CSV files (measures_csv)")
  print(measures_csv)
  print("This should be the exp_measures_csv list")
  print(exp_measures_csv)
  print("This should be the out_measures_csv list")
  print(out_measures_csv)
  
  

##CLEANING THE DATA 
#Exposures (cross-sectional):
  #"Transform" so that each proportion variable is it's own column. Data should be one row per practice. Then check & merge.
  
  if (var_consistency_check(exp_measures_csv, var_list = c("interval_start", "interval_end"))) {
    message("Variable consistency check passed for each dataset")
    
    #Pre-allocating objects
      wide_exp_measures <- vector("list", length(exp_measures_csv))   #list containing transformed datasets, set length = length of exp_measures_csv
      rename_list <- c("ratio_exp_prop" = "exp_prop", "numerator_exp_prop" = "num", "denominator_exp_prop" = "denom") #Renaming rules for dataset
      print("Pre-allocation done")

    #For-loop of the data management steps 
      for(i in seq_along(exp_measures_csv)) {
        print("For-loop started - seq along exp_measures_csv")
        df <- readr::read_csv(exp_measures_csv[[i]])
        print("import csv into df")
        wide_exp_measures[[i]] <- df %>%
          pivot_wider(       #Transform the dataset to "wide', i.e. one column per measure
            names_from = measure,
            values_from = c(numerator, denominator, ratio)) %>%
          rename_with(~ str_replace_all(., rename_list))    #Renaming the variables
       
        #Check that each wide dataset now has one row per practice
          if(one_row_check(wide_exp_measures[[i]], "practice_pseudo_id")){
          } else {
            stop()
          }
        #Check that each numerical variable is non-negative
          if(all(positive_var_check(wide_exp_measures[[i]]))) {
          }else{
            stop()
          }
      }
    #Merging each dataset, dropping repeat variables  
      merge_and_drop(
        wide_exp_measures, 
        var_list = c("interval_start", "interval_end"), 
        c("practice_pseudo_id"), 
        merged_df_name = "merged_exp_measures")
      #Once again, checking that data are one row per practice
        if(one_row_check(merged_exp_measures, "practice_pseudo_id")) {
        } else {
          message("merged_exp_measures is NOT one row per practice")
        }
      #Checking that each proportion variable goes between 0 and 1 
        prop_vars <- names(merged_exp_measures)[grepl("prop", names(merged_exp_measures))]
        range_check(merged_exp_measures, var_list = prop_vars, min = 0.000000000000000000, max= 1.00000000000000000000000)
  }
  

#Exposures (vaccines, cross-sectional) 
  #"Transform" so that each vax proportion variable is it's own column
  
  if (var_consistency_check(exp_vax_measures_csv, var_list = c("interval_start", "interval_end"))) {
    message("Variable consistency check passed for each dataset")
    
    #Pre-allocating objects
      rename_list <- c("ratio_exp_prop" = "exp_prop", "numerator_exp_prop" = "num", "denominator_exp_prop" = "denom") #Renaming rules for dataset
    
    print(exp_vax_measures_csv)
    #Transform the dataset to "wide', i.e. one column per measure
      wide_exp_vax_measures<- readr::read_csv(exp_vax_measures_csv) %>%
        pivot_wider(  
          names_from = measure,
          values_from = c(numerator, denominator, ratio)) %>%
        rename_with(~ str_replace_all(., rename_list))    #Renaming the variables
      
      #Check that each wide dataset now has one row per practice
        if(one_row_check(wide_exp_vax_measures, "practice_pseudo_id")){
        } else {
          stop()
        }
      
      #Check that each numerical variable is non-negative
        if(all(positive_var_check(wide_exp_vax_measures))) {
        }else{
          stop()
        }
      
      #Checking that each proportion variable goes between 0 and 1 
        prop_vars <- names(wide_exp_vax_measures)[grepl("prop", names(wide_exp_vax_measures))]
        range_check(wide_exp_vax_measures, var_list = prop_vars, min = 0.000000000000000000, max= 1.00000000000000000000000)
    }

#Exposures (consultations, longitudinal)  
   print("date_check_long for longitudinal exposure - consultation")
  date_check_cons <-date_check_long(
    exp_cons_measures_csv,
    date_var_list = c("interval_start", "interval_end"), 
    group_vars = NULL,
    start_date = "2017-10-01",
    n_expected = 12,
    by = "1 month"
  )
  #NOTE: Date check NOT passed - interval_start, and interval_end dates are not on the first of each month
  #I have left the data cleaning code blank for 
  
  
#Outcomes (longitudinal):
  #Just need to date check & merge (structurally, can keep as is: one row per practice per week)
  print("date_check_long for longitudinal outcomes")
  date_check_out <- date_check_long(
    out_measures_csv, 
    date_var_list = c("interval_start", "interval_end"), 
    group_vars = NULL, 
    start_date = "2018-10-01", 
    n_expected = 20,
    by= "1 week"
  )
  print("if date_check_out passed")
  if(date_check_out$date_check_passed) {
    ##Pre-allocating objects
      wide_out_measures <- vector("list", length(out_measures_csv))   #list containing transformed datasets
      rename_list <-c("numerator_out_num" = "num", "denominator_out_num" = "denom", "ratio_out_num" = "out_prop")   #Renaming rules for dataset
    
    #For-loop of the data management steps 
      for(i in seq_along(out_measures_csv)) {
        wide_out_measures[[i]] <- readr::read_csv(out_measures_csv[[i]])
    
      #Check that there are multiple rows per practice
        if(!one_row_check(wide_out_measures[[i]],"practice_pseudo_id")){
          message("There are multiple rows per practice in dataset: ", i)
        }else{
          stop()
        }
        
      #Check that each numerical variable is non-negative
        if(all(positive_var_check(wide_out_measures[[i]]))) {
        }else{
          stop()
        }
    print("Renaming existing variables in wide_out_measures")    
    #Using "reshape", but actually we're just renaming the existing variables and dropping the measure var
      wide_out_measures[[i]]<- wide_out_measures[[i]]  %>% 
        pivot_wider(
          names_from = measure,
          values_from = c(numerator, denominator, ratio)) %>%
        rename_with(~ str_replace_all(., rename_list))
      }
      
      #Check that interval_start and interval_end are consistent ACROSS datasets
        if (identical_vector_check(wide_out_measures, var_list = c("interval_start", "interval_end"))) {
        }else{
          stop()
        }
      
    #Merge and delete duplicate columns
      merge_and_drop(
        wide_out_measures, 
        var_list = c("interval_end"), 
        join_var = c("practice_pseudo_id", "interval_start"), 
        merged_df_name = "merged_out_measures")
      
      #Check again that there are multiple rows per practice
        if(!one_row_check(merged_out_measures, "practice_pseudo_id")) {
          message("OK - merged_out_measures is multiple rows per practice")
        } else {
          message("ERROR - something weird happened and merged_exp_measures is one row per practice")
        }
      #Checking that each proportion variable goes between 0 and 1 
        prop_vars <- names(merged_out_measures)[grepl("prop", names(merged_out_measures))]
        range_check(merged_out_measures, var_list = prop_vars, min = 0.000000000000000000, max= 1.00000000000000000000000)
  }


  
#Outcomes ACSCs (longitudinal):
  date_check_out_acscs <- date_check_long(
    out_acscs_measures_csv, 
    date_var_list = c("interval_start", "interval_end"), 
    group_vars = c("measure"), 
    start_date = "2018-10-01", 
    n_expected = 20,
    by= "1 week"
  )
  
  if(date_check_out_acscs$date_check_passed) {
    ##Pre-allocating objects
    wide_out_acscs_measures <- vector("list", length(out_acscs_measures_csv))   #list containing transformed datasets
    rename_list <-c("numerator_out_num" = "num", "denominator_out_num" = "denom", "ratio_out_num" = "out_acscs_prop")   #Renaming rules for dataset
    
    #For-loop of the data management steps 
    for(i in seq_along(out_acscs_measures_csv)) {
      wide_out_acscs_measures[[i]] <- readr::read_csv(out_acscs_measures_csv[[i]])
      
      #Check that there are multiple rows per practice
      if(!one_row_check(wide_out_acscs_measures[[i]],"practice_pseudo_id")){
        message("There are multiple rows per practice in dataset: ", i)
      }else{
        stop()
      }
      
      #Check that each numerical variable is non-negative
      if(all(positive_var_check(wide_out_acscs_measures[[i]]))) {
      }else{
        stop()
      }
      
      #Reshaping so that each ACSC condistion is it's own column 
      wide_out_acscs_measures[[i]]<- wide_out_acscs_measures[[i]]  %>% 
        pivot_wider(
          names_from = measure,
          values_from = c(numerator, denominator, ratio)) %>%
        rename_with(~ str_replace_all(., rename_list))
    }
    
    #Check that interval_start and interval_end are consistent ACROSS datasets
      if (identical_vector_check(wide_out_acscs_measures, var_list = c("interval_start", "interval_end"))) {
      }else{
        stop()
      }
    
    #Merge and delete duplicate columns
      merge_and_drop(
        wide_out_acscs_measures, 
        var_list = c("interval_end"), 
        join_var = c("practice_pseudo_id", "interval_start"), 
        merged_df_name = "merged_out_acscs_measures")
      
    #Check again that there are multiple rows per practice
      if(!one_row_check(merged_out_acscs_measures, "practice_pseudo_id")) {
        message("OK - merged_out_acscs_measures is multiple rows per practice")
      } else {
        message("ERROR - something weird happened and merged_exp_measures is one row per practice")
      }
      
    #Checking that each proportion variable goes between 0 and 1 
      prop_vars <- names(merged_out_acscs_measures)[grepl("prop", names(merged_out_acscs_measures))]
      range_check(merged_out_acscs_measures, var_list = prop_vars, min = 0.000000000000000000, max= 1.00000000000000000000000)
  }

  
#Merging the exposures, exposures_vax, outcomes, and outcomes_acscs data together
  exp_data_precovid <-left_join(merged_exp_measures, wide_exp_vax_measures, by ="practice_pseudo_id") %>%
    rename(interval_start_exp = interval_start.x,
           interval_end_exp = interval_end.x,
           interval_start_exp_vax = interval_start.y,
           interval_end_exp_vax =interval_end.y)

  
  out_data_precovid <- left_join(merged_out_measures, merged_out_acscs_measures, by = c("practice_pseudo_id", "interval_start", "interval_end"))
  
  
  analytic_data_precovid <- left_join(out_data_precovid, merged_exp_measures, by = "practice_pseudo_id") #Merging the exp data to the longitudinal outcomes
  analytic_data_precovid <- left_join(analytic_data_precovid, wide_exp_vax_measures, by = "practice_pseudo_id") %>% #Then merging the exp_vax data
    rename(interval_start_out = interval_start.x,
           interval_end_out = interval_end.x,
           interval_start_exp = interval_start.y,
           interval_end_exp = interval_end.y,
           interval_start_exp_vax = interval_start,
           interval_end_exp_vax = interval_end)
  
  
#EXPORTING ANALYTIC DATASET  
  data.table::fwrite(analytic_data_precovid, "/workspace/output/analytic_data_precovid.csv") #Exp, exp_vax, out, out_acscs combined
  data.table::fwrite(exp_data_precovid, "/workspace/output/exp_data_precovid.csv") #Exp + exp_vax
  data.table::fwrite(out_data_precovid, "/workspace/output/out_data_precovid.csv") #Out + out_acscs
  
  
  #data.table::fwrite(analytic_data_precovid, "C:/Users/61487/OneDrive - London School of Hygiene and Tropical Medicine/GitHub/WinterPressuresDescriptive/output/analytic_data_precovid.csv") 
  #data.table::fwrite(exp_data_precovid, "C:/Users/61487/OneDrive - London School of Hygiene and Tropical Medicine/GitHub/WinterPressuresDescriptive/output/exp_data_precovid.csv")
  #data.table::fwrite(out_data_precovid, "C:/Users/61487/OneDrive - London School of Hygiene and Tropical Medicine/GitHub/WinterPressuresDescriptive/output/out_data_precovid.csv")
  
  
 
#TO DO:
#Get the positive_var_check function to output a nice dataset (like date_check_long does)
#Maybe: Check that each denom variable is the same, and then drop them (because they should just be the practice list size)
    #Only issue is if some patients have missing data on key characteristics.
#Add the number of registered patients used to calculate each proportion variable
  #CHECK that this number is consistent within each dataset, and for each category variable 
## Figure out a way to create 3 different datasets, one for each cohort.
#Create the CMS
  
#Discuss with Zoe:
  #The proportion of patients variable for acscs
  #GP consultation rate - dates
    #Also confirm that we decided to make this a cross-sectional snapshot, BUT
      #per a couple of meetings ago, we want to visualise the variation in this variation before deciding HOW we're collapsing it
  
##Code to clear items from memory, and free unused memory
  #rm(list = ls())
  #gc()
  

