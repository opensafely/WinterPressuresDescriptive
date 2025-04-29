## IMPORTING R PACKAGES
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(haven)      # Allows you to import STATA .dta files 
library(stringr)    # Allows you to replace strings, useful for renaming vars
library(tidyverse)
library(glue)
#library(arrow)


##SETTING DIRECTORIES & PATHS
#setwd("C:/Users/61487/Documents/GitHub/WinterPressuresDescriptive/output")
#measures_path <-"C:/Users/61487/Documents/GitHub/WinterPressuresDescriptive/output/measures"
measures_path <-"output/"


#DEFINING FUNCTIONS   
##var_consistency_check: 
    ## checks if the VALUES of a specific variable are the same:
        #1. For the entire variable WITHIN a file & 2. For the variable if it exists across multiple files 
    ## Arg 1: file_list: list of files you want to loop over 
    ## Arg 2: var_list: list of variables you want to check 
    #Other functions used when defining "var_consistency_check"
        #message: apparently a better way to print text than "print"  

var_consistency_check <- function(file_list, var_list, type = c("CS", "Long")) {
    #Makes sure that "type" is one of the allowed values
    type <- match.arg(type) 
    #Makes sure the second argument in the function is a character vector of column names
        if (!is.character(var_list)) stop("`var_list` must be a character vector of column names.") 
    
    #Store reference values (from the first file)
        ref_values <- list()
        result <- TRUE #Add a function "result", start assuming the result = TRUE
   
    #Sequentially upload each .csv file in the file_list
        for (i in seq_along(file_list)) {
            df <- file_list[[i]]
          for (var in var_list) {             #Then, sequentially go through each variable in the var_list
              if (!var %in% colnames(df)) {   #Check whether each variable is actually in the dataset
                  warning("Variable '", var, "' not found in dataset: ", i)
                  result <- FALSE
                  next                        #Allows the loop to continue?
              }
            if (type == "CS")  
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
            
            
            
            
              # Check 
    }
    }
return(result)
}

date_check_cs
  #Check date variable exists
  #Check that the date variable = specific value
  #Check that the date value is the same for all rows
  #check that the date value is the same across datasets

date_check_long
  #Check date variable exists
  #Check that the date values = specific SET of SEQUENTIAL values, BY certain variables
  #Check that the date values are the same across datasets



library(lubridate)
date_check_long <- function(file_list, date_var_list, group_vars = NULL,
                            start_date = NULL, n_weeks = NULL) {
  #Makes sure the 'var_list' argument is a character vector of column names
  if (!is.character(var_list)) stop("`var_list` must be a character vector of column names.") 
  
  #Store reference values (from the first file)
  ref_values <- list()
  result <- TRUE #Add a function "result", start assuming the result = TRUE
  
  #Sequentially upload each .csv file in the file_list
  for (i in seq_along(file_list)) {
    df <- file_list[[i]]
    for (date_var in date_var_list) {             #Then, sequentially go through each variable in `var_list'
      if (!date_var %in% colnames(df)) {       #Check that the variable exists in the dataset
        warning("Variable '", var, "' not found in dataset: ", i)
        result <- FALSE
        next                            #Allows the loop to continue?
      }
      
      df[[date_var]] <- as.Date(df[[date_var]]) #Converts date_var into date format (extra step - most dates will already be in date format, but just in case)
      
      
      df %>% group_by(across(all_of(group_vars))) %.% #groups the data by the variables specified in the group_vars argument
        summarise(   #For each group, calculates the following 
          min_date = min(.data[[date_var]]),       #The earliest date in the group. Note '.data' is from rlang (used by dyplr) and says "Look inside whatever the current data frame is"
          max_date = max(.data[[date_var]]),       #The latest date in the group
          n_dates = n_distinct(.data[[date_var]]), #Distinct rows aka dates in each group
          expected_dates = list(seq(               #Creates an item-list column to store the vectors
                                    min(.data[[date_var]]), 
                                    max(.data[[date_var]]), 
                                    by = "1 week")),
          actual_dates = list(sort(unique(.data[[date_var]])))
        )
                    
      
      
        
}  
  } 
}
}

#Q's for chat gpt
# what does .data specify in min(.data[[date_var]])?
# why do you have to create an item-list to store the vectors, e.g. through: expected_dates = list(seq(? What is the issue with comparing a volumn vector?


if (type == "Long")
  
  
date_check_acsc



##merge_and_drop:
    ##Runs var_consistency_check (prev function), and merges + drops the same variables IF they are consistent: 



##Identical_vector_check: 
    ##Checks whether variables are COMPLETELY IDENTICAL across multiple dataframes 
  identical_vector_check <- function(df_list, var_list) {
    for (i in seq_along(df_list)) {
      for (var in var_list) {
        result <- identical(df_list[[1]][[var]], df_list[[i]][[var]])
          if (!(result == "TRUE")) {
            warning("Variable '", var, "'in dataset '", i , "' is NOT identical to dataset 1")
          }
          else {
            print("All good!")
          }
      }
    }
  }

  
#Rename if var exists: renames multiple variables across multiple files
rename_if_var_exists <- function(file_list, rename_rules) {
  lapply(file_list, function(df) {
    df %>% rename_with(~ str_replace_all(., rename_rules))
  })
}




##IMPORTING FILES
  #list.files: lists all the files in a specified directory
    #pattern: option, only identifies files that match a specific regular expression
  #grepl: an easier way to identify strings, b/c list.files doesn't support full regex in pattern (I think)

measures_csv <- list.files(path = measures_path, pattern = "\\precovid.csv$", full.names = TRUE) 
  exp_measures_csv <- grep("_apc|_ec|_consultation", measures_csv, invert=TRUE, value=TRUE) 
  #out_measures_csv <- grep("_apc|_ec", measures_csv, value=TRUE)
  out_measures_csv <- grep("acscs", (grep("_apc|_ec", measures_csv, value=TRUE)), invert=TRUE, value = TRUE)
  out_acscs_measures_csv <- grep("acscs", (grep("_apc|_ec", measures_csv, value=TRUE)), value = TRUE)
  



#CLEANING THE DATA 
#Reshape so there's one column per proportion category variable
    wide_exp_measures <- vector("list", length(exp_measures_csv))  # pre-allocate list, set length = length of exp_measures_csv
    wide_out_measures <- vector("list", length(out_measures_csv))
    wide_out_acscs_measures <- vector("list", length(out_acscs_measures_csv))
    
    
    for(i in seq_along(exp_measures_csv)) {
        df <- readr::read_csv(exp_measures_csv[[i]], show_col_types = FALSE)

        wide_exp_measures[[i]] <- df %>%
            pivot_wider(
                names_from = measure,
                values_from = c(numerator, denominator, ratio))
    }  
    
    for(i in seq_along(out_measures_csv)) {
      df <- readr::read_csv(out_measures_csv[[i]], show_col_types = FALSE)
      
      wide_out_measures[[i]] <- df %>%
        pivot_wider(
          names_from = measure,
          values_from = c(numerator, denominator, ratio))
    } 
    
    for(i in seq_along(out_acscs_measures_csv)) {
      df <- readr::read_csv(out_acscs_measures_csv[[i]], show_col_types = FALSE)
      
      wide_out_acscs_measures[[i]] <- df %>%
        pivot_wider(
          names_from = measure,
          values_from = c(numerator, denominator, ratio))
    } 
    
#First rename of variables 
    rename_list <- c("ratio_exp_prop" = "exp_prop", "numerator_exp_prop" = "num", "denominator_exp_prop" = "denom")
    wide_exp_measures <- rename_if_var_exists(wide_exp_measures, rename_list)
    wide_out_measures <- rename_if_var_exists(wide_out_measures, rename_list)
    wide_out_acscs_measures <- rename_if_var_exists(wide_out_acscs_measures, rename_list)
                    
#Check that the interval_start and interval_end are internally + externally consistent
    #Note: interval_start/end will be the same because all the measures were defined using cohort_start     
    #NOTE: warnings do not automatically clear once a function is fixed!
        #Will continue to display the last set of errors, until a NEW set of errors replaces them            
    var_consistency_check(wide_exp_measures, var_list = c("interval_start", "interval_end"))
        
#Checking variables for consistency, merging datafiles, and dropping duplicate variables
    #Dates - cross-sectional exposures
    
    
    
    merge_and_drop(
      wide_exp_measures, 
      var_list = c("interval_start", "interval_end"), 
      c("practice_pseudo_id"), 
      merged_df_name = "merged_exp_measures")
    
    
    merge_and_drop(
      wide_out_measures, 
      var_list = c("interval_start", "interval_end"), 
      c("practice_pseudo_id"), 
      merged_df_name = "")
    
    #If it's a cross-sectional variable, we want to check that all the dates are the same
    #If it's a longitudinal variable, we want to check that all the row dates are the same when transformed wide
    
    
    merge_and_drop <- function(df_list, var_list, join_var, merged_df_name = "merged_df") {
      if (var_consistency_check(df_list, var_list)) { #Runs var_consistency_check & only proceeds if the function = TRUE
        merged_df <- reduce(df_list, full_join, by = join_var) 
        #Identify if any of the vars in var_list are now duplicates, post-merge
        duplicate_vars <- grep(paste0("^(", paste(var_list, collapse = "|"), ")"), colnames(merged_df), value = TRUE)
        #Create a list of vars to remove, but make sure to KEEP the first instance of each var
        
        remove_vars <- unlist(lapply(var_list, function(v) {
          matches <- grep(paste0("^", v), duplicate_vars, value = TRUE)
          if (length(matches) > 1) matches[-1] else character(0)
        }))      
        
        merged_df <- merged_df %>% select(-all_of(remove_vars))      
        #%>%
        #select(-starts_with(var_list))
        #select(-starts_with(var_list[1]), -starts_with(var_list[2]))  # Remove all but the first occurrence
        
        message("Consistency check passed, dataset merged, and duplicates removed successfully")  
        assign(merged_df_name, merged_df, envir = .GlobalEnv) #adds new dataframe to the global environment
      }
      else {
        stop("Inconsistent variables found - merging and duplication not run")
      } 
    }    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    merged_out_measures <- reduce(wide_out_measures, full_join, by = "practice_pseudo_id") 
    merged_out_acscs_measures <- reduce(wide_out_acscs_measures, full_join, by = "practice_pseudo_id") 
    
    
     #Identify if any of the vars in var_list are now duplicates, post-merge
    duplicate_vars <- grep(paste0("^(", paste(var_list, collapse = "|"), ")"), colnames(merged_df), value = TRUE)
    #Create a list of vars to remove, making sure to exclude the first instance of each var
    #keep_vars <- unique(duplicate_vars)
    #remove_vars <- duplicate_vars[duplicate_vars != keep_vars[1]]
    
    remove_vars <- unlist(lapply(var_list, function(v) {
      matches <- grep(paste0("^", v), duplicate_vars, value = TRUE)
      if (length(matches) > 1) matches[-1] else character(0)
    }))      
    
    
    
    merge_and_drop(
      wide_out_acscs_measures, 
      var_list = c("interval_start", "interval_end"), 
      c("practice_pseudo_id"), 
      merged_df_name = "merged_out_acscs_measures")





      
    
  



#DONE: #Reshape each file to wide
#DONE:    #First rename
#DONE:    #Merge
#DONE:    #Check interval_start & end
#DONE:        #Drop extra interval start & end files 
    #Check denominator
        #Drop extra denominator vars 
        #Second rename
        


##Function to check whether a variable is consistent within practice ID
    ##Denominator should be the same across each measure category variable, AND across the .csv files
        #Because we set a default denominator when defining all the measures
    ##We want to check:
        ##For each practice, across .csvs whether the denominators are the same
    ##IF they are all the same
        ##We can delete all denominators, apart from the first instance
        ##And re-name this variable: registered_patients

    ##Denominator needs to be the same for each measure category (because we're including missing)
    ##But CAN be different across the measure categories (bc diff patients may have diff data?)


#measures_consultations_precovid
    #The GP consultations are already in "category wide" format (each row represents a time point, not new category)
    #Therefore, the GP consultations are also in "long" format (each practice should have 12 rows of data)
    #We want to collapse this as an average per year, and then MERGE that into the exp_measures dataset....

    
    
    
#Questions for chatGPT
    #Why are you using readr instead of read_csv?
    #What does pre-allocating the list do? 
    #Can you explain the syntax of for (i in seq_along(exp_measures_csv))? 
    #What does if & stop do here? if (!is.character(vars)) stop("`vars` must be a character vector of column names.")
    #What does "next" do here? 
    for (var in var_list) { #Sequentially checks whether each variable in the var_list is in the dataset
      if (!var %in% colnames(df)) {
        warning("Variable '", var, "' not found in file: ", file_list[[i]])
        next #Allows the loop to continue?
      }



#YESTERDAY'S CODE    
#Function that reshapes each csv file to wide, renames the columns, checks for duplicate columns
    reshape_wide <- function(file) {
      df <-read_csv(file)   #Read in each .csv file 
      print(object.size(df), units = "auto")    #see the size of each .csv file       
      file.info(file)$size /(1024^2)  # Size of the CSV file on disk in MB
      df_wide <- df %>%
        pivot_wider(
          names_from = measure,  # Column that defines new wide column names
          values_from = c(numerator, denominator, ratio)) %>%   # Columns containing the values we want to transform wide 
        select(-starts_with("denominator")) %>%  # remove all the other denominator variables
        rename_with(~ str_replace_all(., c("ratio_exp_prop" = "exp_prop", 
                                           "denominator_exp_prop_under_5y" = "registered_patients", 
                                           "numerator_exp_prop" = "num")
        ))
      message("Columns after pivot: ", ncol(df_wide))
      return(df_wide)
    }

wide_exp_measures <- lapply(exp_measures_csv, reshape_wide)  #Applying the reshape wide function to each measures csv file 
wide_out_measures <- lapply(out_measures_csv, reshape_wide)   
  
merged_exp_measures <-reduce(wide_exp_measures, full_join, by ="practice_pseudo_id")    #merging the measures together
merged_out_measures <-reduce(wide_out_measures, full_join, by ="practice_pseudo_id")    

#reduce: purr/tidyverse package that sequentially joins all the dataframes in your list
    #full_join: dplyr function that combines datasets, including all rows in both datasets. Missing values are marked as NA
    #Info on different join functions: https://statisticsglobe.com/r-dplyr-join-inner-left-right-full-semi-anti

write_csv(merged_exp_measures, "merged_exp_measures.csv")
write_csv(merged_out_measures, "merged_out_measures.csv")
#data.table::fwrite(merged_exp_measures, "output/merged_exp_measures.csv")

##Code to clear items from memory, and free unused memory
  #rm(list = ls())
  #gc()
  

#CREATING THE CMS


##TO DO 
#Check that the interval start and end dates match what we expect, given the start_cohort date
#Add the number of registered patients used to calculate each proportion variable
    #CHECK that this number is consistent within each dataset, and for each category variable 

#When reshaping & merging the individual csv files, add a variable to the final dataset denoting the number of registered patients in each practice 
## Figure out a way to create 3 different datasets, one for each cohort....\
#Create the CMS
#Remove the interval_start.x, interval_end.x for each merged dataset



