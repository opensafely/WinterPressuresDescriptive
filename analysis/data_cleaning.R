## IMPORTING R PACKAGES
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(haven)      # Allows you to import STATA .dta files 
library(stringr)    # Allows you to replace strings, useful for renaming vars
library(tidyverse)
library(glue)


#setwd("C:/Users/61487/Documents/GitHub/WinterPressuresDescriptive/output")


##RESHAPING & MERGING THE DATA
measures_path <-"output/"


measures_csv <- list.files(path = measures_path, pattern = "\\precovid.csv$", full.names = TRUE) 
  exp_measures_csv <- grep("_apc|_ec", measures_csv, invert=TRUE, value=TRUE) 
  out_measures_csv <- grep("_apc|_ec", measures_csv, value=TRUE) 
  
#list.files: lists all the files in a specified directory
    #pattern: option, only identifies files that match a specific regular expression
#grepl: an easier, non-reg-ex way to identify certain strings
    #because list.files doesn't always support regex

#Function that reshapes each csv file to wide, and renames the columns
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
#When reshaping & merging the individual csv files, add a variable to the final dataset denoting the number of registered patients in each practice 
## Figure out a way to create 3 different datasets, one for each cohort....\
#Create the CMS
#Remove the interval_start.x, interval_end.x for each merged dataset



