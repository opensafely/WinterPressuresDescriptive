## IMPORTING R PACKAGES
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(haven)      # Allows you to import STATA .dta files 
library(stringr)    # Allows you to replace strings, useful for renaming vars
library(tidyverse)
library(glue)


## IMPORTING THE DATA


measures_csv <- list.files(path = "output/measures", pattern = "\\precovid.csv$", full.names = TRUE)  
#list.files: lists all the files in a specified directory
    #pattern: option, only identifies files that match a specific regular expression

#TO DO: add a variable denoting the number of registered patients in each practice 
#Function that reshapes each csv file to wide, and renames the columns
reshape_wide <- function(file) {
    df <-read_csv(file)   #Read in each .csv file 
    df_wide <- df %>%
        pivot_wider(
            names_from = measure,  # Column that defines new wide column names
            values_from = c(numerator, denominator, ratio)) %>%   # Columns containing the values we want to transform wide 
        select(-starts_with("denominator")) %>%  # remove all the other denominator variables
        rename_with(~ str_replace_all(., c("ratio_exp_prop" = "exp_prop", 
                                           "denominator_exp_prop_under_5y" = "registered_patients", 
                                          "numerator_exp_prop" = "num")
                                          ))
    return(df_wide)
}

wide_measures <- lapply(measures_csv, reshape_wide)  #Applying the reshape wide function to each measures csv file 

merged_measures <-reduce(wide_measures, full_join, by ="practice_pseudo_id")    #merging the measures together
    #reduce: purr/tidyverse package that sequentially joins all the dataframes in your list
    #full_join: dplyr function that combines datasets, including all rows in both datasets. Missing values are marked as NA

write_csv(merged_measures, "output/merged_measures.csv")

##TO DO 
## Figure out a way to create 3 different datasets, one for each cohort....



