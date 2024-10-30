## The following R script is created to run a correlation analysis to uderstand the relationship between the different variables measured in the Bold Vision Youth Thriving Survey.

## Author: Maria Kha 

#### Step 0: Setting Up Workspace ####
library(data.table)
library(dplyr)
library(RPostgreSQL)
library(tidyr)
library(readxl)
library(tidyverse)
library(srvyr)
library(survey)
library(devtools) 
options(scipen=999)

# connect to postgres and source functions
source("W:\\RDA Team\\R\\credentials_source.R")

con <- connect_to_db("bold_vision")

svy_data <- dbGetQuery(con, "SELECT * FROM youth_thriving.raw_survey_data")
svy_dd <- dbGetQuery(con, "SELECT * FROM youth_thriving.bvys_datadictionary_2024 where response_domain !='Demographics' AND response_domain != 'Info'")


#### Step 1: Create a datatable for postgres averaging scores per component and subcomponent for each survey respondent ####

## Step 1A: Create a reference table for how scoring will be done ##

# true_factors<-c("Never true","Sometimes true","Often true","Always true","Don't wish to answer") 
# time_factors<-c("None of the time","A little of the time","Some of the time","Most of the time","All of the time","Don't wish to answer")
# yes_factors<-c("Yes","No","Not sure","Don't wish to answer")
# yes_factors_2<-c("No","Yes","Dont Know","Don't wish to answer")
# count_factors<-c("None","One","Two","Three or more","Not Sure","Don't wish to answer")
# frequency_factors<-c("Never","Rarely","Sometimes","Most of the time","All of the time","Does not apply to me")
# frequency_factors_2<-c("Never","Rarely","Sometimes","Most of the time","All of the time","Don't wish to answer","Does not apply to me")
# 
# 
# # Filter out unwanted levels and create continuous scales
# true_factors_cont <- factor(true_factors, levels = c("Never true", "Sometimes true", "Often true", "Always true"))
# true_scale <- as.numeric(true_factors_cont)  # 1 to 4 scale
# 
# time_factors_cont <- factor(time_factors, levels = c("None of the time", "A little of the time", "Some of the time", "Most of the time", "All of the time"))
# time_scale <- as.numeric(time_factors_cont)  # 1 to 5 scale
# 
# yes_factors_cont <- factor(yes_factors, levels = c("Yes", "No"))
# yes_scale <- ifelse(yes_factors_cont == "Yes", 1, 0)  # Binary scale
# 
# count_factors_cont <- factor(count_factors, levels = c("None", "One", "Two", "Three or more"))
# count_scale <- as.numeric(count_factors_cont)  # 1 to 4 scale
# 
# frequency_factors_cont <- factor(frequency_factors, levels = c("Never","Rarely","Sometimes","Most of the time","All of the time"))
# frequency_scale <- as.numeric(frequency_factors_cont) # 1 to 4 scale
# 
# # View the resulting scales
# print(true_scale)      # Outputs 1 to 4 scale for "true_factors" levels
# print(time_scale)      # Outputs 1 to 5 scale for "time_factors" levels
# print(yes_scale)       # Outputs binary scale for "yes_factors" levels
# print(count_scale)     # Outputs 1 to 4 scale for "count_factors" levels
# print(frequency_scale) # Outputs 1 to 4 scale for "frequency_factors" levels


#making sure to only use unique ones and creating a list that the loop functions can run through
unique_indicators <- unique(svy_dd_filter$variable)
#function used to create master datasets to push to pgAdmin
combine_data_frames <- function(df_list) {
  combined_df <- do.call(rbind, df_list)
  return(combined_df)
}

#NOTE THAT Don't Wish to Answer, Not Sure, and Does Not Apply to Me ARE OMMITTED IN THIS STEP
true_factors<-c("Never true","Sometimes true","Often true","Always true") 
time_factors<-c("None of the time","A little of the time","Some of the time","Most of the time","All of the time")
time_factors_reverse<-c("All of the time", "Most of the time", "Some of the time", "A little of the time", "None of the time") #reverse so a greater number means a good outcome and a smaller number means a bad outcome
yes_factors<-c("No", "Yes")
yes_factors_reverse <- c("Yes", "No") #reverse so a greater number means a good outcome and a smaller number means a bad outcome
count_factors<-c("None","One","Two","Three or more")
freq_factors<-c("Never","Rarely","Sometimes","Most of the time","All of the time")
freq_factors_reverse<-c("All of the time", "Most of the time","Sometimes", "Rarely","Never") #reverse so a greater number means a good outcome and a smaller number means a bad outcome

#### Step 1B: Run scoring for one subcomponent #### 

dict <- svy_dd %>% filter(variable == 'cn') %>%
  pivot_longer(cols = response_1: response_12,
               names_to = "response_numbers",
               values_to = paste0("response"),
               values_drop_na = TRUE)
dict_var <- dict %>%
  mutate(variable_factor_level = 1:nrow(dict)) 

svy_data_var <- svy_data %>% 
  select(c('response_id', 'cn'))

df_almost_final <-  merge(x = svy_data_var, y = dict_var, by.x = c("cn"), by.y = c("variable_factor_level"))

df_final <- df_almost_final %>%
  mutate(
    factor_levels = case_when(
      likert_type == 'true_scale' ~ list(true_factors),
      likert_type == 'count_scale' ~ list(count_factors),
      likert_type == 'freq_scale' ~ list(freq_factors),
      likert_type == 'freq_scale_rev' ~ list(freq_factors_reverse),
      likert_type == 'yes_scale' ~ list(yes_factors),
      likert_type == 'yes_scale_rev' ~ list(yes_factors_reverse),
      likert_type == 'time_scale' ~ list(time_factors),
      likert_type == 'time_scale_rev' ~ list(time_factors_reverse)
      TRUE ~ list(NA_character_)
    )
  ) %>%
  rowwise() %>%  # Process each row independently to avoid recycling issues
  mutate(response = factor(response, levels = factor_levels),
         factor_score = as.numeric(response)) %>%
  ungroup() %>%  # Remove rowwise grouping
  filter(!is.na(response)) %>% 
  subset(select = c(response_id, variable, variable_name, response_domain, likert_type, response, factor_score))  # Drop the temporary column and just select final columns we want

assign(paste0("df_", i), df_final)


ata_frames_list <- paste("df_chi_", unique_indicators, sep = "")

actual_data_frames <- lapply(data_frames_list, get)

df_merged_chi_test <- combine_data_frames(actual_data_frames)

## averaging by subcomponent and then component


