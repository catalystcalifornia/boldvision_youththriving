## The following R script is created to calculate average scores by component and subcomponent. 
## It serves to prep for run correlation and factor analysis to understand the relationship between the different variables measured in the Bold Vision Youth Thriving Survey.

## Author: Maria Khan 

#### Step 0: Setting Up Workspace ####
# library(data.table)
library(dplyr)
library(RPostgreSQL)
library(tidyr)
library(tidyverse)
# library(devtools) 
options(scipen=999)

# connect to postgres and source functions
source("W:\\RDA Team\\R\\credentials_source.R")

con <- connect_to_db("bold_vision")

#pulling raw survey data
svy_data <- dbGetQuery(con, "SELECT * FROM youth_thriving.raw_survey_data")
#pulling data dictionary and filtering for just variables associated with component related questions
svy_dd <- dbGetQuery(con, "SELECT * FROM youth_thriving.bvys_datadictionary_2024 where response_domain !='Demographics' AND response_domain != 'Info'
                     AND response_domain != 'Fun' AND response_domain != 'Weights'")
#transforming names that will eventually be columns so there shouldn't be any spaces, slashes, etc.
svy_dd <- svy_dd %>%
  mutate(variable_name = gsub("[ ,/-]", "", tolower(variable_name)),
         response_domain = gsub("[ ,/-]", "", tolower(response_domain)))

#function used to create master datasets to push to pgAdmin
combine_data_frames <- function(df_list) {
  combined_df <- do.call(rbind, df_list)
  return(combined_df)
}

#### Step 1: Create a datatable scoring every variable for each respondent ####

#making sure to only use unique ones and creating a list that the loop functions can run through
unique_indicators <- unique(svy_dd$variable)

#NOTE THAT Don't Wish to Answer, Not Sure, and Does Not Apply to Me ARE OMMITTED IN THIS STEP
true_factors<-c("Never true","Sometimes true","Often true","Always true") 
time_factors<-c("None of the time","A little of the time","Some of the time","Most of the time","All of the time")
time_factors_reverse<-c("All of the time", "Most of the time", "Some of the time", "A little of the time", "None of the time") #reverse so a greater number means a good outcome and a smaller number means a bad outcome
yes_factors<-c("No", "Yes")
yes_factors_reverse <- c("Yes", "No") #reverse so a greater number means a good outcome and a smaller number means a bad outcome
count_factors<-c("None","One","Two","Three or more")
freq_factors<-c("Never","Rarely","Sometimes","Most of the time","All of the time")
freq_factors_reverse<-c("All of the time", "Most of the time","Sometimes", "Rarely","Never") #reverse so a greater number means a good outcome and a smaller number means a bad outcome

for (i in unique_indicators) { 
dict <- svy_dd %>% filter(variable == i) %>%
  pivot_longer(cols = response_1: response_12,
               names_to = "response_numbers",
               values_to = paste0("response"),
               values_drop_na = TRUE)
dict_var <- dict %>%
  mutate(variable_factor_level = 1:nrow(dict)) 

svy_data_var <- svy_data %>% 
  select(c('response_id', i))

df_almost_final <-  merge(x = svy_data_var, y = dict_var, by.x = c(i), by.y = c("variable_factor_level"))

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
      likert_type == 'time_scale_rev' ~ list(time_factors_reverse),
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
}

data_frames_list <- paste("df_", unique_indicators, sep = "")

actual_data_frames <- lapply(data_frames_list, get)

df_merged_master <- combine_data_frames(actual_data_frames)

#### Step 2: Create a datatable averaging variable scores for each respondent and BY SUBCOMPONENT ####

#note that subcomponent is coded as variable_name in the data dictionary
unique_subcomponents <- unique(svy_dd$variable_name)

for (i in unique_subcomponents) { 
  
df_subcomponents <- df_merged_master %>%
  filter(variable_name == i) %>%
  group_by(response_id, variable_name) %>%
  summarise(subcomponent_score = mean(factor_score)) %>%
  rename(!!paste0(i, "_sc_score") := subcomponent_score) %>%
  select(-variable_name)

assign(paste0("df_sc_", i), df_subcomponents)

}

#### Step 3: Create a datatable averaging variable scores for each respondent and BY COMPONENT ####

#note that subcomponent is coded as response_domain in the data dictionary
unique_components <- unique(svy_dd$response_domain)

for (i in unique_components) { 
  
  df_components <- df_merged_master %>%
    filter(response_domain == i) %>%
    group_by(response_id, response_domain) %>%
    summarise(domain_score = mean(factor_score)) %>%
    rename(!!paste0(i, "_domain_score") := domain_score) %>%
    select(-response_domain)
  
  assign(paste0("df_domain_", i), df_components)
  
}

#### Step 4: Create final data table by merging the subcomponent average scores and the component average scores ####

#getting list of subcomponent tables
data_frames_list_sc <- paste("df_sc_", unique_subcomponents, sep = "")
sc_data_frames <- lapply(data_frames_list_sc, get)

#getting list of component tables
data_frames_list_domain <- paste("df_domain_", unique_components, sep = "")
domain_data_frames <- lapply(data_frames_list_domain, get)

#merge the tables by running a full join by the response_id columns so there is one row PER survey respondent
df_merged_final <- Reduce(function(x, y) full_join(x, y, by = "response_id"), c(sc_data_frames,domain_data_frames))

#### Step 5: Push final table to postgres and apply appropriate comments ####
dbWriteTable(con, c('youth_thriving', 'avg_scores'), df_merged_final,
             overwrite = TRUE, row.names = FALSE)

dbSendQuery(con, "COMMENT ON TABLE youth_thriving.avg_scores IS 'The following is a table of average scores across subcomponent (referred to as variable name in data dictionary) 
and across component (referred to as response domain in data dictionary). We first converted the responses to factor levels, ranging from worst outcome to best outcome-
the lower the score, the worst outcome this respondent reported and the higher the score, the better the outcome. Next, we averaged the scores per respondent to subcomponent and
then to component. Learn more about the methodology here: [insert QA doc]'
")


dbSendQuery(con, "COMMENT ON COLUMN youth_thriving.avg_scores.response_id IS 
                      'refers to the id of the respondent who took this survey';")

for (i in unique_subcomponents) {            
  dbSendQuery(con, paste0("COMMENT ON COLUMN youth_thriving.avg_scores.", i, "_sc_score", 
  " IS 'the average scoring of this subcomponent for this respondent';")) 
  }
                    

for (i in unique_components) {            
  dbSendQuery(con, paste0("COMMENT ON COLUMN youth_thriving.avg_scores.", i, "_domain_score", 
                          " IS 'the average scoring of this component/domain for this respondent';")) 
}

dbDisconnect(con)

# Qa
qa<-svy_dd %>%
  unite("likert_values_list", response_1:response_12, remove = FALSE)%>%
  select(variable,question,sub_question,variable_name,response_domain,likert,likert_type,likert_values_list)
