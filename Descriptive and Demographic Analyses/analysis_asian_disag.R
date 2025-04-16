# The following script will create crosstab tables for the variables by Asian Subgroups
# Author: Maria Khan 
# QA DOC: W:\Project\OSI\Bold Vision\Youth Thriving Survey\Documentation\QA_asian_youth_analysis.docx

#### STEP 1: Setting Up and Downloading Tables ####
library(dplyr)
library(RPostgreSQL)
library(srvyr)
library(stringr)
options(scipen=999)

# connect to postgres and pull credentials
source("W:\\RDA Team\\R\\credentials_source.R")
con <- connect_to_db("bold_vision")

#raw survey data 
svy_data <- dbGetQuery(con, "SELECT * FROM youth_thriving.raw_survey_data")
#data dictionary
svy_dd <- dbGetQuery(con, "SELECT * FROM youth_thriving.bvys_datadictionary_2024 where response_type = 'mc' AND response_domain !='Demographics' AND response_domain != 'Info'")
#race data
race_df <- dbGetQuery(con, "SELECT response_id, nh_race, race_asian, detailed_asian, detailed_race, 
                      "isSouthAsian" AS south_asian, 
                       "isSoutheastAsian" AS southeast_asian
                         FROM youth_thriving.race_ethnicity_data where detailed_asian IS NOT NULL")
#outputs 476 response_ids that identified as some form of Asian (multiracial included)

#join to have a df with the detailed asian categories and filter out any survey respondents that did not identify with this category
svy_data <- race_df %>%
  left_join(svy_data, by = "response_id")


####STEP 2: Create a function for detailed asian crosstabing with a survey question####
fx_detailed_asian <- function(variable_input) {
  dict <- svy_dd %>%
    filter(variable == variable_input) %>%
    pivot_longer(cols = response_1:response_12,
                 names_to = "response_numbers",
                 values_to = "response",
                 values_drop_na = TRUE)
  
  dict_var <- dict %>%
    mutate(variable_merge_col = 1:nrow(dict))
  
  df_var <- as_survey(svy_data, weights = !!sym("weights_final")) %>%
    filter(!is.na(!!sym(variable_input))) %>%  # Remove NAs
    group_by(detailed_asian, !!sym(variable_input)) %>%
    summarise(count = n(), 
              rate = survey_mean(vartype = c("cv", "se"), level = .90), .groups = "drop") %>%
    mutate(rate = rate * 100, 
           rate_cv = rate_cv * 100, 
           moe = rate_se * 1.645 * 100) 
  
  df_final <- merge(x = df_var, y = dict_var, 
                    by.x = variable_input, by.y = "variable_merge_col") %>%
    relocate(response) %>%
    select(-notes)
  
  return(df_final)
}

####STEP 3: Run functions for variables of interest####
df_co <- fx_detailed_asian("co")  

df_dl <- fx_detailed_asian("dl")

####STEP 4: Create function for south asian and southeast asian with a survey question ####
fx_subgroups_asian <- function(variable_input) {
  dict <- svy_dd %>%
    filter(variable == variable_input) %>%
    pivot_longer(cols = response_1:response_12,
                 names_to = "response_numbers",
                 values_to = "response",
                 values_drop_na = TRUE)
  
  dict_var <- dict %>%
    mutate(variable_merge_col = 1:nrow(dict))
  
  svy_data <- svy_data %>%
    mutate(
      subgroup_asian = case_when(
        south_asian == "isSouthAsian" ~ "South Asian",
        southeast_asian == "isSoutheastAsian" ~ "Southeast Asian",
        race_asian == "1" ~ "Asian",
        TRUE ~ NA_character_
      )
    )
  df_asian_combined <- as_survey(svy_data, weights = !!sym("weights_final")) %>%
    filter(!is.na(!!sym(variable_input)), !is.na(subgroup_asian)) %>%
    group_by(subgroup_asian, !!sym(variable_input)) %>%
    summarise(
      count = n(),
      rate = survey_mean(vartype = c("cv", "se"), level = .90),
      .groups = "drop"
    ) %>%
    mutate(
      rate = rate * 100,
      rate_cv = rate_cv * 100,
      moe = rate_se * 1.645 * 100
    )
  # df_southasian <- as_survey(svy_data, weights = !!sym("weights_final")) %>%
  #   filter(!is.na(!!sym(variable_input))) %>%  # Remove NAs
  #   group_by(south_asian, !!sym(variable_input)) %>%
  #   summarise(count = n(), 
  #             rate = survey_mean(vartype = c("cv", "se"), level = .90), .groups = "drop") %>%
  #   mutate(rate = rate * 100, 
  #          rate_cv = rate_cv * 100, 
  #          moe = rate_se * 1.645 * 100) 
  # 
  # df_southeastasian <- as_survey(svy_data, weights = !!sym("weights_final")) %>%
  #   filter(!is.na(!!sym(variable_input))) %>%  # Remove NAs
  #   group_by(southeast_asian, !!sym(variable_input)) %>%
  #   summarise(count = n(), 
  #             rate = survey_mean(vartype = c("cv", "se"), level = .90), .groups = "drop") %>%
  #   mutate(rate = rate * 100, 
  #          rate_cv = rate_cv * 100, 
  #          moe = rate_se * 1.645 * 100) 
  # 
  # df_asian <- as_survey(svy_data, weights = !!sym("weights_final")) %>%
  #   filter(!is.na(!!sym(variable_input))) %>%  # Remove NAs
  #   group_by(race_asian, !!sym(variable_input)) %>%
  #   summarise(count = n(), 
  #             rate = survey_mean(vartype = c("cv", "se"), level = .90), .groups = "drop") %>%
  #   mutate(rate = rate * 100, 
  #          rate_cv = rate_cv * 100, 
  #          moe = rate_se * 1.645 * 100) 
  # 
  # df_asian_combined <- bind_rows(df_southasian, df_southeastasian, df_asian)
  
  df_final <- merge(x = df_asian_combined, y = dict_var, 
                    by.x = variable_input, by.y = "variable_merge_col") %>%
    relocate(response) %>%
    select(-notes)
  
  return(df_final)
}

df_co_subgroup <- fx_subgroups_asian("co")
