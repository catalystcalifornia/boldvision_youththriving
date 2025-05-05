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
svy_data_raw <- dbGetQuery(con, "SELECT * FROM youth_thriving.raw_survey_data")
#data dictionary
svy_dd <- dbGetQuery(con, "SELECT * FROM youth_thriving.bvys_datadictionary_2024 where response_type = 'mc' AND response_domain !='Demographics' AND response_domain != 'Info'")
#race data
race_df <- dbGetQuery(con, 'SELECT response_id, nh_race, race_asian, detailed_asian, detailed_race, 
                      "isSouthAsian" AS south_asian, 
                       "isSoutheastAsian" AS southeast_asian
                         FROM youth_thriving.race_ethnicity_data where detailed_asian IS NOT NULL')
#outputs 476 response_ids that identified as some form of Asian (multiracial included)

#### STEP 2: Clean up your data and create a column that defines different Asian subgroups ####
#join to have a df with the detailed asian categories and filter out any survey respondents that did not identify with this category
svy_data <- race_df %>%
  left_join(svy_data_raw, by = "response_id")

svy_data <- svy_data %>%
  mutate(
    another_race_asian = ifelse(nh_race %in% c("nh_twoormor", "latinx"), 1, 0),
    east_asian = ifelse(str_detect(detailed_asian, regex("Chinese|Hmong|Japanese|Korean|Mongolian|Okinawan|Taiwanese|Other East Asian", ignore_case = FALSE)), 1, 0),
    central_asian = ifelse(str_detect(detailed_asian, regex("Tartarian|Uzbek|Central Asian|Kazakh|Uzbekh|Other Central Asian", ignore_case = FALSE)), 1, 0), 
    multiple_asian = rowSums(across(c(south_asian, southeast_asian, east_asian, central_asian))),
    subgroup_asian = case_when(
      another_race_asian ==  1 ~ "Multiracial" , # Asian and Another Race
      multiple_asian >  1 ~ "Multi-Asian", # More than one Asian subgroup
      south_asian == 1 ~ "South Asian Alone",
      southeast_asian == 1 ~ "Southeast Asian Alone",
      east_asian == 1 ~ "East Asian Alone",
      central_asian == 1 ~ "Central Asian Alone",
      race_asian == 1 ~ "Asian",
      TRUE ~ NA_character_
    ),
    southeast_asian_aoic = southeast_asian,
    south_asian_aoic = south_asian,
    central_asian_aoic = central_asian
  ) %>%
  select(response_id, subgroup_asian, race_asian, southeast_asian, south_asian, east_asian, central_asian, another_race_asian, multiple_asian, southeast_asian_aoic, south_asian_aoic, central_asian_aoic, everything())

qa<-svy_data%>%group_by(nh_race,subgroup_asian,detailed_asian)%>%summarise(count=n())

qa_2<-svy_data%>%group_by(detailed_asian,southeast_asian_aoic)%>%summarise(count=n())

qa_3<-svy_data%>%group_by(detailed_asian,south_asian_aoic)%>%summarise(count=n())

qa_4<-svy_data%>%group_by(detailed_asian,central_asian_aoic)%>%summarise(count=n())

# Recode AOIC variables for easier reading later
# list of binary vars it can apply to systematically
binary_vars <- svy_data%>% 
  select(southeast_asian_aoic:central_asian_aoic) %>%
  names()

# loop statement
for (i in binary_vars) { 
  svy_data <- svy_data %>%
    mutate(!!paste({{i}}):=
             case_when(
               !!sym(i)==1 ~ str_to_title(gsub("_"," ",!!paste({{i}}))),
               !!sym(i)==0 ~ str_to_title(gsub("_"," ",!!paste("not",{{i}}))),
               TRUE ~ NA
             ) )
}

####STEP 3: Create a function for Asian subgroups ####
fx_disagg_asian_orig <- function(df_input, variable_input,
                            low_input_a, low_input_b, low_rename,
                            high_input_a, high_input_b, high_rename) {
  
  # Filter for the variable of interest
  df <- df_input %>%
    # filter(.data[["variable"]] == variable_input) %>%
    as_survey(weights = weights_final) %>%
    filter(!is.na(!!sym(variable_input)), !is.na(subgroup_asian)) %>%
    group_by(subgroup_asian, !!sym(variable_input)) %>%
    summarise(
      count = n(),
      rate = survey_mean(vartype = c("cv", "se"), level = 0.90),
      .groups = "drop"
    ) %>%
    mutate(
      rate = rate * 100,
      rate_cv = rate_cv * 100,
      rate_se = rate_se * 100,
      moe = rate_se * 1.645
    )
  
  # Load response dictionary for this variable
  dict <- svy_dd %>%
    filter(variable == variable_input) %>%
    pivot_longer(cols = response_1:response_12,
                 names_to = "response_number",
                 values_to = "response",
                 values_drop_na = TRUE) 
  
  dict <- dict %>%
    mutate(variable_merge_col = 1:nrow(dict)) # for merging later
  
  # Merge dictionary
  df_merge <- df %>%
    left_join(dict, by = setNames("variable_merge_col", variable_input)) %>%  
    relocate(response)
  
  # Combine response groups
  df_combined_responses <- df_merge %>%
    mutate(response_group = case_when(
      response %in% c(low_input_a, low_input_b) ~ low_rename,
      response %in% c(high_input_a, high_input_b) ~ high_rename,
      TRUE ~ NA_character_
    )) %>%
    filter(!is.na(response_group)) %>%
    group_by(subgroup_asian, response_group) %>%
    summarise(
      count = sum(count, na.rm = TRUE),
      rate = sum(rate * count, na.rm = TRUE) / sum(count, na.rm = TRUE),
      rate_cv = rate_cv * 100,
      rate_se = rate_se * 100,
      moe = rate_se * 1.645,
      variable = first(variable),
      question = first(question),
      sub_question = first(sub_question),
      question_number = first(question_number),
      likert_type = first(likert_type),
      variable_name = first(variable_name),
      response_domain = first(response_domain),
      .groups = 'drop'
    )
  
  return(df_combined_responses)
}

fx_disagg_asian <- function(df_input, race_var, variable_input,
                            low_input_a, low_input_b, low_rename,
                            high_input_a, high_input_b, high_rename) {
  
  # Filter for the variable of interest
  df <- df_input %>% select(!!sym(race_var), !!sym(variable_input),weights_final)
      
  # Load response dictionary for this variable
  dict <- svy_dd %>%
    filter(variable == variable_input) %>%
    pivot_longer(cols = response_1:response_12,
                 names_to = "response_number",
                 values_to = "response",
                 values_drop_na = TRUE) 
  
  dict <- dict %>%
    mutate(variable_merge_col = 1:nrow(dict)) # for merging later
  
  # Merge dictionary
  df_merge <- df %>%
    left_join(dict, by = setNames("variable_merge_col", variable_input)) %>%  
    relocate(response)
  
  # Combine response groups
  df_combined_responses <- df_merge %>%
    mutate(response_group = case_when(
      response %in% c(low_input_a, low_input_b) ~ low_rename,
      response %in% c(high_input_a, high_input_b) ~ high_rename,
      TRUE ~ NA_character_
    )) %>%
    as_survey(weights = weights_final) %>%
    group_by(!!sym(race_var), response_group) %>%
    summarise(
      count = n(),
      rate = survey_mean(vartype = c("cv", "se"), level = 0.90),      
      variable = first(variable),
      question = first(question),
      sub_question = first(sub_question),
      question_number = first(question_number),
      likert_type = first(likert_type),
      variable_name = first(variable_name),
      response_domain = first(response_domain),
      .groups = 'drop'
    ) %>%
  filter(!is.na(response_group)) %>%
    mutate(
      rate = rate * 100,
      rate_cv = rate_cv * 100,
      rate_se = rate_se * 100,
      moe = rate_se * 1.645
    )
    
   return(df_combined_responses)
}


####STEP 4: Run tables on variables of interest and check if they look okay ####

disagg_asian_co_alone <- fx_disagg_asian(svy_data, "subgroup_asian", "co", "Never true", "Sometimes true", "Sometimes/Never True", "Often true", "Always true", "Often/Always True")

disagg_asian_co_se <- fx_disagg_asian(svy_data, "southeast_asian_aoic", "co", "Never true", "Sometimes true", "Sometimes/Never True", "Often true", "Always true", "Often/Always True")

disagg_asian_co_sa <- fx_disagg_asian(svy_data, "south_asian_aoic", "co", "Never true", "Sometimes true", "Sometimes/Never True", "Often true", "Always true", "Often/Always True")

disagg_asian_co_ca <- fx_disagg_asian(svy_data, "central_asian_aoic", "co", "Never true", "Sometimes true", "Sometimes/Never True", "Often true", "Always true", "Often/Always True")

disagg_asian_co<-rbind(disagg_asian_co_alone, 
                       disagg_asian_co_se %>%
                         rename(subgroup_asian=1), # rename columns for binding
                       disagg_asian_co_sa %>%
                         rename(subgroup_asian=1), # rename columns for binding
                       disagg_asian_co_ca %>%
                         rename(subgroup_asian=1)) %>% # rename columns for binding
                      filter(!str_detect(subgroup_asian,"Not ")) # remove the not aoic matching categories

disagg_asian_dl_alone <- fx_disagg_asian(svy_data, "subgroup_asian", "dl", "Never true", "Sometimes true", "Sometimes/Never True", "Often true", "Always true", "Often/Always True")

disagg_asian_dl_se <- fx_disagg_asian(svy_data, "southeast_asian_aoic", "dl", "Never true", "Sometimes true", "Sometimes/Never True", "Often true", "Always true", "Often/Always True")

disagg_asian_dl_sa <- fx_disagg_asian(svy_data, "south_asian_aoic", "dl", "Never true", "Sometimes true", "Sometimes/Never True", "Often true", "Always true", "Often/Always True")

disagg_asian_dl_ca <- fx_disagg_asian(svy_data, "central_asian_aoic", "dl", "Never true", "Sometimes true", "Sometimes/Never True", "Often true", "Always true", "Often/Always True")

disagg_asian_dl <- rbind(disagg_asian_dl_alone, 
                         disagg_asian_dl_se %>%
                           rename(subgroup_asian=1), # rename columns for binding
                         disagg_asian_dl_sa %>%
                           rename(subgroup_asian=1), # rename columns for binding
                         disagg_asian_dl_ca %>%
                           rename(subgroup_asian=1)) %>% # rename columns for binding
  filter(!str_detect(subgroup_asian,"Not ")) # remove the not aoic matching categories

# qa check of survey function
qa<- svy_data %>% as_survey_design(weights = weights_final) %>%
  group_by(subgroup_asian, dl) %>%
  summarise(count=n(), # participant records count
            rate = survey_prop(vartype=c("cv","se"),level=.90)) %>% 
  mutate(rate = rate * 100,
         rate_se = rate_se * 100,
         rate_cv=rate_cv*100)   # Convert to percentages
# checks out

####STEP 5: Create function for writing data table to database ####

write_survey_data_to_db <- function(df_var, variable_input) {
table_name <- paste0("asian_disagg_", variable_input)

# Write data to database
dbWriteTable(con, DBI::Id(schema = "youth_thriving", table = table_name), df_var,  overwrite = FALSE, row.names = FALSE)

# Create table comment
indicator <- paste0(
  "The following is a table of the rate by disaggregates Asian subgroups (alone categories including Asian + another race, youth with two or more regional Asian identities, and alone or in combination categories for groups with lower sample sizes) for the following variable of interest: ",  
  variable_input
)

source <- "Script: W:/Project/OSI/Bold Vision/Youth Thriving Survey/GitHub/MK/boldvision_youththriving/Descriptive and Demographic Analyses/analysis_asian_disag.R "
qa_filepath <- "W:/Project/OSI/Bold Vision/Youth Thriving Survey/Documentation/QA_asian_youth_analysis.docx"
# table_comment <- paste(indicator, source, qa_filepath)
column_names <- colnames(df_var)

# Define the table schema and name
schema <- "youth_thriving"

# List of column comments, correctly setting the demographic column name
column_comments <- c(
  "subgroup_asian" = "Asian subgroup includes alone and alone and in combo categories for select groups",
  "response_group" = "The response that the data is about",
  "count" = " count of youth that selected this response",
  "rate" = " rate of youth that selected this response",
  "rate_cv" = "A weighted coefficient of variation for this rate",
  "rate_se" = "A weighted standard error for this rate",
  "variable" = "Refers to the column label or variable in the survey data",
  "question" = "The question that this variable refers to",
  "sub_question" = "The subquestion that this variable refers to",
  "question_number" = "The question number of the variable",
  "likert_type" = "Likert scale type",
  "variable_name" = "The survey SUBcomponent this variable falls under",
  "response_domain" = "The survey component this variable falls under",
  "moe" = "A weighted margin of error for this rate"
)

add_table_comments(con, schema, table_name, indicator, source, qa_filepath, column_names, column_comments)
}

####STEP 6: Push tables to postgres ####
write_survey_data_to_db(disagg_asian_co, "co")
write_survey_data_to_db(disagg_asian_dl, "dl")

####STEP 7: close connections ####

dbDisconnect(con)