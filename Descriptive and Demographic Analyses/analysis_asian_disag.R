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
race_df <- dbGetQuery(con, 'SELECT response_id, nh_race, race_asian, detailed_asian, detailed_race, 
                      "isSouthAsian" AS south_asian, 
                       "isSoutheastAsian" AS southeast_asian
                         FROM youth_thriving.race_ethnicity_data where detailed_asian IS NOT NULL')
#outputs 476 response_ids that identified as some form of Asian (multiracial included)

#### STEP 2: Clean up your data and create a column that defines different Asian subgroups ####
#join to have a df with the detailed asian categories and filter out any survey respondents that did not identify with this category
svy_data <- race_df %>%
  left_join(svy_data, by = "response_id")

svy_data <- svy_data %>%
  mutate(
    another_race_asian = ifelse(nh_race %in% c("nh_twoormor", "latinx"), 1, 0),
    east_asian = ifelse(str_detect(detailed_asian, regex("Chinese|Hmong|Japanese|Korean|Mongolian|Okinawan|Taiwanese|Other East Asian", ignore_case = FALSE)), 1, 0),
    central_asian = ifelse(str_detect(detailed_asian, regex("Tartarian|Uzbek|Central Asian|Kazakh|Uzbekh|Other Central Asian", ignore_case = FALSE)), 1, 0), 
    multiple_asian = rowSums(across(c(south_asian, southeast_asian, east_asian, central_asian))),
    subgroup_asian = case_when(
      multiple_asian >  1 ~ "Multi-Asian", # More than one Asian subgroup
      another_race_asian ==  1 ~ "Multiracial" , # Asian and Another Race
      south_asian == 1 ~ "South Asian",
      southeast_asian == 1 ~ "Southeast Asian",
      east_asian == 1 ~ "East Asian",
      central_asian == 1 ~ "Central Asian",
      race_asian == 1 ~ "Asian",
      TRUE ~ NA_character_
    ),
    southeast_asian_aoic = southeast_asian,
    south_asian_aoic = south_asian,
    central_asian_aoic = central_asian
  ) %>%
  select(response_id, subgroup_asian, race_asian, southeast_asian, south_asian, east_asian, central_asian, another_race_asian, multiple_asian, everything())

####STEP 3: Create a function for Asian subgroups ####
fx_disagg_asian <- function(df_input, variable_input) {
  df_asian_combined <- as_survey(df_input, weights = !!sym("weights_final")) %>%
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
      rate_se = rate_se * 100, 
      moe = rate_se * 1.645 * 100
    )
  
  dict <- svy_dd %>%
    filter(variable == variable_input) %>%
    pivot_longer(cols = response_1:response_12,
                 names_to = "response_numbers",
                 values_to = "response",
                 values_drop_na = TRUE)
  
  dict_var <- dict %>%
    mutate(variable_merge_col = 1:nrow(dict))
  
  df_final <- merge(x = df_asian_combined, y = dict_var, 
                    by.x = variable_input, by.y = "variable_merge_col") %>%
    relocate(response) %>%
    select(-notes)
  
  return(df_final)
}

####STEP 4: Run tables on variables of interest and check if they look okay ####
disagg_asian_co <- fx_disagg_asian(svy_data, "co")
disagg_asian_dl <-fx_disagg_asian(svy_data, "dl")

####STEP 5: Create function for writing data table to database ####

write_survey_data_to_db <- function(df_var, variable_input) {
table_name <- paste0("asian_disagg_", variable_input)

# Write data to database
dbWriteTable(con, DBI::Id(schema = "youth_thriving", table = table_name), df_var,  overwrite = FALSE, row.names = FALSE)

# Create table comment
indicator <- paste0(
  "The following is a table of the rate by disaggregates Asian subgroups for the following variable of interest: ",  
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
  "subgroup_asian" = "Asian subgroup",
  "response" = "The response that the data is about",
  "count" = " count of youth that selected this response",
  "rate" = " rate of youth that selected this response",
  "rate_cv" = "A weighted coefficient of variation for this rate",
  "moe" = "A weighted margin of error for this rate",
  "variable" = "Refers to the column label or variable in the survey data",
  "question" = "The question that this variable refers to",
  "sub_question" = "The subquestion that this variable refers to",
  "question number",
  "likert_type" = "Likert scale type",
  "variable_name" = "The survey SUBcomponent this variable falls under",
  "response_domain" = "The survey component this variable falls under"
)

add_table_comments(con, schema, table_name, indicator, source, qa_filepath, column_names, column_comments)
}

####STEP 6: Push tables to postgres ####
write_survey_data_to_db(disagg_asian_co, "co")
write_survey_data_to_db(disagg_asian_dl, "dl")

####STEP 7: close connections ####

dbDisconnect(con)