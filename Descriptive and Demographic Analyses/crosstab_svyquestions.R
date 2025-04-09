# The following script will create crosstab tables for the variables by other variables (in other words survey questions against other survey questions)
# Author: Maria Khan 
# QA DOC: W:\Project\OSI\Bold Vision\Youth Thriving Survey\Documentation\QA_crosstab_surveyquestions_function.docx

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

#### STEP 2: Create a function to cross tab two different variables ####

cross_tab_df <- function(data, var1, var2, data_dict) {
  # Convert to survey design using weights
  df_svy <- as_survey(data, weights = weights_final) %>%
    filter(!is.na(!!sym(var1)), !is.na(!!sym(var2)))  # Remove NAs
  
  # Compute weighted counts & rates
  df_result <- df_svy %>%
    group_by(!!sym(var1), !!sym(var2)) %>%
    summarise(
      count = survey_total(),
      rate = survey_prop()  # Automatically ensures sum = 100%
    ) %>%
    mutate(rate = rate * 100,
           rate_se = rate_se * 100) %>%  # Convert to percentages
    ungroup()
  
  #prep work for adding meaning to data
  dict <- data_dict %>%
    filter(variable == var1 | variable == var2) %>%
    pivot_longer(cols = response_1:response_12,
                 names_to = "response_numbers",
                 values_to = "response",
                 values_drop_na = TRUE)
  dict_var <- dict %>%
    mutate(
      response_number = as.numeric(str_extract(response_numbers, "\\d+")))  # Extract the number and convert to numeric
  dict_var1 <- dict_var %>%
    filter(variable == var1) %>%
    select(response_number, response, question, sub_question, response_domain, variable_name) %>%
    rename(response_var1 = response,
           component_var1 = response_domain,
           subcomponent_var1 = variable_name,
           question_var1 = question,
           sub_question_var1 = sub_question)

  df_var1_result <- df_result %>% 
    mutate(var1_number = .[[1]]) %>%
    left_join(dict_var1, by = c("var1_number" = "response_number"))
  
  dict_var2 <- dict_var %>%
    filter(variable == var2) %>%
    select(response_number, response, question, sub_question, response_domain, variable_name) %>%
    rename(response_var2 = response,
           component_var2 = response_domain,
           subcomponent_var2 = variable_name,
           question_var2 = question,
           sub_question_var2 = sub_question)
  
  df_var1_result_final <- df_var1_result %>%
    mutate(var2_number = .[[2]]) %>%
    left_join(dict_var2, by = c("var2_number" = "response_number"))
  
  return(df_var1_result_final)
  
}

cross_dd_de <- cross_tab_df(svy_data, "dd", "de", svy_dd)

# View(cross_dd_de)

#### STEP 3: Close db connection ####
dbDisconnect(con)
