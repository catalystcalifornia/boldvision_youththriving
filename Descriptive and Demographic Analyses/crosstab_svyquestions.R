# The following script will create crosstab tables for the variables by other variables (in other words survey questions against other survey questions)
# Author: Maria Khan 
# QA DOC: W:\Project\OSI\Bold Vision\Youth Thriving Survey\Documentation\QA_crosstab_surveyquestions_function.docx

#### STEP 1: Setting Up and Downloading Tables ####
library(dplyr)
library(RPostgreSQL)
library(srvyr)
options(scipen=999)

# connect to postgres and pull credentials
source("W:\\RDA Team\\R\\credentials_source.R")
con <- connect_to_db("bold_vision")

#raw survey data 
svy_data <- dbGetQuery(con, "SELECT * FROM youth_thriving.raw_survey_data")
#data dictionary
svy_dd <- dbGetQuery(con, "SELECT * FROM youth_thriving.bvys_datadictionary_2024 where response_type = 'mc' AND response_domain !='Demographics' AND response_domain != 'Info'")

#### STEP 2: Create a function to cross tab two different variables ####

cross_tab_df <- function(data, var1, var2) {
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
    mutate(rate = rate * 100) %>%  # Convert to percentages
    ungroup()
  
  return(df_result)
}


cross_dd_de <- cross_tab_df(svy_data, "dd", "de")
View(cross_dd_de)

#### STEP 3: Close db connection ####
dbDisconnect(con)