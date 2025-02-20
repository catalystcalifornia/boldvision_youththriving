# The following script will create crosstab tables for the variables by other variables (in other words survey questions against other survey questions)
# Author: Maria Khan 
# QA DOC: W:\Project\OSI\Bold Vision\Youth Thriving Survey\Documentation\QA_crosstab_surveyquestions_function.docx

#### STEP 1: Setting Up and Downloading Tables ####
library(dplyr)
library(RPostgreSQL)
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
  # Create a contingency table
  tab <- table(data[[var1]], data[[var2]])
  
  # Convert table to data frame
  df_counts <- as.data.frame(tab)
  colnames(df_counts) <- c(var1, var2, "count")
  
  # Calculate rate
  df_rate <- as.data.frame(prop.table(tab) * 100)
  colnames(df_rate) <- c(var1, var2, "rate")
  
  # Merge counts and rate
  result_df <- left_join(df_counts, df_rate, by = c(var1, var2)) %>%
    mutate(rate = round(rate, 2))  # Round percentages
  
  return(result_df)
}

cross_dd_de <- cross_tab_df(svy_data, "dd", "de")
View(cross_dd_de)

#### STEP 3: Close db connection ####
dbDisconnect(con)