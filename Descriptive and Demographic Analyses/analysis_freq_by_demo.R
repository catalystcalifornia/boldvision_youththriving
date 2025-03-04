# The following script will create frequency tables for the variables by binary demographics

#### STEP 1: Setting Up and Downloading Tables ####
library(dplyr)
library(RPostgreSQL)
library(sf)
library(tidyr)
library(tidyverse)
library(srvyr)
library(survey)
library(knitr)
library(kableExtra)
library(weights)
options(scipen=999)

# connect to postgres and pull credentials
source("W:\\RDA Team\\R\\credentials_source.R")
con <- connect_to_db("bold_vision")

#raw survey data 
svy_data <- dbGetQuery(con, "SELECT * FROM youth_thriving.raw_survey_data")
#data dictionary
svy_dd <- dbGetQuery(con, "SELECT * FROM youth_thriving.bvys_datadictionary_2024 where response_type = 'mc' AND response_domain !='Demographics' AND response_domain != 'Info'")

#LGBTQIA data in here and adding male_female too
id_sogi <- dbGetQuery(con, "SELECT response_id, cishet_lgbtqia AS lgbtqia, cisgender_mf AS male_female FROM youth_thriving.gender_sexuality_data")
#BINARY demographics data
id_binary_dem <- dbGetQuery(con, "SELECT * FROM youth_thriving.demographics_binary_data")
#Combine so we have one table with demographics by respondent id
id_dem <- id_binary_dem %>% left_join( id_sogi, 
                                by='response_id')
#combine with actual data
svy_data <- svy_data %>% left_join( id_dem, 
                                       by='response_id')


####Step 2: Creating a function to get frequency tables by demographic variable ####


fx_freq_table  <- function(demographic_variable) {
  weights_col <- "weights_final"  # Define the weight column name
  unique_indicators <- unique(svy_dd$variable)  # Extract unique indicators
  
  combined_data_list <- list()
  
  for (i in unique_indicators) { 
    # Filter and pivot dictionary
    dict <- svy_dd %>%
      filter(variable == i) %>%
      pivot_longer(cols = response_1:response_12,
                   names_to = "response_numbers",
                   values_to = "response",
                   values_drop_na = TRUE)
    
    dict_var <- dict %>%
      mutate(variable_merge_col = 1:nrow(dict))
    
    # Filter survey data, remove NAs, and summarize
    df_var <- as_survey(svy_data, weights = !!sym(weights_col)) %>%
      filter(!is.na(!!sym(i)), !is.na(!!sym(demographic_variable))) %>%  # Remove NAs
      group_by(!!sym(demographic_variable), !!sym(i)) %>%
      summarise(count = n(), 
                rate = survey_mean(vartype = c("cv", "se"), level = .90), .groups = "drop") %>%
      mutate(rate = rate * 100, 
             rate_cv = rate_cv * 100, 
             moe = rate_se * 1.645 * 100) %>%
      mutate(!!sym(demographic_variable) := case_when(
        !!sym(demographic_variable) == 1 ~ demographic_variable, 
        !!sym(demographic_variable) == 0 ~ paste0("not ", demographic_variable),
        TRUE ~ as.character(!!sym(demographic_variable)) # Keep other values unchanged
      ))
    
    # Merge data with dictionary
    df_almost_final <- merge(x = df_var, y = dict_var, 
                             by.x = i, by.y = "variable_merge_col") %>%
      relocate(response) 
    
    df_final_per_demo <- df_almost_final %>%
      relocate(!!sym(demographic_variable)) %>%
      select(all_of(demographic_variable), response, count, rate, rate_cv, moe, 
             variable, question, sub_question, question_number, likert_type,
             variable_name, response_domain)
    
    combined_data_list[[i]] <- df_final_per_demo
  }

  # Combine all dataframes into one
  df_merged <- do.call(rbind, combined_data_list)
  
  # Dynamically name the output dataframe
  table_name <- paste0("df_merged_per_", demographic_variable)
  assign(table_name, df_merged, envir = .GlobalEnv)  # Store in global environment
  
  return(df_merged)
}


####Step 3: Create a function where to you write to table to postgres ####
write_survey_data_to_db <- function(df, demographic_variable) {
  table_name <- paste0("response_analysis_per_", demographic_variable)
  
  # Write data to database
  dbWriteTable(con, DBI::Id(schema = "youth_thriving", table = table_name), df,
               overwrite = FALSE, row.names = FALSE)
  
  # Create table comment
  table_comment <- paste0(
    "The following is a table of response frequencies and rates for non-demographic questions grouped by",  
    demographic_variable, " status. The denominator for each stat is the total number of youth who 
    answered the question grouped by",  demographic_variable," status. For example, looking at 
    arrested youth, count represents how many arrested youth selected response X to a given question. 
    Rate represents what % of arrested youth selected response X out of the total number of arrested youth 
    who answered the question.",
    "W:\\Project\\OSI\\Bold Vision\\Youth Thriving Survey\\Documentation\\QA_freqtables_binarydemo.docx",
    " Created on ", Sys.Date()
  )
  
  dbSendQuery(con, paste0(
    "COMMENT ON TABLE youth_thriving.", table_name, " IS '", table_comment, "';"
  ))
  
  # Define the table schema and name
  schema_name <- "youth_thriving"
  
  # Define the actual column name dynamically
  demographic_col <- demographic_variable  # e.g., "bipoc", "lgbtqia", etc.
  
  # List of column comments, correctly setting the demographic column name
  column_comments <- list(
    "variable" = "Refers to the column label or variable in the survey data",
    "question" = "The question that this variable refers to",
    "sub_question" = "The subquestion that this variable refers to",
    "variable_name" = "The survey SUBcomponent this variable falls under",
    "response_domain" = "The survey component this variable falls under",
    demographic_col = paste0(demographic_col, " youth"),  # This fixes the issue
    "response" = "The response that the data is about",
    "count" = paste0("The count of ", demographic_col, " youth that selected this response"),
    "rate" = paste0("The weighted % of ", demographic_col, 
                    " youth who selected this response out of the total number of ", 
                    demographic_col, " youth who answered this question"),
    "rate_cv" = "A weighted coefficient of variation for this rate",
    "moe" = "A weighted margin of error for this rate",
    "likert_type" = "Likert scale type"
  )
  
  # Iterate through each column and send the COMMENT query individually
  for (col in names(column_comments)) {
    query <- paste0(
      "COMMENT ON COLUMN ", schema_name, ".", table_name, ".", col, " IS '", column_comments[[col]], "';"
    )
    dbSendQuery(con, query)
  }
  
}


####Step 4: Run the functions, check the tables in the environment and push to postgres if everything looks good ####
df_merged_per_bipoc <- fx_freq_table("bipoc")
df_merged_per_disconnected <- fx_freq_table("disconnected")
df_merged_per_systems_impacted <- fx_freq_table("systems_impacted")
df_merged_per_arrested <- fx_freq_table("arrested")
df_merged_per_suspended <- fx_freq_table("suspended")
df_merged_per_undocumented <- fx_freq_table("undocumented")
df_merged_per_unhoused <- fx_freq_table("unhoused")
df_merged_per_lgbtqia <- fx_freq_table("lgbtqia")
df_merged_per_male_female <- fx_freq_table("male_female") %>%
  mutate(male_female = ifelse(male_female == "male_female", "female", 
                              ifelse(male_female == "not male_female", "male", NA)))


#FIRST CHECK THE TABLES IN THE ENVIRONMENT AND THEN PUSH TO POSTGRES

write_survey_data_to_db(df_merged_per_bipoc, "bipoc")
write_survey_data_to_db(df_merged_per_disconnected, "disconnected")
write_survey_data_to_db(df_merged_per_systems_impacted, "systems_impacted")
write_survey_data_to_db(df_merged_per_arrested, "arrested")
write_survey_data_to_db(df_merged_per_suspended, "suspended")
write_survey_data_to_db(df_merged_per_undocumented, "undocumented")
write_survey_data_to_db(df_merged_per_unhoused, "unhoused")
write_survey_data_to_db(df_merged_per_lgbtqia, "lgbtqia")
write_survey_data_to_db(df_merged_per_male_female, "male_female")
  
####Step 5: CLOSE database connection! ####
dbDisconnect(con)
