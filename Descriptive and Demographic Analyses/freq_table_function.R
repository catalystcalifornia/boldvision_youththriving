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
             variable, question, sub_question, question_number, 
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
  dbWriteTable(con, c('youth_thriving', table_name), df,
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
  
  # Add comments to columns
  dbSendQuery(con, paste0("
    COMMENT ON COLUMN youth_thriving.", table_name, ".variable IS 
      'refers to the column label or variable in the survey data';
    COMMENT ON COLUMN youth_thriving.", table_name, ".question IS 
      'the question that this variable refers to';
    COMMENT ON COLUMN youth_thriving.", table_name, ".sub_question IS 
      'the subquestion that this variable refers to';
    COMMENT ON COLUMN youth_thriving.", table_name, ".variable_name IS 
      'the survey SUBcomponent this variable falls under';
    COMMENT ON COLUMN youth_thriving.", table_name, ".response_domain IS 
      'the survey component this variable falls under';
    COMMENT ON COLUMN youth_thriving.", table_name, ".", demographic_variable, " IS 
      '", demographic_variable, " youth';
    COMMENT ON COLUMN youth_thriving.", table_name, ".response IS 
      'the response that the data is about';
    COMMENT ON COLUMN youth_thriving.", table_name, ".count IS 
      'the count of ", demographic_variable, " youth that selected this response';
    COMMENT ON COLUMN youth_thriving.", table_name, ".rate IS 
      'the weighted % of", demographic_variable, " youth who selected this response out of the total number of ", demographic_variable, " youth who answered this question';
    COMMENT ON COLUMN youth_thriving.", table_name, ".rate_cv IS 
      'a weighted coefficient of variation for this rate';
    COMMENT ON COLUMN youth_thriving.", table_name, ".moe IS 
      'a weighted margin of error for this rate';
  "))
}
