#Importing Data to PGAMIN Database 


####Step 1: Set Up ####


packages <- c("data.table", "stringr", "dplyr", "RPostgreSQL", "dbplyr", "srvyr", "tidyverse","tidycensus", "tidyr", "here", "sf", "usethis", 
              "readxl", "janitor") 


for(pkg in packages){
  library(pkg, character.only = TRUE)
}

options(scipen = 100) # disable scientific notation

# create connection for bold_vision
source("W:\\RDA Team\\R\\credentials_source.R")
con_bv <- connect_to_db("bold_vision") 


####Step 2: Read in Dataset and sent to Database with comments ####
ys_data <- read_excel("W:\\Project\\OSI\\Bold Vision\\Youth Thriving Survey\\Data\\Survey responses\\Final Data\\BVYTSWeightSummary_Database.xlsx",sheet="Database")

ys_data<-clean_names(ys_data)

# check for NA columns
columns_na_counts<- lapply(ys_data, function(x) sum(is.na(x)))%>%as.data.frame()%>%
  pivot_longer(everything(),names_to="variable",values_to="na_count")

# exclude extra blank columns
ys_data<-ys_data%>%select(-c(x155,x159))

colnames(ys_data)[grepl('q12a_how_true_is_this_about_you',colnames(ys_data))] <- 'q12a'
colnames(ys_data)[grepl('q10b_which_of_the_following_',colnames(ys_data))] <- 'q10b'
colnames(ys_data)[grepl('q10a_how_many_adults_really_',colnames(ys_data))] <- 'q10a'


dbWriteTable(con_bv, c('youth_thriving', 'raw_survey_data'), ys_data,
                          overwrite = FALSE, row.names = FALSE)

dbSendQuery(con_bv, "COMMENT ON TABLE youth_thriving.raw_survey_data IS 'The following dataset are responses from the Youth Thriving Survey conducted by Bold Vision in 2024. The data dictionary explaining each variable is here: youth_thriving.bvys_datadictionary_2024 . Steps explaining data cleaning can be found here: W:\\Project\\OSI\\Bold Vision\\Youth Thriving Survey\\Data\\Survey responses\\Final Data\\BVYTSPopulationWeighting_DataCleaning.pdf Original Dataset is here: a)	W:\\Project\\OSI\\Bold Vision\\Youth Thriving Survey\\Data\\Survey responses\\Final Data\\BVYTSWeightSummary_Database.xlsx The process for cleaning and uploading the data is explained in the QA Documentation here: W:\\Project\\OSI\\Bold Vision\\Youth Thriving Survey\\Documentation\\QA_dataimport_datadictionary.docx'")




data_dictionary <- read_excel("W:\\Project\\OSI\\Bold Vision\\Youth Thriving Survey\\Data\\Survey responses\\bvys_datadictionary_2024.xlsx")

data_dictionary <- data_dictionary %>% mutate(
  variable_name = str_to_title(variable_name),
  response_domain = str_to_title(response_domain)) %>%
  filter_all(any_vars(!is.na(.))) %>%
  mutate(primary_key = row_number()) %>%
  select(primary_key, everything())

dbWriteTable(con_bv, c('youth_thriving', 'bvys_datadictionary_2024'), data_dictionary,
             overwrite = TRUE, row.names = FALSE)

dbSendQuery(con_bv, "COMMENT ON TABLE youth_thriving.bvys_datadictionary_2024 IS 'The following data dictionary aims to decode the data for the Bold Visin Youth Thriving Survey Data for 2024. QA Documentation here: W:\\Project\\OSI\\Bold Vision\\Youth Thriving Survey\\Documentation\\QA_dataimport_datadictionary.docx'")


dbSendQuery(con_bv, "COMMENT ON COLUMN youth_thriving.bvys_datadictionary_2024.variable IS 'refers to the column label or variable in the survey data';
                     COMMENT ON COLUMN youth_thriving.bvys_datadictionary_2024.question IS 'the question that this variable refers to';
                     COMMENT ON COLUMN youth_thriving.bvys_datadictionary_2024.sub_question IS 'the subquestion that this variable refers to';
                     COMMENT ON COLUMN youth_thriving.bvys_datadictionary_2024.variable_category IS 'the categories the data collector identified in their codebook';
                     COMMENT ON COLUMN youth_thriving.bvys_datadictionary_2024.variable_name IS 'a more explanatory name of what the variable aims to measure';
                     COMMENT ON COLUMN youth_thriving.bvys_datadictionary_2024.response_domain IS 'the survey domain this variable refers to';
                     COMMENT ON COLUMN youth_thriving.bvys_datadictionary_2024.response_type IS 'the type of question this is so oe: open ended, mc: multiple choice, tf: true or false, fun: fun questions used to break up survey';
                     COMMENT ON COLUMN youth_thriving.bvys_datadictionary_2024.response_1 IS 'what the response is if the variable has a 1 coded for the data. True and False questions will also show an actual response too. So there is no 0 which is just not true but for 1, we coded what is true for the respondent';
                     COMMENT ON COLUMN youth_thriving.bvys_datadictionary_2024.response_2 IS 'what the response is if the variable has a 2 coded for the data.';
                     COMMENT ON COLUMN youth_thriving.bvys_datadictionary_2024.response_3 IS 'what the response is if the variable has a 3 coded for the data.';
                     COMMENT ON COLUMN youth_thriving.bvys_datadictionary_2024.response_4 IS 'what the response is if the variable has a 4 coded for the data.';
                     COMMENT ON COLUMN youth_thriving.bvys_datadictionary_2024.response_5 IS 'what the response is if the variable has a 5 coded for the data.';
                     COMMENT ON COLUMN youth_thriving.bvys_datadictionary_2024.response_6 IS 'what the response is if the variable has a 6 coded for the data.';
                     COMMENT ON COLUMN youth_thriving.bvys_datadictionary_2024.response_7 IS 'what the response is if the variable has a 7 coded for the data.';
                     COMMENT ON COLUMN youth_thriving.bvys_datadictionary_2024.response_8 IS 'what the response is if the variable has a 8 coded for the data.';
                     COMMENT ON COLUMN youth_thriving.bvys_datadictionary_2024.response_9 IS 'what the response is if the variable has a 9 coded for the data.';
                     COMMENT ON COLUMN youth_thriving.bvys_datadictionary_2024.response_10 IS 'what the response is if the variable has a 10 coded for the data.';
                     COMMENT ON COLUMN youth_thriving.bvys_datadictionary_2024.response_11 IS 'what the response is if the variable has a 11 coded for the data.';
                     COMMENT ON COLUMN youth_thriving.bvys_datadictionary_2024.response_12 IS 'what the response is if the variable has a 12 coded for the data.';
                     COMMENT ON COLUMN youth_thriving.bvys_datadictionary_2024.notes IS 'any important notes like what variables were just for the weights';
            ")

dbDisconnect(con_bv)
