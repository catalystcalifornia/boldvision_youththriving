### Prep data tables to share with DYD based on data sharing request
## Requesting: ZIP Code, immigrant youth, ever unhoused, ever systems impacted, systems impacted category
## Not requesting: SOGI data, NHPI origin, Community org

# Step 0: Set up ----
library(data.table)
library(dplyr)
library(RPostgreSQL)
library(stringr)
library(purrr)
library(tidyr)
library(writexl)

# Connect to postgres and functions
source("W:\\RDA Team\\R\\credentials_source.R")
con <- connect_to_db("bold_vision")

dict <- dbGetQuery(con, "SELECT * FROM youth_thriving.bvys_datadictionary_2024")

# Prep data for export -------
# Pull in screened svy data and filter for the variables they want
svy_data <- dbGetQuery(con, "SELECT * FROM bvyts_data_sharing.screened_svy_data")

svy_data <- svy_data %>%
  select(-org) # not requesting org but requesting ZIP Code

systems <- dbGetQuery(con, "SELECT * FROM bvyts_data_sharing.screened_systems_unhoused_imgrtion_data")

# not requested
# sogi <-  dbGetQuery(con, "SELECT * FROM bvyts_data_sharing.screened_sogi_data")

race <- dbGetQuery(con, "SELECT * FROM bvyts_data_sharing.screened_race_data")

# not requesting nhpi detail
nhpi_vars <- dict %>% filter(variable_name=='Race NHPI Subgroup') %>%
  filter(!variable %in% c("bk","bl","bs")) # vars omitted from screened data

race <- race %>%
  select(-all_of(nhpi_vars$variable))

# sort variables
race <- race %>%
  select(sort(colnames(race))) %>%
  select(response_id,acs_race,nh_race,everything())

# export data
# # Export to different sheets in one Excel file
# data<- list("svy_data" = svy_data, "race_data" = race,"systems_data"=systems)
# write_xlsx(data, path = "W:/Project/OSI/Bold Vision/Youth Thriving Survey/Data/Data Sharing/BVYTS_LACDYD_Data_Sharing_Request_120825.xlsx")

# Prep data dictionaries for export -------
# Pull in screened svy data and filter for the variables they want
svy_dict <- dbGetQuery(con, "SELECT * FROM bvyts_data_sharing.screened_svy_data_dictionary")

svy_dict <- svy_dict %>%
  filter(variable %in% colnames(svy_data)) # select only variable entries that are in the data requested

# rearrange dictionary based on dataframe
var_order <- colnames(svy_data)
svy_dict <- svy_dict %>% arrange(factor(variable, levels = var_order))


systems_dict <- dbGetQuery(con, "SELECT * FROM bvyts_data_sharing.screened_systems_unhoused_imgrtion_data_dictionary")

systems_dict <- systems_dict %>%
  filter(column_name %in% colnames(systems)) %>% # select only variable entries that are in the data requested
  rename(variable=column_name,
         description=column_comment)

# not requested
# sogi_dict <-  dbGetQuery(con, "SELECT * FROM bvyts_data_sharing.screened_sogi_data_dictionary")

race_dict <- dbGetQuery(con, "SELECT * FROM bvyts_data_sharing.screened_race_data_dictionary")

race_dict <- race_dict %>%
  filter(column_name %in% colnames(race)) %>% # select only variable entries that are in the data requested
  rename(variable=column_name,
         description=column_comment)

race %>%
  select(-any_of(race_dict$variable)) %>%
  colnames()

# Add columns without definitions
race_dict_added <- data.frame(variable=c('ar_at','az_ba','race_other_dwta'),
                             data_type=c('double precision','double precision','double precision'),
                             description=c('1 flags respondents that selected or were recoded as American Indian/Alaska Native or indigenous from Mexico, Central America, or South America',
                                           '1 flags respondents that selected or were recorded to Other Race or Dont Wish to Answer to the Race Question',
                                           '0/1 flag for whether the respondent is marked as Other Race or Dont Wish to Answer to the Race Question'))

race_dict <- bind_rows(race_dict,race_dict_added)

# rearrange dictionary based on dataframe
var_order <- colnames(race)
race_dict <- race_dict %>% arrange(factor(variable, levels = var_order))

# Flag vars screened or removed for privacy
# vars screened or combined for privacy for values <=10
screened_vars <- c("q22","nh_race","detailed_race","detailed_asian","ar_at","az_ba", "race_other_dwta","q27","q28","q25","q26","zipcode_clean_respondent","org")

# vars removed from request - note in added readme to Excel
readme <- data.frame(Date="12-15-25",
`Prepared For`="LA County Department of Youth Development",
`Courtesy of`="Catalyst California and Bold Vision",
Citation="2024 Bold Vision Youth Thriving Survey, Catalyst California and the Social Justice Learning Institute",
Notes="Some information collected in the survey has been omitted for privacy purposes.This includes: 
Respondents' original responses to gender identity and sexual orientation questions (Q1 & Q23).
Respondent write-ins to specific questions like race (Q3), Asian ethnicity (Q4), systems involvement (Q24a), and employment and education status (Q6).
Responses to sub-questions about Native Hawaiian or Pacific Islander ethnicity (Q5).
Don't wish to answer responses related to systems involvement (Q24a) and full-time or part-time student status (Q7).
Lastly, detailed numeric age for each respondent has been removed.

For data screened but not omitted, the value -999 indicates a response has been suppressed.
Note for race data, we included our cleaned data that has recoded write-in responses for the appropriate race category where applicable.
Before sharing this data or using it for other purposes other than requested, please contact Elycia Mulholland Graves - egraves@catalystcalifornia.org")

svy_dict <- svy_dict %>%
  mutate(screened_flag=ifelse(variable %in% screened_vars, "Yes",NA)) %>%
  select(-notes,-likert,-likert_type,-primary_id)

race_dict <- race_dict %>%
  mutate(screened_flag=ifelse(variable %in% screened_vars, "Yes",NA)) 

systems_dict <- systems_dict %>%
  mutate(screened_flag=ifelse(variable %in% screened_vars, "Yes",NA)) 

# # export dictionaries
# # Export dictionaries to different sheets in one Excel file
# dictionaries <- list("ReadMe"=readme, "svy_dict" = svy_dict, "race_dict" = race_dict,"systems_dict"=systems_dict)
# write_xlsx(dictionaries, path = "W:/Project/OSI/Bold Vision/Youth Thriving Survey/Data/Data Sharing/BVYTS_LACDYD_Data_Sharing_Request_Dictionaries_120825.xlsx")
# Reviewed files and did some formatting cleanup on the readme