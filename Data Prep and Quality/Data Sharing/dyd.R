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

# export data
# Export to different sheets in one Excel file
write_xlsx(list(Sheet1 = svy_data, Sheet2 = race, Sheet3 = systems), path = "W:/Project/OSI/Bold Vision/Youth Thriving Survey/Data/Data Sharing/BVYTS_LACDYD_Data_Sharing_Request_120825.xlsx")

