# The purpose of this script is to calculate poverty rates for each respondent by their designated zipcode. Final calculations are produced as a datatable and pushed to pgAdmin database. 
# Author: Maria Khan 

#### Step 1: Set up ####
library(data.table)
library(dplyr)
library(RPostgreSQL)
library(purrr)
library(stringr)
library(tibble)

# Connect to postgres and functions
source("W:\\RDA Team\\R\\credentials_source.R")
con <- connect_to_db("bold_vision")
conn <- connect_to_db("rda_shared_data") 

# Pull in the raw survey data and data dictionary
raw_svy_data <- dbGetQuery(con, "SELECT * FROM youth_thriving.raw_survey_data")

svy_dd <- dbGetQuery(con, "SELECT * FROM youth_thriving.bvys_datadictionary_2024")

pov_data <- dbGetQuery(conn, "SELECT * FROM economic.acs_5yr_s1701_multigeo_2023")
