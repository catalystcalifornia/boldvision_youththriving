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
raw_svy_data <- dbGetQuery(con, "SELECT * FROM youth_thriving.raw_survey_data") %>%
  mutate(zipcode_svy = as.character(zipcode_clean_respondent)) %>%
  select(response_id, zipcode_svy)

#filter for ZCTAs
pov_data <- dbGetQuery(conn, "SELECT * FROM economic.acs_5yr_s1701_multigeo_2023") %>%
  filter(geolevel == "zcta")

#### Step 2: Calculate percentages for each ZCTA####

# s1701_c01_001e | total population , s1701_c01_039e | below 125% pov level, s1701_c01_042e | below 200% pov level
final_pov_data <- pov_data %>%
  mutate(
    perc_below_125_fpl = (s1701_c01_039e / s1701_c01_001e) * 100,
    perc_below_200_fpl = (s1701_c01_042e / s1701_c01_001e) * 100,
    zcta_join = as.character(name),
    zcta_acs = as.character(name)
  ) %>%
  select(geoid, zcta_acs, zcta_join, perc_below_125_fpl, perc_below_200_fpl)

#### Step 3: Merge ACS data with respondent data####
bvyts_pov_data <- raw_svy_data %>%
  left_join(final_pov_data, by = c("zipcode_svy" = "zcta_join")) %>%
  select(response_id, zipcode_svy, zcta_acs, perc_below_125_fpl, perc_below_200_fpl)


#### Step 4: Identify unmatched ZIP codes####
# Create a flag for unmatched rows
bvyts_pov_data_unmatched <- bvyts_pov_data %>%
  mutate(
    unmatched = ifelse(is.na(perc_below_125_fpl), TRUE, FALSE)
  )

# Count the number of unmatched ZIP codes
num_unmatched <- bvyts_pov_data_unmatched %>%
  filter(unmatched) %>%
  distinct(zipcode_svy) %>%
  nrow()

total_rows <- nrow(bvyts_pov_data_unmatched)
prc_unmatched <- (num_unmatched / total_rows) * 100
# Output the result
cat("Number of unmatched ZIP codes:", num_unmatched, "\n", "Percentage of unmatched:", prc_unmatched)

#### Step 5: Push datatable to postgres ####
source("W:\\RDA Team\\R\\Github\\RDA Functions\\main\\RDA-Functions\\Utility_Functions.R")

table_name <- "povery_rates_acs_23"
schema <- "youth_thriving"
indicator <- "Poverty level rates of the zipcodes respondents reported they live in. 
We calculated these rates by joining ACS Table S1701 ZCTAs to the survey data zipcodes.
One indicates the rate of those living below the 125% poverty level and below 200% poverty level."
source <- "Script: W:/Project/OSI/Bold Vision/Youth Thriving Survey/GitHub/MK/boldvision_youththriving/Data Prep and Quality/calc_poverty.R "
qa_filepath<-" See QA doc for details: W:/Project/OSI/Bold Vision/Youth Thriving Survey/Documentation/QA_calc_poverty.docx "
table_comment <- paste0(indicator, source)
dbWriteTable(con, c(schema, table_name), bvyts_pov_data,
            overwrite = FALSE, row.names = FALSE)

# Add metadata 
column_names <- colnames(bvyts_pov_data) # Get column names
column_comments <- c(
  'respondent id',
  'zip code from survey data, reported by each survey respondent on where they live',
  'zcta from ACS data table S1701, 2019-2023',
  'Rate of poeple living under 125% poverty level in the associated zipcode.',
  'Rate of poeple living under 200% poverty level in the associated zipcode.')

add_table_comments(con, schema, table_name, indicator, source, qa_filepath, column_names, column_comments)

#### Step 6: Close Connections ####
dbDisconnect(con)
dbDisconnect(conn)