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
  mutate(zipcode_clean_respondent = as.character(zipcode_clean_respondent)) %>%
  select(response_id, zipcode_clean_respondent)

svy_dd <- dbGetQuery(con, "SELECT * FROM youth_thriving.bvys_datadictionary_2024")

pov_data <- dbGetQuery(conn, "SELECT * FROM economic.acs_5yr_s1701_multigeo_2023")

#### Step 2: Filter for ZCTAs ####
# Example assumes `County` column indicates county name
zcta_pov_data <- pov_data %>%
  filter(geolevel == "zcta")

#### Step 3: Calculate percentages for each ZCTA####

# s1701_c01_001e | total population , s1701_c01_039e | below 125% pov level, s1701_c01_042e | below 200% pov level
final_pov_data <- zcta_pov_data %>%
  mutate(
    perc_below_125_fpl = (s1701_c01_039e / s1701_c01_001e) * 100,
    perc_below_200_fpl = (s1701_c01_042e / s1701_c01_001e) * 100,
    zcta_acs= as.character(name)
  ) %>%
  select(geoid, zcta_acs, perc_below_125_fpl, perc_below_200_fpl)

#### Step 4: Merge ACS data with respondent data####
bvyts_pov_data <- raw_svy_data %>%
  left_join(final_pov_data, by = c("zipcode_clean_respondent", "zcta_acs")) %>%
  select(response_id, zipcode_clean_respondent, zcta_acs, perc_below_125_fpl, perc_below_200_fpl)


#### Step 5: Identify unmatched ZIP codes####
# Create a flag for unmatched rows
bvyts_pov_data_unmatched <- bvyts_pov_data %>%
  mutate(
    unmatched = ifelse(is.na(perc_below_125_fpl), TRUE, FALSE)
  )

# Count the number of unmatched ZIP codes
num_unmatched <- bvyts_pov_data_unmatched %>%
  filter(unmatched) %>%
  distinct(zipcode_clean_respondent) %>%
  nrow()

total_rows <- nrow(bvyts_pov_data_unmatched)
prc_unmatched <- (num_unmatched / total_rows) * 100
# Output the result
cat("Number of unmatched ZIP codes:", num_unmatched, "\n", "Percentage of unmatched:", prc_unmatched)



#testing 
debug_join <- raw_svy_data %>%
  left_join(final_pov_data, by = c("zipcode_clean_respondent" = "zcta"), suffix = c("_respondent", "_acs"))

colnames(debug_join)  # Check all column names after the join
head(debug_join)      # View the first few rows

unmatched <- raw_svy_data %>%
  anti_join(final_pov_data, by = c("zipcode_clean_respondent" = "zcta"))

head(unmatched)
