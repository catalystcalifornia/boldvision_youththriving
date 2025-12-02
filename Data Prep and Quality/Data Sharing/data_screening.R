### Prep data tables for data sharing using screening thresholds
### Tables to screen
# 1. raw_survey_data
# 2. demographics_binary_data
# 3. gender_sexuality_data
# 4. poverty_rate_data
# 5. race_ethnicity_data

# Step 0: Set up ----
library(data.table)
library(dplyr)
library(RPostgreSQL)
library(stringr)
library(purrr)
library(tidyr)

# Connect to postgres and functions
source("W:\\RDA Team\\R\\credentials_source.R")
con <- connect_to_db("bold_vision")

# Pull in the raw survey data, data, dictionary, and reformatted columns
raw_svy_data <- dbGetQuery(con, "SELECT * FROM youth_thriving.raw_survey_data")

dict <- dbGetQuery(con, "SELECT * FROM youth_thriving.bvys_datadictionary_2024")

systems <- dbGetQuery(con, "SELECT * FROM youth_thriving.demographics_binary_data")

sogi <-  dbGetQuery(con, "SELECT * FROM youth_thriving.gender_sexuality_data")

# zip <- dbGetQuery(con, "SELECT * FROM youth_thriving.poverty_rate_data")

race <- dbGetQuery(con, "SELECT * FROM youth_thriving.race_ethnicity_data")

# save copies for screening
screened_svy_data <- raw_svy_data
screened_sogi_data <- sogi
screened_race_data <- race
screened_systems_data <- systems

# Step 1: Screen raw survey data ----
### Sex at birth screen -----
# Include Sex at Birth responses (q22) but recode Don’t Wish to Answer and Other to *
# dictionary reference
dict %>% filter(variable=="q22")

# values below 10
screen_q22 <- table(screened_svy_data$q22,useNA='always') %>% as.data.frame() %>% filter(Freq<=10)

# convert values below 10 to -999
screened_svy_data <- screened_svy_data %>%
  mutate(q22=case_when(
    q22 %in% screen_q22$Var1 ~ -999,
    TRUE ~ q22
  ))

# check
table(screened_svy_data$q22,useNA='always')
table(raw_svy_data$q22,useNA='always')

### SOGI screen -----
# Omit original SOGI questions from provided data and special request data
# data dictionary for gender and sexuality vars
omit_sogi_vars <- dict %>% filter(variable_name %in% c("Gender","Sexual Orientation"))

omit_sogi_vars <- omit_sogi_vars$variable

# omit from svy data
screened_svy_data <- screened_svy_data %>%
  select(-all_of(omit_sogi_vars))

# omit from sogi data
screened_sogi_data <- screened_sogi_data %>%
  select(-all_of(omit_sogi_vars))

# Omit detailed_gender and detailed_sexuality from provided data and from special request data
# Include ONLY recoded SOGI vars in special request data ("cisgender_mf", "cisgender_tgnc", "cis_trans_gnc", "cis_mf_trans_gnc", "straight_lgbqa", "cishet_lgbtqia", "lgbtqia_white_bipoc")

# omit remaining columns from sogi data
screened_sogi_data <- screened_sogi_data %>%
  select(-c(detailed_gender,detailed_sexuality))

### Race data screen -----

## Race data - provided
# Omit ba_clean, ba_original from provided data and from special request data
# Omit bh_clean, bh_original from provided data and from special request data
# Recode ba (from race_ethnicity_data) to just include 1/0 for Other in provided data
# race_dwta (from race_ethnicity_data) do not include it
# recode (az) to * in provided data do not include it
# Recode bh (from race_ethnicity_data) to just include 1/0 for Other in provided data
# Asian detailed_asian responses - but recode <=10 to Other in provided data
# nh_race (from race_ethnicity_data) recode nh_other and do_not_wish to * in provided data
# don't include "at" indigenous latinx or "ar"
# only include race_aian_indigenous
# don't include race_other (from race_ethnicity_data)

## NHPI data - special request
# Omit br_clean, br_original from provided data and from special request data
# Recode br (from race_ethnicity_data) to just include 1/0 for Other in special request data
# Recode bk (from race_ethnicity_data) to Other in special request data
# Drop bs (from race_ethnicity_data) in special request data
# Recode bl (from race_ethnicity_data) to Other in special request data
# Recode br (from race_ethnicity_data) to Other in special request data
# NHPI detailed_nhpi responses - but recode <=10 to Other 
# Special request data should include recoded responses from race_ethnicity_data rather than the original data


## Systems involvement -- by special request
# q24, q24a, q27, and q28 by special request with recoding by special request
# Recoded variables - undocumented, systems_impacted,unhoused by special request
# Recode specified Other write in for system involvement (q24a -> gg) -- generalize to other
# Drop don't wish to answer from system involvement (q24a -> gf)
# Omit value of 5 from immigration (q27) -- make *
# Omit don't wish to answer from unhoused status value 4 from (q28) -- make *

### Other demographics screen ------
# Omit write-ins to the employment and education status write-in question (bz) -- generalize to Other
# Numerical age omitted entirely from provided and special request data
# Drop cf Don’t Wish to Answer response to "Q7. I am a full-time or part-time student. Right now I am in:" 
# Recode value of 3 (Don’t wish to answer) from q25 “Q25. At any point, have you ever been detained AND/OR arrested by law enforcement in any capactiy?" to *
# Recode value of 3 (don’t wish to answer) from q26 “Q. 26. At any point, have you ever been suspended, experienced an \"opportunity transfer\", been expelled, have been…” to *

### Geo data screen -----
# ZIP Code (zipcode_clean_respondent) by special request, but <=10 counts recoded as -999
# Omit org_spa, org_sp_an, q20 (old zip code column) variable entirely from provided and special request data
# dictionary reference
zip_code_vars <- dict %>% filter(grepl("ZIP", question, ignore.case=TRUE))
zip_code_vars$variable
# keep cleaned, omit original

# values below 10
screen_zipcode <- table(screened_svy_data$zipcode_clean_respondent,useNA='always') %>% as.data.frame() %>% filter(Freq<=10)

# convert values below 10 to -999
screened_svy_data <- screened_svy_data %>%
  mutate(zipcode_clean_respondent=case_when(
    zipcode_clean_respondent %in% screen_zipcode$Var1 ~ "-999",
    TRUE ~ zipcode_clean_respondent
  ))

# check
table(screened_svy_data$zipcode_clean_respondent,useNA='always')
table(raw_svy_data$zipcode_clean_respondent,useNA='always')

# remove original ZIP Code var and SPA vars from dataset
screened_svy_data <- screened_svy_data %>% select(-c(q20,org_spa, org_sp_an))

## Geo demographics -- special request
# Organization and ZIP Code available by special request
# Omit org where count <=10 make *
