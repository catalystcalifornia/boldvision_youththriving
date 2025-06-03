# Explore variables that need to be suppressed or made available by special request based on cell sizes <=5 or <=10

### Step 1: Set up ----
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

zip <- dbGetQuery(con, "SELECT * FROM youth_thriving.poverty_rate_data")

race <- dbGetQuery(con, "SELECT * FROM youth_thriving.race_ethnicity_data")


### Step 1: Run counts of demographics to determine small sample sizes -----
#### Check original demographics -----
# filter data dictionary
dict_demo <- dict %>% filter(response_domain %in% c("Demographics","Info"))

# omit numerical_age, omit original ZIP Code q20, and response_id
dict_demo <- dict_demo %>% filter(!variable %in% c('numerical_age','q20', 'response_id'))
var_select <- dict_demo$variable


# Create a combined frequency table
freq_df <- map_dfr(var_select, function(var) {
  raw_svy_data  %>%
    count(!!sym(var)) %>%
    mutate(Variable= var,
           Value = as.character(!!sym(var))) %>%
    select(Variable, Value, Frequency = n)
})

# Reorder columns
freq_df <- freq_df %>%
  select(Variable, Value, Frequency)

# look at variables with <=5
under_5_vars <- freq_df %>% 
  filter(Frequency<=5) %>% 
  group_by(Variable) %>% 
  summarise(num_values=n()) %>%
  left_join(dict_demo %>% select(variable, question,sub_question,response_1), by=c("Variable"="variable"))

print(under_5_vars)

# What to not include always #
# Omit ZIP Code counts <=5 (zipcode_clean_respondent) -- make NA
# Omit specified Other (ba) or Asian (bh) Race write-ins -- generalize to Other
# Omit Don't wish to answer from Race question (az) -- make NA
# Omit NHPI race disaggregation (bj, bk, bl, bm, bn, bo, bp, bq, br, bs)
# Omit write-ins to the employment and education status write-in question (bz) -- generalize to Other
# Omit original gender sexuality questions and include recoded ones only
# Omit specified Other write in for system involvement (gg) -- generalize to other
# Omit don't wish to answer from system involvement (gf) -- make NA
# Omit org where count <=5
# Omit original org_spa variable
# Omit value of 5 from immigration (q27) -- make NA
# Omit don't wish to answer from unhoused status value 4 from (q28) -- make NA
# Numerical age


# By special request #
# ZIP Code (zipcode_clean_respondent - but <=5 counts omitted 
# System involvement subquestion Q24a, but Other write in  (gg) -- generalize to other and don't wish to answer (gf) -- make NA
# Immigration status original variable (q27 - make value 5 NA) and recoded variable (undocumented)
# SOGI data
# Org but recode any counts <=5 to NA
# Unhoused status (q28 - make value 4 NA) and recoded variable (unhoused)
# System involvement question (q24) and recoded variable (systems_impacted)

#### Check recoded variables ----
# race
race_demo <- race %>% select(2:54)

var_select <- colnames(race_demo)

# Create a combined frequency table
freq_df_race <- map_dfr(var_select, function(var) {
  race  %>%
    count(!!sym(var)) %>%
    mutate(Variable= var,
           Value = as.character(!!sym(var))) %>%
    select(Variable, Value, Frequency = n)
})

# Reorder columns
freq_df_race <- freq_df_race %>%
  select(Variable, Value, Frequency)

# look at variables with <=5
under_5_vars_race <- freq_df_race %>% 
  filter(Frequency<=5) %>% 
  group_by(Variable) %>% 
  summarise(num_values=n()) %>%
  left_join(dict_demo %>% select(variable, question,sub_question,response_1), by=c("Variable"="variable"))

print(under_5_vars_race)

# Omit ba_clean, ba_original, recode ba to just include 1/0 for Other
# race_dwta recode as NA
# Omit bh_clean, bh_original, recode bh to just include 1/0 for Other
# Omit br_clean, br_original, recode br to just include 1/0 for Other
# Recode bk to Other
# Recode bs to NA
# NHPI detailed_nhpi responses - but recode <=5 to Other 
# Asian detailed_asian responses - but recode <=5 to Other
# nh_race recode nh_other and do_not_wish to NA



# Sogi
sogi_demo <- sogi %>% select(2:30)

var_select <- colnames(sogi_demo)


# Create a combined frequency table
freq_df_sogi <- map_dfr(var_select, function(var) {
  sogi  %>%
    count(!!sym(var)) %>%
    mutate(Variable= var,
           Value = as.character(!!sym(var))) %>%
    select(Variable, Value, Frequency = n)
})

# Reorder columns
freq_df_sogi <- freq_df_sogi %>%
  select(Variable, Value, Frequency)

# look at variables with <=5
under_5_vars_sogi <- freq_df_sogi %>% 
  filter(Frequency<=5) %>% 
  group_by(Variable) %>% 
  summarise(num_values=n()) %>%
  left_join(dict_demo %>% select(variable, question,sub_question,response_1), by=c("Variable"="variable"))

print(under_5_vars_sogi)

# Omit detailed_gender and detailed_sexuality

# Systems recoded
colSums(systems[, c(2:8)],na.rm=TRUE)
# no suppression needed



# Summary
## SOGI data - special request
# Omit original SOGI questions from provided data and special request data
# Omit detailed_gender and detailed_sexuality from provided data and from special request data
# Include ONLY recoded SOGI vars in special request data ("cisgender_mf", "cisgender_tgnc", "cis_trans_gnc", "cis_mf_trans_gnc", "straight_lgbqa", "cishet_lgbtqia", "lgbtqia_white_bipoc")

## Race data - provided
# Omit ba_clean, ba_original from provided data and from special request data
# Omit bh_clean, bh_original from provided data and from special request data
# Recode ba (from race_ethnicity_data) to just include 1/0 for Other in provided data
# race_dwta (from race_ethnicity_data) recode as NA in provided data
# recode (az) to NA in provided data
# Recode bh (from race_ethnicity_data) to just include 1/0 for Other in provided data
# Asian detailed_asian responses - but recode <=5 to Other in provided data
# nh_race (from race_ethnicity_data) recode nh_other and do_not_wish to NA in provided data

## NHPI data - special request
# Omit br_clean, br_original from provided data and from special request data
# Recode br (from race_ethnicity_data) to just include 1/0 for Other in special request data
# Recode bk (from race_ethnicity_data) to Other in special request data
# Recode bs (from race_ethnicity_data) to NA in special request data
# NHPI detailed_nhpi responses - but recode <=5 to Other 
# Special request data should include recoded responses from race_ethnicity_data rather than the original data


## Systems involvement -- by special request
# q24, q24a, q27, and q28 by special request with recoding by special request
# Recoded variables - undocumented, systems_impacted,unhoused by special request
# Recode specified Other write in for system involvement (q24a -> gg) -- generalize to other
# Recode don't wish to answer from system involvement (q24a -> gf) -- make NA
# Omit value of 5 from immigration (q27) -- make NA
# Omit don't wish to answer from unhoused status value 4 from (q28) -- make NA

## Other demographics -- provided data
# Omit write-ins to the employment and education status write-in question (bz) -- generalize to Other
# Omit original org_spa, org_sp_an, q20 (old zip code column) variable entirely from provided and special request data
# Numerical age omitted entirely from provided and special request data

## Geo demographics -- special request
# Organization and ZIP Code available by special request
# Omit org where count <=5 make NA
# ZIP Code (zipcode_clean_respondent), but <=5 counts recoded as NA 




