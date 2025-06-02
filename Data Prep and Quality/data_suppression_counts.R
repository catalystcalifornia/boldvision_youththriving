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

# look at variables with less than 5

under_5_vars <- freq_df %>% 
  filter(Frequency<=5) %>% 
  group_by(Variable) %>% 
  summarise(num_values=n()) %>%
  left_join(dict_demo %>% select(variable, question,sub_question,response_1), by=c("Variable"="variable"))

print(under_5_vars)

# What to not include always
# Omit ZIP Code counts <=5 (zipcode_clean_respondent) -- make NA
# Omit specified Other (ba) or Asian (bh) Race write-ins -- generalize to Other
# Omit Don't wish to answer from Race question (az) -- make NA
# Omit NHPI race disaggregation (bj, bk, bl, bm, bn, bo, bp, bq, br, bs, detailed_nhpi)
# Omit write-ins to the employment and education status write-in question (bz) -- generalize to Other
# Omit original gender sexuality questions and include recoded ones only
# Omit specified Other write in for system involvement (gg) -- generalize to other
# Omit don't wish to answer from system involvement (gf) -- make NA
# Omit org where count <=5
# original org_spa variable
# Omit value of 5 from q27 -- make NA
# Omit don't wish to answer from unhoused status value 4 from (q28) -- make NA


# By special request
# ZIP Code (zipcode_clean_respondent - but <=5 counts omitted 
# NHPI disaggregation recoded responses - but recode <=5 to Other or NA (don't wish to answer)
# System involvement subquestion Q24a, but Other write in  (gg) -- generalize to other and don't wish to answer (gf) -- make NA
# Immigration status (q27 - make value 5 NA and recoded variable - undocumented)
# sogi data
# Org but recode any counts <=5
# Unhoused status (q28 - make value 4 NA and recoded variable - unhoused)
# System involvement question and recoded variable ()







