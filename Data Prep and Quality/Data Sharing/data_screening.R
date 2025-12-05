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

### Step 1: Sex at birth screen -----
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

### Step 2: SOGI screen -----
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

### Step 3: Race data screen -----
race_vars <- dict %>% filter(variable_category=='race')

# drop race vars from the svy data as we'll send our cleaned variables in race_ethnicity_data
omit_svy_race_vars <- race_vars$variable

screened_svy_data <- screened_svy_data %>%
  select(-all_of(omit_svy_race_vars))

##### General race categories and nh_race first ----
# Omit ba_clean, ba_original from provided data and from special request data
# don't include "at" indigenous latinx or "ar", we are only including the aggregate column race_aian_indigenous
omit_race_vars <- c("ba_clean","ba_original","at","ar","race_indigenous","race_aian")

screened_race_data <- screened_race_data %>%
  select(-all_of(omit_race_vars)) %>%
  mutate(ar_at=case_when(race_aian_indigenous==1 ~ 1,
                         TRUE ~ NA))

# check
table(race$race_aian_indigenous,useNA='always')
table(screened_race_data$ar_at,useNA='always') # using NA here as opposed to 0 to follow with the column formatting for the variable columns as opposed to the recoded columns

# for nh_race (from race_ethnicity_data) recode nh_other and do_not_wish to -999 in provided data
table(screened_race_data$nh_race,useNA='always')

screened_race_data <- screened_race_data %>%
  mutate(nh_race=case_when(
    nh_race=="do_not_wish" ~ "-999",
    nh_race=="nh_other" ~ "-999",
    TRUE ~ nh_race
  ))

# check
table(screened_race_data$nh_race,useNA='always')
table(race$nh_race,useNA='always')

# don't include race_other (from race_ethnicity_data) and race_dwta in provided data, combine columns az and ba into one
omit_race_vars_2<-c("race_other","race_dwta","az","ba")

screened_race_data <- screened_race_data %>%
  mutate(az_ba=case_when(race_other==1 ~ 1,
                         race_dwta==1 ~ 1,
                         TRUE ~ NA)) %>%
  mutate(race_other_dwta=ifelse(az_ba==1, 1, 0) ) %>%
  select(-all_of(omit_race_vars_2))
  
table(screened_race_data$az_ba,useNA='always')
table(screened_race_data$race_other_dwta,useNA='always')
table(race$race_other,useNA='always')
table(race$race_dwta,useNA='always')

# recoded detailed race response below 10 to Another category
# values below 10
screen_race <- screened_race_data %>% count(detailed_race) %>% filter(n<=10)

# convert values below 10 to -999
screened_race_data <- screened_race_data %>%
  mutate(detailed_race=case_when(
    detailed_race %in% screen_race$detailed_race ~ "Another Race Alone or In Combination (suppressed)",
    TRUE ~ detailed_race
  ))

# check
sum(screen_race$n)
screened_race_data %>% filter(detailed_race=="Another Race Alone or In Combination (suppressed)") %>% count(detailed_race)

##### Asian race categories ----
# Include Asian detailed_asian responses - but recode <=10 to Other in provided data
screen_asian <- screened_race_data %>% count(detailed_asian) %>% filter(n<=10)

# convert values below 10 to -999
screened_race_data <- screened_race_data %>%
  mutate(detailed_asian=case_when(
    detailed_asian %in% screen_asian$detailed_asian ~ "Another Asian Subgroup Alone or In Combination (suppressed)",
    TRUE ~ detailed_asian
  ))

# check
sum(screen_asian$n)
screened_race_data %>% filter(detailed_asian=="Another Asian Subgroup Alone or In Combination (suppressed)") %>% count(detailed_asian)

# Omit bh_clean, bh_original from provided data and from special request data, variable #bh includes just a 1/0 response, keep that column
omit_asian <- c("bh_clean","bh_original")

screened_race_data <- screened_race_data %>%
  select(-all_of(omit_asian))

##### NHPI race categories ----
# Omit br_clean, br_original from provided data and from special request data and recode br to just include 1/NA for Other in special request data
# Drop bs (from race_ethnicity_data) in special request data
omit_nhpi_vars<-c("br_clean","br_original","bs")

screened_race_data <- screened_race_data %>%
  mutate(br=case_when(
    !is.na(br_clean) ~1,
    TRUE ~ NA
  )) %>%
  select(-all_of(omit_nhpi_vars))

sum(!is.na(race$br_clean))
sum(!is.na(screened_race_data$br))

# Recode bk, bl, and br to Other in special request data and drop original columns
omit_nhpi_vars<-c("bk","bl")

screened_race_data <- screened_race_data %>%
  mutate(br=case_when(
    !is.na(bk) ~1,
    !is.na(bl) ~1,
    TRUE ~ br
  )) %>%
  select(-all_of(omit_nhpi_vars))

sum(!is.na(race$bk))
sum(!is.na(race$bl))
sum(!is.na(screened_race_data$br))

# Include NHPI detailed_nhpi responses in special request data- but recode <=10 to Other 
screen_nhpi <- screened_race_data %>% count(detailed_nhpi) %>% filter(n<=10)

# convert values below 10 to -999
screened_race_data <- screened_race_data %>%
  mutate(detailed_nhpi=case_when(
    detailed_nhpi %in% screen_nhpi$detailed_nhpi ~ "Another NHPI Subgroup Alone or In Combination (suppressed)",
    TRUE ~ detailed_nhpi
  ))

# check
sum(screen_nhpi$n)
screened_race_data %>% filter(detailed_nhpi=="Another NHPI Subgroup Alone or In Combination (suppressed)") %>% count(detailed_nhpi)

### Step 4: Systems involvement screen ------
systems_vars <- dict %>% filter(variable_name %in% c("System Involvement","Unhoused"))

# Recode specified Other write in for system involvement (q24a -> var gg) -- generalize to other
# Drop don't wish to answer from system involvement (q24a -> var gf)
omit_systems <- c("gf")

screened_svy_data <- screened_svy_data %>%
  mutate(gg=case_when(
    !is.na(gg) ~1,
    TRUE ~ NA
  )) %>%
  select(-all_of(omit_systems))

sum(!is.na(raw_svy_data$gg))
sum(!is.na(screened_svy_data$gg))

# Omit value of 5 from immigration (q27) -- make -999
# Omit don't wish to answer from unhoused status value 4 from (q28) -- make -999
screened_svy_data <- screened_svy_data %>%
  mutate(q27=case_when(
    q27==5 ~ -999,
    TRUE ~ q27),
  q28=case_when(
    q28==4 ~ -999,
    TRUE ~ q28))

# check
table(raw_svy_data$q27)
table(screened_svy_data$q27)

# Recode value of 3 (Don’t wish to answer) from q25 “Q25. At any point, have you ever been detained AND/OR arrested by law enforcement in any capactiy?" to -999
# Recode value of 3 (don’t wish to answer) from q26 “Q. 26. At any point, have you ever been suspended, experienced an \"opportunity transfer\", been expelled, have been…” to -999
screened_svy_data <- screened_svy_data %>%
  mutate(q25=case_when(
    q25==3 ~ -999,
    TRUE ~ q25),
    q26=case_when(
      q26==3 ~ -999,
      TRUE ~ q26))

# check
table(raw_svy_data$q26)
table(screened_svy_data$q26)

### Step 5: Other demographics screen ------
# Numerical age omitted entirely from provided and special request data
screened_svy_data <- screened_svy_data %>% select(-numerical_age)

#### Employment and education status ----
empl_educ_vars <- dict %>% filter(grepl("Education",variable_name))

# Omit write-ins to the employment and education status write-in question (bz) -- generalize to Other 1/NA
screened_svy_data <- screened_svy_data %>%
  mutate(bz=case_when(
    !is.na(bz) ~1,
    TRUE ~ NA))

sum(!is.na(raw_svy_data$bz))
sum(!is.na(screened_svy_data$bz))

# Drop cf Don’t Wish to Answer response to "Q7. I am a full-time or part-time student. Right now I am in:" 
screened_svy_data <- screened_svy_data %>% select(-cf)

### Step 6: Geo data screen -----
# ZIP Code (zipcode_clean_respondent) by special request, but <=10 counts recoded as -999
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

# Omit org_spa, org_sp_an, q20 (old zip code column) variable entirely from provided and special request data
# remove original ZIP Code var and SPA vars from dataset
screened_svy_data <- screened_svy_data %>% select(-c(q20,org_spa, org_sp_an,org))


# Omit org where count <=10 make -999
screen_org <- screened_svy_data %>% count(org) %>% filter(n<=10)

# convert values below 10 to -999
screened_svy_data <- screened_svy_data %>%
  mutate(org=case_when(
    org %in% screen_org$org ~ "Another Collector (suppressed)",
    TRUE ~ org
  ))

# check
sum(screen_org$n)
screened_svy_data %>% filter(org=="Another Collector (suppressed)") %>% count(org)
