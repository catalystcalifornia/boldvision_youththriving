# Importing Data and Data Dictionary to PGAMIN Database 
## Script imports and cleans data & data dictionary, including re-calculating weights, adding new columns for likert scales, and improving consistency in variable values in data dictionary


####Step 1: Set Up ####
packages <- c("dplyr", "RPostgreSQL", "usethis", "readxl", "janitor", "stringr") 

for(pkg in packages){
  library(pkg, character.only = TRUE)
}

options(scipen = 100) # disable scientific notation

# create connection for bold_vision
source("W:\\RDA Team\\R\\credentials_source.R")
con_bv <- connect_to_db("bold_vision") 


#### Step 2a: Read in dataset and clean column names ####
## Read in data
ys_data <- read_excel("W:\\Project\\OSI\\Bold Vision\\Youth Thriving Survey\\Data\\Survey responses\\Updated - 09252024\\BVYTSDatabase_Updated.xlsx", sheet="Data Base")

ys_data <- clean_names(ys_data)

# check for NA columns
columns_na_counts <- lapply(ys_data, function(x) sum(is.na(x)))%>%
  as.data.frame()%>%
  pivot_longer(everything(), names_to="variable",values_to="na_count")

# exclude extra blank columns
ys_data <- ys_data %>%
  select(-c(x155,x159))

colnames(ys_data)[grepl('q12a_how_true_is_this_about_you',colnames(ys_data))] <- 'q12a'
colnames(ys_data)[grepl('q10b_which_of_the_following_',colnames(ys_data))] <- 'q10b'
colnames(ys_data)[grepl('q10a_how_many_adults_really_',colnames(ys_data))] <- 'q10a'


#### Step 2b: Correct skip-logic failures ####
# Using paper survey as reference: 
# W:\Project\OSI\Bold Vision\Youth Thriving Survey\Data\Survey responses\Updated - 09252024\BVYTS_PaperSurvey_Updated.pdf
# Responses to correct: q7 (from q6), q10a and 10b (from q10), q12a (from q12), q24a (from q24) 
# Note: q4 and q5 corrections are addressed in race recode script

# Before correcting, check that no other values are possible (i.e., only those in the in the paper survey or data dictionary)
# q6/bv (Full-time student) - 1 or NA; sums to 3444
addmargins(table(ys_data$bv, useNA = "ifany")) %>%
  as.data.frame(.) %>%
  View()

# q6/bw (Part-time student) - 1 or NA; sums to 3444
addmargins(table(ys_data$bw, useNA = "ifany")) %>%
  as.data.frame(.) %>%
  View()

# q10 - 1, 2, 3, or 4; sums to 3444
addmargins(table(ys_data$q10, useNA = "ifany")) %>%
  as.data.frame(.) %>%
  View()

# q10a - 1, 2, 3, 4, 5, 6, or NA; sums to 3444
addmargins(table(ys_data$q10a, useNA = "ifany")) %>%
  as.data.frame(.) %>%
  View()

# q10b - 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, or NA; sums to 3444
addmargins(table(ys_data$q10b, useNA = "ifany")) %>%
  as.data.frame(.) %>%
  View()

# q12 - 1 or 2; sums to 3444
addmargins(table(ys_data$q12, useNA = "ifany")) %>%
  as.data.frame(.) %>%
  View()

# q24 - 1, 2, 3, 4, or NA; sums to 3444
addmargins(table(ys_data$q24, useNA = "ifany")) %>%
  as.data.frame(.) %>%
  View()

# q24a/ga - 1 or NA
addmargins(table(ys_data$ga, useNA = "ifany")) %>%
  as.data.frame(.) %>%
  View()

# make copy of ys_data for QA checks
ys_data_corrected <- ys_data

# Make the corrections:
# Q7: if bv != 1 and bw != 1 then ca:cf are null
# any cases of this: 81 responses (e.g., response_id == 136135266)
q6_cols_logiccheck <- c("bv", "bw")
q7_cols_tocorrect <- c("ca", "cb", "cc", "cd", "ce", "cf")
check_q6_q7 <- ys_data_corrected %>%
  select(response_id, all_of(q6_cols_logiccheck), all_of(q7_cols_tocorrect)) %>%
  # take sum of q7 columns (1 or NA) to gauge if respondent answered q7
  mutate(q7_response = select(., all_of(q7_cols_tocorrect)) %>% 
           rowSums(na.rm = TRUE)) %>%
  filter(is.na(bv) & is.na(bw) & q7_response > 0)

# make the q6/q7 correction
ys_data_corrected <- ys_data_corrected %>%
  mutate(across(all_of(q7_cols_tocorrect), ~ifelse((is.na(bv) & is.na(bw)), NA, .)))
  

# Q10a: if q10 == 2 (No) - in other words q10 != 1 (Yes) & q10 != 3 (Not sure) & q10 != 4 (Don't wish to answer) then q10a is null
# any cases of this: None - not addressed
check_q10_q10a <- ys_data_corrected %>%
  select(response_id, q10, q10a) %>%
  filter((q10 == 2) & !is.na(q10a))


# Q10b: if q10 != 2 (No) then q10b is null
# any cases of this: None - not addressed
check_q10_q10b <- ys_data_corrected %>%
  select(response_id, q10, q10b) %>%
  filter(q10 != 2 & !is.na(q10b))


# Q12a: if q12 != 1 (Yes) then q12a is null
# any cases of this: 61 responses (e.g., response_id == 136183460)
check_q12_q12a <- ys_data_corrected %>%
  select(response_id, q12, q12a) %>%
  filter(q12 != 1 & !is.na(q12a))

# make the q12/q12a correction
ys_data_corrected <- ys_data_corrected %>%
  mutate(q12a = ifelse(q12 != 1, NA, .))


# Q24a: if q24 != 1 (Yes) then q24a is null 
# any cases of this: None - not addressed
q24a_cols_tocorrect <- c("ga", "gb", "gc", "gd", "ge", "gf", "gg")
check_q24_q24a <- ys_data_corrected %>%
  select(response_id, q24, all_of(q24a_cols_tocorrect)) %>%
  mutate(
    # convert gg (other write-in) to 1/NA to take row sums 
    gg = ifelse(is.na(gg) == TRUE, as.numeric(NA), as.numeric(1))) %>%
  mutate(
    gg = as.numeric(gg),
    # take sum of q24a columns (1 or NA) to gauge if respondent answered q24a
    q24a_response = select(., all_of(q24a_cols_tocorrect)) %>% 
           rowSums(na.rm = TRUE)) %>%
  filter(q24 != 1 & q24a_response > 0)


#### Step 3: Add adjusted weights ####
# remove original weighting cols from vendor (note: will reuse column names in table exported to pg)
ys_data_corrected <- ys_data_corrected %>%
  select(-c(weights_a1, weights_a2, weights_a3, weights_final))

# read in acs population weights (age, sex, SPA)
acs_pop_weights <- dbGetQuery(conn=con_bv, statement="SELECT * FROM youth_thriving.acs_weighting_population_counts;")

###### 1. add age_wt col to survey data -----
acs_age_pop <- acs_pop_weights %>%
  filter(weighting_group=='Age') %>%
  rename(pop_count = count,
         pop_rate = percent)

acs_age_pop$variable <- str_replace(acs_age_pop$variable, "15-17","1")
acs_age_pop$variable <- str_replace(acs_age_pop$variable, "18-24", "2")

acs_age_sample <- ys_data_corrected %>%
  select(age_minor_adult) %>%
  table(., useNA = "ifany") %>%
  as.data.frame() %>%
  mutate(age_minor_adult = as.character(age_minor_adult)) %>%
  rename(sample_count = Freq) %>%
  mutate(sample_rate = sample_count/sum(sample_count))

age_weights <- acs_age_pop %>%
  left_join(acs_age_sample, by=c('variable'='age_minor_adult')) %>%
  mutate(
    weights = pop_rate/sample_rate,
    weighted_count = sample_count * weights,
    weighted_rate = weighted_count / sum(sample_count),
    variable = as.numeric(variable)
  )

ys_data_agewts <- ys_data_corrected %>%
  left_join(select(age_weights, variable, weights), by=c("age_minor_adult"="variable")) %>%
  rename(age_wt = weights)

##### 2. add race_wt col to survey data -----
# read in acs population weights (race)
acs_population_races <- c("latino", "nh_white", "nh_black", "nh_asian", "nh_aian",
                          "nh_pacisl", "nh_twoormor", "nh_other")

acs_race_pop_weights <- acs_pop_weights %>%
  filter(weighting_group=='Race') %>%
  filter(variable %in% acs_population_races) %>%
  select(geoid, variable, count, percent) %>%
  group_by(variable) %>%
  summarise(pop_count = sum(count)) %>%
  ungroup() %>%
  mutate(pop_rate = pop_count/sum(pop_count))

# read in race recode values for final race sample counts
race_recode <- dbGetQuery(conn=con_bv, statement="SELECT response_id, acs_race FROM youth_thriving.race_ethnicity_data;") %>%
  mutate(acs_race = as.character(acs_race))

race_recode_counts <- race_recode %>%
  select(acs_race) %>%
  table(., useNA = "ifany") %>%
  as.data.frame() 

acs_race_sample_weights <- race_recode_counts %>%
  rename(race = acs_race) %>%
  group_by(race) %>%
  summarise(Freq = sum(Freq)) %>%
  ungroup() %>%
  rename(sample_count=Freq) %>%
  mutate(sample_rate = sample_count/sum(sample_count))

race_weights <- acs_race_pop_weights %>%
  left_join(acs_race_sample_weights, by=c("variable"="race")) %>%
  mutate(
    weights = pop_rate/sample_rate,
    weighted_count = sample_count * weights,
    weighted_rate = weighted_count / sum(sample_count))

# add race_weight col
ys_data_racewts <- ys_data_agewts %>%
  left_join(select(race_recode, response_id, acs_race)) %>%
  left_join(select(race_weights, variable, weights), by=c("acs_race"="variable")) %>%
  rename(race_wt = weights)

##### 3. add sex_wt col to survey data ----
acs_sex_pop <- acs_pop_weights %>%
  filter(weighting_group=='Sex at birth') %>%
  rename(pop_count = count,
         pop_rate = percent)

acs_sex_pop$variable <- str_replace(acs_sex_pop$variable, "Male","1")
acs_sex_pop$variable <- str_replace(acs_sex_pop$variable, "Female", "2")

acs_sex_sample <- ys_data_corrected %>%
  select(q22) %>%
  table(., useNA = "ifany") %>%
  as.data.frame() %>%
  filter(q22==1 | q22==2) %>%
  mutate(q22 = as.character(q22)) %>%
  rename(sample_count = Freq) %>%
  mutate(sample_rate = sample_count/sum(sample_count))

sex_weights <- acs_sex_pop %>%
  left_join(acs_sex_sample, by=c('variable'='q22')) %>%
  mutate(
    weights = pop_rate/sample_rate,
    weighted_count = sample_count * weights,
    weighted_rate = weighted_count / sum(sample_count),
    variable = as.numeric(variable)
  )

ys_data_sexwts <- ys_data_racewts %>%
  left_join(select(sex_weights, variable, weights), by=c("q22"="variable")) %>%
  rename(sex_wt = weights)

##### 4. add spa_wt col to survey data ----
acs_spa_pop <- acs_pop_weights %>%
  filter(weighting_group=='SPA') %>%
  rename(pop_count = count,
         pop_rate = percent) %>%
  mutate(variable = as.numeric(str_replace_all(variable, "SPA ", "")))

acs_spa_sample <- ys_data_corrected %>%
  select(spa_final_respondent) %>%
  table(.) %>%
  as.data.frame() %>%
  rename(sample_count = Freq) %>%
  mutate(sample_rate = sample_count/sum(sample_count),
         spa_final_respondent = as.numeric(spa_final_respondent))

spa_weights <- acs_spa_pop %>%
  left_join(acs_spa_sample, by=c('variable'='spa_final_respondent')) %>%
  mutate(
    weights = pop_rate/sample_rate,
    weighted_count = sample_count * weights,
    weighted_rate = weighted_count / sum(sample_count)
  )

ys_data_spawts <- ys_data_sexwts %>%
  left_join(select(spa_weights, variable, weights), by=c("spa_final_respondent"="variable")) %>%
  rename(spa_wt = weights)

##### 5. Calculate final weights ----
# calculate weights_a1, weights_a2, weights_a3, and weights_final
# replace NAs in _wt cols with 1
ys_data_finalwts <- ys_data_spawts %>%
  mutate(across(ends_with("_wt"), ~replace_na(., 1))) %>%
  mutate(
    weights_a1 = age_wt,
    weights_a2 = age_wt * race_wt,
    weights_a3 = age_wt * race_wt * sex_wt,
    weights_final = age_wt* race_wt * sex_wt * spa_wt
  )

# save to csv for QA
# write.csv(ys_data_finalwts, file = "./Data Prep/Survey Weighting/ys_data_finalwts.csv", row.names=FALSE, fileEncoding = "UTF-8")

####Step 4: Export survey data to postgres ####

# remove _wt cols, etc 
ys_data_finalwts <- ys_data_finalwts %>%
  select(-c(age_wt, acs_race, race_wt, sex_wt, spa_wt))

# # export survey data table and comments
# dbWriteTable(con_bv, c('youth_thriving', 'raw_survey_data'), ys_data_finalwts,
#                           overwrite = FALSE, row.names = FALSE)

# dbSendQuery(con_bv, paste0("COMMENT ON TABLE youth_thriving.raw_survey_data IS
#             'The following dataset are responses from the Youth Thriving Survey conducted by Bold Vision in 2024. The data dictionary explaining each variable is here: youth_thriving.bvys_datadictionary_2024 .
#             Steps explaining data cleaning can be found here: W:\\Project\\OSI\\Bold Vision\\Youth Thriving Survey\\Data\\Survey responses\\Updated - 09252024\\BVYTSPopulationWeighting_DataCleaning.pdf
#             Original Dataset is here: a)	W:\\Project\\OSI\\Bold Vision\\Youth Thriving Survey\\Data\\Survey responses\\Updated - 09252024\\BVYTSWeightSummary_Database.xlsx
#             The process for cleaning and uploading the data is explained in the QA Documentation here: W:\\Project\\OSI\\Bold Vision\\Youth Thriving Survey\\Documentation\\QA_dataimport_datadictionary.docx
#             Script for cleaning data and recalculating sample weights can be found here Data Prep and Quality/importing_data.R. ", "Table last updated: ", Sys.Date(), "'"))

# UPDATE THIS LINE EVERY TIME DATA IS REPUSHED Survey data update date in postgres 11-25-24-

####Step 5: Read in data dictionary and send to database comments ####

data_dictionary <- read_excel("W:\\Project\\OSI\\Bold Vision\\Youth Thriving Survey\\Data\\Survey responses\\bvys_datadictionary_2024.xlsx")

data_dictionary <- data_dictionary %>% 
  mutate(
    variable_name = str_to_title(variable_name),
    response_domain = str_to_title(response_domain)) %>%
  filter_all(any_vars(!is.na(.))) %>%
  mutate(
    primary_key = row_number()) %>%
  select(primary_key, everything())

# generalize variables in Caring Families and Relationships to align with conceptboard
data_dictionary$variable_name[data_dictionary$variable_name %in% c("Support: Adults, Family, Peers (Aligns With Youth Voice)","School Relationships")] <- "Relationships/Support"

# match Demographic to Demographics in response domains
data_dictionary$response_domain[data_dictionary$response_domain=="Demographic"] <- "Demographics"

# specify Info domain for org
data_dictionary$response_domain[data_dictionary$variable=="org"] <- "Info"

# do some other clean up for consistent labels
data_dictionary <- data_dictionary %>%
  mutate(
    variable_name=str_replace(variable_name," Nhpi "," NHPI "),
    variable_name=str_replace(variable_name,"Spa ","SPA "))


data_dictionary[data_dictionary=="Don't Wish to Answer"]<-"Don't wish to answer"
data_dictionary[data_dictionary=="Don't Wish to Answer"]<-"Don't wish to answer"
data_dictionary[data_dictionary=="Dont wish to answer"]<-"Don't wish to answer"
data_dictionary[data_dictionary=="Dont Wish to Answer"]<-"Don't wish to answer"
data_dictionary[data_dictionary=="dont wish to answer"]<-"Don't wish to answer"
data_dictionary[data_dictionary=="I am not sure or Questioning"]<-"I am not sure or questioning"
data_dictionary[data_dictionary=="Dont know"]<-"Don't know"
data_dictionary[data_dictionary=="Don't Know"]<-"Don't know"
data_dictionary[data_dictionary=="Dont Know"]<-"Don't know"
data_dictionary[data_dictionary=="South West Asian and North African"]<-"Southwest Asian and North African"
data_dictionary[data_dictionary=="some of the time"]<-"Some of the time"
data_dictionary[data_dictionary=="most of the time"]<-"Most of the time"
data_dictionary[data_dictionary=="all of the time"]<-"All of the time"
data_dictionary[data_dictionary=="Never True"]<-"Never true"
data_dictionary[data_dictionary=="Sometimes True"]<-"Sometimes true"
data_dictionary[data_dictionary=="Often True"]<-"Often true"
data_dictionary[data_dictionary=="Always True"]<-"Always true"

# # Export data dictionary table and comments
#  dbWriteTable(con_bv, c('youth_thriving', 'bvys_datadictionary_2024'), data_dictionary,
#               overwrite = FALSE, row.names = FALSE)

 # dbSendQuery(con_bv, "COMMENT ON TABLE youth_thriving.bvys_datadictionary_2024 IS 'The following data dictionary aims to decode the data for the Bold Vision Youth Thriving Survey Data for 2024. QA Documentation here: W:\\Project\\OSI\\Bold Vision\\Youth Thriving Survey\\Documentation\\QA_dataimport_datadictionary.docx'")
 # 
 # 
 # dbSendQuery(con_bv, "COMMENT ON COLUMN youth_thriving.bvys_datadictionary_2024.variable IS 'refers to the column label or variable in the survey data';
 #                      COMMENT ON COLUMN youth_thriving.bvys_datadictionary_2024.question IS 'the question that this variable refers to';
 #                      COMMENT ON COLUMN youth_thriving.bvys_datadictionary_2024.sub_question IS 'the subquestion that this variable refers to';
 #                      COMMENT ON COLUMN youth_thriving.bvys_datadictionary_2024.variable_category IS 'the categories the data collector identified in their codebook';
 #                      COMMENT ON COLUMN youth_thriving.bvys_datadictionary_2024.variable_name IS 'a more explanatory name of what the variable aims to measure';
 #                      COMMENT ON COLUMN youth_thriving.bvys_datadictionary_2024.response_domain IS 'the survey domain this variable refers to';
 #                      COMMENT ON COLUMN youth_thriving.bvys_datadictionary_2024.response_type IS 'the type of question this is so oe: open ended, mc: multiple choice, tf: true or false, fun: fun questions used to break up survey';
 #                      COMMENT ON COLUMN youth_thriving.bvys_datadictionary_2024.response_1 IS 'what the response is if the variable has a 1 coded for the data. True and False questions will also show an actual response too. So there is no 0 which is just not true but for 1, we coded what is true for the respondent';
 #                      COMMENT ON COLUMN youth_thriving.bvys_datadictionary_2024.response_2 IS 'what the response is if the variable has a 2 coded for the data.';
 #                      COMMENT ON COLUMN youth_thriving.bvys_datadictionary_2024.response_3 IS 'what the response is if the variable has a 3 coded for the data.';
 #                      COMMENT ON COLUMN youth_thriving.bvys_datadictionary_2024.response_4 IS 'what the response is if the variable has a 4 coded for the data.';
 #                      COMMENT ON COLUMN youth_thriving.bvys_datadictionary_2024.response_5 IS 'what the response is if the variable has a 5 coded for the data.';
 #                      COMMENT ON COLUMN youth_thriving.bvys_datadictionary_2024.response_6 IS 'what the response is if the variable has a 6 coded for the data.';
 #                      COMMENT ON COLUMN youth_thriving.bvys_datadictionary_2024.response_7 IS 'what the response is if the variable has a 7 coded for the data.';
 #                      COMMENT ON COLUMN youth_thriving.bvys_datadictionary_2024.response_8 IS 'what the response is if the variable has a 8 coded for the data.';
 #                      COMMENT ON COLUMN youth_thriving.bvys_datadictionary_2024.response_9 IS 'what the response is if the variable has a 9 coded for the data.';
 #                      COMMENT ON COLUMN youth_thriving.bvys_datadictionary_2024.response_10 IS 'what the response is if the variable has a 10 coded for the data.';
 #                      COMMENT ON COLUMN youth_thriving.bvys_datadictionary_2024.response_11 IS 'what the response is if the variable has a 11 coded for the data.';
 #                      COMMENT ON COLUMN youth_thriving.bvys_datadictionary_2024.response_12 IS 'what the response is if the variable has a 12 coded for the data.';
 #                      COMMENT ON COLUMN youth_thriving.bvys_datadictionary_2024.notes IS 'any important notes like what variables were just for the weights and whether skip logic was applied';
 #                      COMMENT ON COLUMN youth_thriving.bvys_datadictionary_2024.likert IS 'notes if a Likert scale was used and how many points/options given (e.g., 5pt_scale, 4pt_scale, etc.)';
 #                      COMMENT ON COLUMN youth_thriving.bvys_datadictionary_2024.likert_type IS 'notes what type of likert scale is used (e.g., yes_scale, freq_scale, etc.)';
 #             ")
 # 
 # # add primary key for editing in database
 # dbSendQuery(con_bv, "ALTER TABLE youth_thriving.bvys_datadictionary_2024 ADD PRIMARY KEY (primary_key)");
 # 
 # dbDisconnect(con_bv)

# QA check: survey columns are equal to data dictionary variables
survey_colnames <- sort(colnames(ys_data_finalwts))
datadict_variables <- sort(data_dictionary$variable)

identical(survey_colnames, datadict_variables) 
setdiff(survey_colnames, datadict_variables) 


#  UPDATE THIS LINE EVERY TIME DICTIONARY IS REPUSHED Data dictionary update date in postgres 11-5-24