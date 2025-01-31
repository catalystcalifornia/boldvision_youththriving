# Recoding select demographic variables to binary variables for regression and analysis
## Variables to recode: BIPOC/non-BIPOC, Connected Youth/Disconnected youth, systems-involved, undocumented, unhoused, ever suspended/expelled, ever arrested


#### Step 1: Set Up -----
packages <- c("dplyr", "RPostgreSQL", "usethis", "janitor", "stringr","purrr","tidyverse") 

for(pkg in packages){
  library(pkg, character.only = TRUE)
}

options(scipen = 100) # disable scientific notation

# create connection for bold_vision
source("W:\\RDA Team\\R\\credentials_source.R")
con <- connect_to_db("bold_vision") 

# pull in data and data dictionary
svy_data_raw <- dbGetQuery(con, "SELECT * FROM youth_thriving.raw_survey_data") 
svy_dd <- dbGetQuery(con, "SELECT * FROM youth_thriving.bvys_datadictionary_2024")

# Step 2: Recode variables to binary vars ----

##### Systems-impacted related variables -------
# convert systems impacted related variables into 0/1 where 1 means yes and 0 means no, dont knows and dont wish to answer recoded to NA
binary_systems_imp <- svy_data_raw %>%
  select(response_id, q24,q25, q26, q27, q28) %>%
  mutate_at(vars(q24:q28), .funs=list(
    ~case_when(
      .==1 ~ 1,
      .==2 ~ 0,
      .>=3 ~ NA,
      TRUE ~ NA)))%>%
  rename(systems_impacted = q24,
         arrested = q25,
         suspended = q26 ,
         undocumented = q27,
         unhoused = q28 )

# qa check
table(svy_data_raw$q24,useNA='always')
table(binary_systems_imp$systems_impacted,useNA='always')

##### Recode race ethnicity to bipoc/non-bipoc ----
race <-  dbGetQuery(con, "SELECT * FROM youth_thriving.race_ethnicity_data") 

# reorder columns and rename
binary_bipoc <- race %>% 
  select(response_id, nh_race,race_black, race_aian_indigenous, race_latinx, race_asian, race_swana, race_nhpi, race_white) %>% 
  mutate(bipoc = ifelse(nh_race == 'nh_white', 0, 
                        ifelse(nh_race %in% c("do_not_wish", "NA"), NA,
                        1))) %>% # make anyone who is not non-Hispanic White alone and who answered the question, BIPOC
  select(response_id, bipoc)

# qa check
table(race$nh_race,useNA='always')
table(binary_bipoc$bipoc,useNA='always')

# Step 3: Test identifying disconnected youth ----
##### Explore data and test strategy for recoding ------
# select variables for employment and education status
educ_employ_vars<-svy_dd %>%
  filter(variable_name %in% c('Employment And Education','Education')) %>%
  select(variable,response_1,variable_name)

# subset variables and create a new dataframe
svy_data_connect <- svy_data_raw  %>%
  select(response_id,all_of(educ_employ_vars$variable)) %>% # select variables
  mutate(across(where(is.character), ~ ifelse(!is.na(.), 1, NA))) # Convert non-numeric values to 0/1

# recode any 1 values to name of column
col_labels <- which(svy_data_connect==1,arr.ind=TRUE)
svy_data_connect[col_labels ] <- names(svy_data_connect)[col_labels [,"col"]]

## qa check
table(svy_data_connect$bt,useNA='always')
table(svy_data_raw$bt,useNA='always')

# create a column with all variables selected by respondent
svy_data_connect <- svy_data_connect %>%
  unite(response_list, bt:cf, sep = ",", remove = FALSE, na.rm = TRUE)

# check responses 
table(svy_data_connect$response_list,useNA='always')

# recode blank to NA for list later
svy_data_connect$response_list[svy_data_connect$response_list == ""] <- "nonresponse"

# look at cases where empty/no response or don't wish to answer or only looking for work
educ_employ_vars # look at data dictionary

disconnected <- list("by","cf","bx","bz","bx,bz","nonresponse") # values to search for

# try filtering for the list
check <- svy_data_connect %>% 
  mutate(response_list_2 = as.list(response_list)) %>% # convert to actual list column
  # check: returns TRUE if any of the elements in list are not in our target list
  mutate(connected_check = map_lgl(response_list_2, ~any(!(. %in% disconnected)))) %>%
  # filter for only youth where all responses are in target list
  filter(connected_check == FALSE)%>%
  left_join(svy_data_raw %>%
              select(response_id,bz),
            by="response_id")
  
table(check$response_list,useNA='always') 

# check other responses
check_other <- check %>%
  filter(str_detect(response_list_2, "bz"))
# some folks who only selected BZ should be recoded to disconnected

# flag other that are disconnected
other <- list("Currently not looking for work but sometime this year", "have employment and not looking for it", # not looking, 2nd is a response where characters for don't are jumbled
              "No","No work","None","Not working","Unemployed","Currently unemployed", "I do not have a job", # not working
                     "Sex work","Become my own boss","I am a stay at home mom","Stay at home mom", # not in labor force or in underground work
              "Y estoy buscando escuela para empezar mis estudios .") # looking for school and work
  
# check youth who did not respond
check_nonresponse <- svy_data_raw %>%
  left_join(svy_data_connect, by="response_id")%>%
  filter(response_list=="nonresponse")
# seem to have responded to other items, but assume NA vs. disconnected

##### Recode youth only looking for work (bx) as disconnected or youth who reported no work ------
binary_connected <- svy_data_connect %>% 
   # recode only youth that reported looking for work and nothing else
  mutate(disconnected_step1=
             case_when(
               response_list=="bx" ~ 1, #only looking for work
               response_list=="by" ~NA, # dont wish to answer as NA, no youth just reported cf and no other response
               response_list=="nonresponse" ~NA, # skipped question as NA
               TRUE ~ 0)) %>%
  left_join(svy_data_raw %>% # join write-ins to flag the others
              select(response_id,bz) %>%
              rename(bz_writein=bz),
            by="response_id") %>%
  # recode write-ins that indicated no work, underground work, or looking for work
  mutate(disconnected=
           case_when(
             str_detect(bz_writein, paste(other, collapse = "|")) ~ 1, 
             TRUE ~ disconnected_step1))

# qa check
table(binary_connected$disconnected,useNA='always')
table(binary_connected$disconnected_step1,useNA='always') # 15 added


# Step 4: Push final table to postgres ----
binary_final <- binary_bipoc %>% 
  left_join(binary_connected %>% select(response_id,disconnected)) %>%
  left_join(binary_systems_imp)

# Write table with metadata
source("W:\\RDA Team\\R\\Github\\RDA Functions\\main\\RDA-Functions\\Utility_Functions.R")
table_name <- "demographics_binary_data"
schema <- "youth_thriving"
indicator <- " A table with binary variables for select demographics BIPOC, disconnected youth, systems-involved, undocumented, unhoused, ever suspended/expelled, ever arrested 
A 1 means that variable is true "
source <- "
Script:W:/Project/OSI/Bold Vision/Youth Thriving Survey/GitHub/EMG/boldvision_youththriving/Data Prep and Quality/Recoding/demo_binary_vars.R"
qa_filepath <- " See QA doc for details: W:/Project/OSI/Bold Vision/Youth Thriving Survey/Documentation/QA_demographics_binary_data.docx "
table_comment <- paste0(indicator, source)
dbWriteTable(con, c(schema, table_name), binary_final,
             overwrite = FALSE, row.names = FALSE)

# Comment on table and columns
column_names <- colnames(binary_final) # Get column names
column_comments <- c(
  "response id",
  "whether the respondent reported a non-white race ethnicity",
  "whether the respondent reported no school enrollment and only looking for work and not employed, does not include youth who did not respond to education or employment questions",
  "whether the respondent reported ever being systems involved based on q24",
  "whether the respondent reported ever being detained or arrest based on q25",
  "whether the respondent reported ever being suspended, subjected to opportunity transfer, expelled, or had parents called for school incident based on q26",
  "whether the respondent reported being on temporary immigration or refugee status based on q27",
  "whether the respondent reported being homeless or had temporary housing in past year based on q28"
)

add_table_comments(con, schema, table_name, indicator, source, qa_filepath, column_names, column_comments)

dbDisconnect(con)