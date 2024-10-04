### Calculate counts and percentages of youth ages 15-24 by race in LA County ###
# Using ACS PUMS 2018-2022 data

# Set up environment -----------------------------------------------------------

packages <- c("data.table", "stringr", "dplyr", "RPostgreSQL", "dbplyr", "srvyr", "tidyverse","tidycensus", "tidyr", "here", "sf", "usethis") 

for(pkg in packages){
  library(pkg, character.only = TRUE)
}

options(scipen = 100) # disable scientific notation

# create connection for rda database
source("W:\\RDA Team\\R\\credentials_source.R")
con <- connect_to_db("rda_shared_data")


# Get PUMA-County Crosswalks -----------------------------------------------------------
# and rename fields to distinguish vintages
crosswalk10 <- st_read(con, query = "select county_id AS geoid, county_name AS geoname, puma, num_county from crosswalks.puma_county_2021")
crosswalk10 <- crosswalk10 %>% rename(puma10 = puma, geoid10 = geoid, geoname10 = geoname) 

crosswalk20 <- st_read(con, query = "select county_id AS geoid, county_name AS geoname, puma, num_county from crosswalks.puma_county_2022")
crosswalk20 <- crosswalk20 %>% rename(puma20 = puma, geoid20 = geoid, geoname20 = geoname) 

# Get PUMS Data -----------------------------------------------------------
# Data Dictionary: https://www2.census.gov/programs-surveys/acs/tech_docs/pums/data_dict/PUMS_Data_Dictionary_2022.pdf
# path where my data lives
root <- "W:/Data/Demographics/PUMS/CA_2018_2022/"

# Load PUMS data
ppl <- fread(paste0(root, "psam_p06.csv"), header = TRUE, data.table = FALSE, 
             colClasses = list(character = c("PUMA10", "PUMA20","ANC1P", "ANC2P", "HISP", "RAC1P", "RAC2P", "RAC3P", "RACAIAN", "RACPI", "RACNH")))

# Filter for LA County ------------------------------------------------------------------
# Add state_geoid to ppl, add state_geoid to PUMA id, so it aligns with crosswalks.puma_county_2020
ppl$state_geoid <- "06"
ppl$puma_id10 <- paste0(ppl$state_geoid, ppl$PUMA10)
ppl$puma_id20 <- paste0(ppl$state_geoid, ppl$PUMA20)

# save copy of original data
orig_data <- ppl

# join county crosswalk using left join function
ppl <- left_join(orig_data, crosswalk10, by=c("puma_id10" = "puma10"))    # specify the field join
ppl <- left_join(ppl, crosswalk20, by=c("puma_id20" = "puma20"))    # specify the field join

# create one field using both crosswalks
ppl <- ppl %>% mutate(geoid = ifelse(is.na(ppl$geoid10) & is.na(ppl$geoid20), NA, 
                                     ifelse(is.na(ppl$geoid10), ppl$geoid20, ppl$geoid10)))


ppl <- ppl %>% mutate(geoname = ifelse(is.na(ppl$geoname10) & is.na(ppl$geoname20), NA, 
                                       ifelse(is.na(ppl$geoname10), ppl$geoname20, ppl$geoname10)))

# review data 
View(ppl[c("geoid","geoid10","geoid20","geoname","geoname10","geoname20","puma_id10","puma_id20","PUMA10","PUMA20")])

# try just filtering based on 037 in puma
ppl_lac<-ppl%>%filter(str_detect(PUMA10, "^037") | str_detect(PUMA20, "^037"))

# check that it worked
qa<-ppl_lac%>%group_by(PUMA10,PUMA20)%>%summarise(count=n())
View(ppl_lac[c("geoid","geoid10","geoid20","geoname","geoname10","geoname20","puma_id10","puma_id20","PUMA10","PUMA20")])
# looks good, proceed with subset

# Filter for Ages ------------------------------------------------------------------
# survey was for ages 15-24
ppl_lac<-ppl_lac%>%filter(AGEP>=15 & AGEP<=24)

# check that it worked
table(ppl_lac$AGEP)

# Reclassify Race/Ethnicity -----------------------------------------
# set up
source("W:/RDA Team/R/Github/RDA Functions/main/RDA-Functions/PUMS_Functions_new.R") # race reclass functions though not using here
source("W:/RDA Team/R/Github/RDA Functions/main/RDA-Functions/SWANA_Ancestry_List.R") # SWANA list for comparison

## Data Dictionary: https://www2.census.gov/programs-surveys/acs/tech_docs/pums/data_dict/PUMS_Data_Dictionary_2022.pdf 
## pull in data dictionary for PUMS 2018-22 from the web
dictionary <- read.csv("https://www2.census.gov/programs-surveys/acs/tech_docs/pums/data_dict/PUMS_Data_Dictionary_2018-2022.csv",header=FALSE,colClasses="character")

### Set up codes for SWANA ---------------------------------------------------------------------------------------
## Create list of swana codes for PUMS
pums_swana_list<-list("Armenian","Algerian","Arab","Assyrian","Bahraini","Berber","Chaldean","Egyptian","Emirati","Iranian","Iraqi","Israeli","Jordanian","Kurdish","Kuwaiti","Lebanese","Libyan","Middle Eastern","Moroccan","North African","Omani","Palestinian","Qatari","Saudi","Syriac","Syrian","Tunisian","Yazidi","Yemeni","Mideast","Saudi Arabian","Arabic","Other Arab","Libyan","Kuwaiti","Turkish","Sudanese","Afghan")

## check for differences from repository list
setdiff(swana_ancestry, pums_swana_list) 
## Mid East in repo but not in swana list, it isn't in the data for PUMS, but we can add it just in case

setdiff(pums_swana_list,swana_ancestry)
## everything in our pums swana list is in the repoistory

## add Mid East
pums_swana_list<-list("Armenian","Algerian","Arab","Assyrian","Bahraini","Berber","Chaldean","Egyptian","Emirati","Iranian","Iraqi","Israeli","Jordanian","Kurdish","Kuwaiti","Lebanese","Libyan","Mid East","Middle Eastern","Moroccan","North African","Omani","Palestinian","Qatari","Saudi","Syriac","Syrian","Tunisian","Yazidi","Yemeni","Mideast","Saudi Arabian","Arabic","Other Arab","Libyan","Kuwaiti","Turkish","Sudanese","Afghan")

# get the corresponding codes from the data dictionary filtering just for the ancestry variables
swana_codes<-dictionary%>%filter(V7 %in% pums_swana_list & V2 %in% c("ANC1P", "ANC2P"))

# clean up and create a list
swana_codes_list<-unique(swana_codes$V5)

### Recode race -------------------------------------------------------------------------------------
# Recode race/eth
## latinx - alone or in combination with another race
ppl_lac$latino <- "latino"
ppl_lac$latino[ppl_lac$HISP=="01"] <- "not latino"
ppl_lac$latino <- as.factor(ppl_lac$latino)

## aian - alone or in combination with another race or latino
ppl_lac$aian <- "not aian"
ppl_lac$aian[ppl_lac$RACAIAN==1] <- "aian"
ppl_lac$aian <- as.factor(ppl_lac$aian)

## nhpi - alone or in combination with another race or latino
ppl_lac$pacisl <- "not pacisl"
ppl_lac$pacisl[ppl_lac$RACPI==1 | ppl_lac$RACNH==1] <- "pacisl"
ppl_lac$pacisl <- as.factor(ppl_lac$pacisl)

## swana - alone or in combination with another race or latino
ppl_lac$swana <- "not swana"
ppl_lac$swana[ppl_lac$ANC1P%in% swana_codes_list| ppl_lac$ANC2P %in% swana_codes_list] <- "swana" # both anc fields use same codes
ppl_lac$swana <- as.factor(ppl_lac$swana)

## nh_race groups
ppl_lac$race = as.factor(ifelse(ppl_lac$RAC1P == 1 & ppl_lac$latino =="not latino", "nh_white",
                                ifelse(ppl_lac$RAC1P == 1 & ppl_lac$latino =="latino", "white",
                                       ifelse(ppl_lac$RAC1P == 2 & ppl_lac$latino =="not latino", "nh_black",
                                              ifelse(ppl_lac$RAC1P == 2 & ppl_lac$latino =="latino", "black",
                                                     ifelse(ppl_lac$RAC1P == 6 & ppl_lac$latino =="not latino", "nh_asian",
                                                            ifelse(ppl_lac$RAC1P == 6 & ppl_lac$latino =="latino", "asian",
                                                                   ifelse(ppl_lac$RAC1P == 8 & ppl_lac$latino =="not latino", "nh_other", 
                                                                          ifelse(ppl_lac$RAC1P == 8 & ppl_lac$latino =="latino", "other",
                                                                                 ifelse(ppl_lac$RAC1P == 9 & ppl_lac$latino =="not latino", "nh_twoormor",
                                                                                        ifelse(ppl_lac$RAC1P == 9 & ppl_lac$latino =="latino", "twoormor",
                                                                                               ifelse(ppl_lac$RAC1P %in% c(3,4,5) & ppl_lac$latino =="latino", "aian",
                                                                                                      ifelse(ppl_lac$RAC1P %in% c(3,4,5) & ppl_lac$latino =="not latino", "nh_aian",
                                                                                                             ifelse(ppl_lac$RAC1P==7 & ppl_lac$latino =="latino", "pacisl",
                                                                                                                    ifelse(ppl_lac$RAC1P==7 & ppl_lac$latino =="not latino", "nh_pacisl",
                                                                                                                           
                                                                                                                           
                                                                                                                           NA)))))))))))))))


## Check that columns are added and populated correctly
## latino includes all races. AIAN is AIAN alone/combo latino/non-latino, NHPI is alone/combo latino/non-latino
View(ppl_lac[c("HISP","latino","RAC1P","race","RAC2P","RAC3P","ANC1P","ANC2P","swana","AGEP")])
table(ppl_lac$race,useNA="always")
ppl_lac%>%group_by(race,latino)%>%summarise(count=n())
## all looks good

# Run population estimates  ----------------------------------------
## prep data and survey design
repwlist = rep(paste0("PWGTP", 1:80)) # create list of replicate weights

weight <- 'PWGTP' 

ppl_srvy<- ppl_lac %>%               
  as_survey_rep(
    variables = c(geoid, race, latino, aian, pacisl, swana),   # dplyr::select grouping variables
    weights = all_of(weight),                       # person weight
    repweights = all_of(repwlist),                  # list of replicate weights
    combined_weights = TRUE,                # tells the function that replicate weights are included in the data
    mse = TRUE,                             # tells the function to calc mse
    type="other",                           # statistical method
    scale=4/80,                             # scaling set by ACS
    rscale=rep(1,80)                        # setting specific to ACS-scaling
  )

## create function to run estimates
estimate_calc<-function(df,geoid,var){
  var_df <- df %>%rename(race_cat=var)%>%
    group_by(geoid, race_cat) %>%   # group by race cat
    summarise(
      count = survey_total(na.rm=T), # get the (survey weighted) count for the numerators
      rate = survey_mean()) %>%        # get the (survey weighted) proportion for the numerator
    left_join(df %>%                                        # left join in the denominators
                group_by(geoid) %>%                                     # group by geo
                summarise(pop = survey_total(na.rm=T))) %>%              # get the weighted total for overall geo
    mutate(rate=rate*100,
           rate_moe = rate_se*1.645*100,    # calculate the margin of error for the rate based on se
           rate_cv = ((rate_moe/1.645)/rate) * 100, # calculate cv for rate
           count_moe = count_se*1.645, # calculate moe for numerator count based on se
           count_cv = ((count_moe/1.645)/count) * 100)  # calculate cv for numerator count
  return(var_df)
}

## run for each group
latino<-estimate_calc(df=ppl_srvy,geoid=geoid,var="latino")
aian<-estimate_calc(df=ppl_srvy,geoid=geoid,var="aian")
pacisl<-estimate_calc(df=ppl_srvy,geoid=geoid,var="pacisl")
swana<-estimate_calc(df=ppl_srvy,geoid=geoid,var="swana")
race<-estimate_calc(df=ppl_srvy,geoid=geoid,var="race")

## combine dataframes
df<-rbind(latino%>%filter(race_cat=="latino"),
          aian%>%filter(race_cat=="aian"),
          pacisl%>%filter(race_cat=="pacisl"),
          swana%>%filter(race_cat=="swana"),
          race%>%filter(str_detect(race_cat, "^nh")))

## clean up columns
df<-df%>%
  select(geoid,race_cat,count,count_moe,count_cv,rate,rate_moe,rate_cv,pop)%>%
  rename(race=race_cat)

# Push to postgres -----
bv_con <- connect_to_db("bold_vision") # connect to bv database

# function for adding table and column comments from RC
add_table_comments <- function(con, schema, table_name, indicator, source, column_names, column_comments) {
  comments <- character()
  comments <- c(comments, paste0("
    COMMENT ON TABLE ", schema, ".", table_name, " IS '", table_comment, "';"))
  for (i in seq_along(column_names)) {
    comment <- paste0("
      COMMENT ON COLUMN ", schema, ".", table_name, ".", column_names[i], " IS '", column_comments[i], "';
      ")
    comments <- c(comments, comment)
  }
  sql_commands <- paste(comments, collapse = "")
  dbSendQuery(con, sql_commands)
}

# specify table and schema
table_name <- "acs_pums_race_pop_15_24"
schema <- 'youth_thriving'

# write table
# dbWriteTable(bv_con, c(schema, table_name),df,
#              overwrite = FALSE, row.names = FALSE)

# comment on table and columns
indicator <- "Long table format of youth population ages 15-24 by race for comparison for youth thriving survey and for survey weighting
All groups are non-Hispanic other than Latino, AIAN, PACISL, and SWANA
Latino, aian, pacisl, swana are all alone or in combo with Latino or another race.
SWANA estimated based on ancestry fields "
source <- "ACS PUMS 2018-2022 estimates. See QA doc for details: W:/Project/OSI/Bold Vision/Youth Thriving Survey/Documentation/QA_acs_pums_race_15_24.docx
Script: W:/Project/OSI/Bold Vision/Youth Thriving Survey/R/acs_pums_race_15_24.R"
table_comment <- paste0(indicator, source)

column_names <- colnames(df) # get column names

# create comments
column_comments <- c(
  "LA County GEOID",
  "race category estimate is for",
  "estimated count of youth 15-24 in that category in LA County",
  "estimated MOE for count of youth in that category at 90% CI",
  "estimated CV for count of youth in that category at 90% CI",
  "estimated rate (%) of youth 15-24 in that category out of all youth in LA county",
  "estimated MOE for rate of youth in that category at 90% CI",
  "estimated CV for rate of youth in that category at 90% CI",
  "estimated population of youth 15-24 in LA County"
)

# push table and column comments to database
# add_table_comments(bv_con, schema, table_name, indicator, source, column_names, column_comments)

dbDisconnect(con) 
dbDisconnect(bv_con)
