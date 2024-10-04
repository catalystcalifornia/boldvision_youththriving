# Create population counts for age, sex at birth, and SPAs using ACS 2022 data

# Set up environment ----
library(tidycensus)
library(tidyverse)
library(RPostgreSQL)
library(tigris)
library(mapview)
library(sf)
library(leaflet)

# connect to postgres and RDA credentials
source("W:\\RDA Team\\R\\credentials_source.R")
con <- connect_to_db("bold_vision")
rda_con<-connect_to_db("rda_shared_data")

# Pull in tables stored in postgres
## age and sex table
age_sex <- dbGetQuery(rda_con, "SELECT * FROM demographics.acs_5yr_s0101_multigeo_2022 WHERE name='Los Angeles County, California' and geolevel = 'county'")
age_sex_tract <- dbGetQuery(rda_con, "SELECT * FROM demographics.acs_5yr_s0101_multigeo_2022 WHERE geolevel = 'tract'")
age_sex_tract<-age_sex_tract%>%filter(grepl('06037',geoid))

# load variables
vars_subject <- load_variables(2022, "acs5/subject", cache = TRUE)

# Age counts ----
# Groups needed
# 15-17
# 18-24

# get list of variables needed
vars_age<-vars_subject%>%
  filter(label %in% c("Estimate!!Total!!Total population!!SELECTED AGE CATEGORIES!!15 to 17 years","Estimate!!Total!!Total population!!SELECTED AGE CATEGORIES!!18 to 24 years"))%>%
  select(name,label)%>%
  mutate(name=tolower(paste0(name,"e"))) # add e to variable name so it matches data in postgres


# pull data from dataframe
age_counts<-subset(age_sex, select=names(age_sex) %in% vars_age$name)%>% # subset variables to those we need matching df above
  mutate(geoid="06037",name="Los Angeles County",weighting_group="Age")%>% # add variables for description in database
  pivot_longer(!c(geoid,name,weighting_group),names_to="variable",values_to="count")%>% # pivot dataframe
  mutate(variable=case_when(variable=='s0101_c01_021e' ~"15-17", # recode categories
                            variable=='s0101_c01_023e' ~ "18-24"))%>%
  group_by(geoid,name,weighting_group)%>%mutate(percent=count/sum(count))%>% # add percentage
  ungroup()


# Sex at birth counts ----
# Groups needed -- ACS doesn't collect gender identity, only sex at birth male/female
# Male
# Female

# create list of categories needed to make groups
age_sex_list<-list('Estimate!!Male!!Total population!!SELECTED AGE CATEGORIES!!15 to 17 years',
     'Estimate!!Male!!Total population!!SELECTED AGE CATEGORIES!!18 to 24 years',
     'Estimate!!Female!!Total population!!SELECTED AGE CATEGORIES!!15 to 17 years',
     'Estimate!!Female!!Total population!!SELECTED AGE CATEGORIES!!18 to 24 years')

# get list of variables needed
vars_age_sex<-vars_subject%>%
  filter(label %in% age_sex_list)%>%
  select(name,label)%>%
  mutate(name=tolower(paste0(name,"e"))) # add e to variable name so it matches data in postgres

# pull data from dataframe
sex_counts<-subset(age_sex, select=names(age_sex) %in% vars_age_sex$name)%>% # subset variables to those we need matching df above
  mutate(geoid="06037",name="Los Angeles County",weighting_group="Sex at birth")%>% # add variables for description in database
  pivot_longer(!c(geoid,name,weighting_group),names_to="variable",values_to="count")%>% # pivot dataframe
  mutate(variable=case_when(variable=="s0101_c03_021e"  ~"Male", # recode and collapse to sex
                            variable=="s0101_c03_023e" ~"Male",
                            variable=="s0101_c05_021e" ~"Female",
                            variable=="s0101_c05_023e" ~ "Female"))%>%
  group_by(geoid,name,weighting_group,variable)%>%
  summarise(count=sum(count),.groups='drop')%>%
  mutate(percent=count/sum(count))

# SPA counts ----
## Get and prep shapes -----
# get spa shapefile and reduce fields
spas <- st_read(rda_con, query = "select * from geographies_la.la_county_service_planning_areas_2022")

# check projection and transform
st_crs(spas)
spas<-st_transform(spas,3310)
spas<-spas%>%select(OBJECTID,SPA,SPA_NAME,LABEL)

# get tract boundaries
ca_tracts <- tracts(state = "CA", cb = TRUE, year = 2022)
# mapview(ca_tracts) # worked

# check projection and transform
st_crs(ca_tracts)
ca_tracts<-st_transform(ca_tracts,3310)

# filter to lac tracts
lac_tracts<-ca_tracts%>%filter(COUNTYFP=='037')%>%select(GEOID)

# check for match to age sex df
nrow(lac_tracts) #2495
nrow(age_sex_tract) #2498 so 3 missing from shapes
missing<-age_sex_tract%>%filter(!geoid %in% lac_tracts$GEOID)
# all 3 missing tracts of 0 populations

## Create crosswalk of 2022 acs tracts to spas ----
# spatial join SPAs to tracts based on centroid
tract_spa_xwalk <- st_join(spas, st_centroid(lac_tracts), join = st_contains)

# check result
nrow(tract_spa_xwalk) # 2494, 1 missing
missing<-lac_tracts%>%filter(!GEOID %in% tract_spa_xwalk$GEOID)
# mapview(missing) Just the islands didn't merge to SPA 8, merge back later

# convert to df and reduce columns
tract_spa_xwalk <- as.data.frame(tract_spa_xwalk%>%st_drop_geometry()) %>% select(GEOID, SPA, SPA_NAME,LABEL) %>% 
  rename(geoid = GEOID, spa = SPA, spa_name = SPA_NAME,label=LABEL)

# # check results through a map
# xwalk_check<-lac_tracts%>%left_join(tract_spa_xwalk,by=c("GEOID"='geoid'))
# xwalk_check<-st_transform(xwalk_check,4326)
# spa_check<-st_transform(spas,4326)
# # create a color palette for the map
# pal <- colorFactor(
#   palette = 'Dark2',
#   domain = xwalk_check$spa_name
# )
# # map the data
# leaflet(xwalk_check) %>%
#   addTiles() %>%
#   addPolygons(
#     stroke = FALSE, fillOpacity = 0.5,
#     smoothFactor = 0.5,  fillColor = ~ pal(spa_name),
#   )%>%
#   addPolygons(data = spa_check,
#               weight = 1)%>%
#   addLegend(
#     pal = pal, values = ~spa_name, opacity = 0.9,
#     position = "bottomleft"
#   )
  
# add back one tract that fails in spatial join
tract_spa_xwalk[nrow(tract_spa_xwalk) + 1, ] = c("06037599100", "8", "South Bay","SPA 8")

## Calculate 15-24 population by SPA ----
# pull data from dataframe
spa_counts_step1<-age_sex_tract%>%select(geoid, all_of(vars_age$name)) %>% # subset variables to those we need matching df above
  left_join(tract_spa_xwalk) # join SPA matches

spa_counts<-spa_counts_step1%>% 
  group_by(spa,spa_name,label)%>% # summarize 15-24 population by SPA
  summarise(count=sum(s0101_c01_021e+s0101_c01_023e),.groups='drop')%>%
  rename(geoid=spa, name=spa_name, variable=label)%>%
  mutate(weighting_group='SPA')%>%
  mutate(percent=count/sum(count))%>%
  filter(!is.na(variable))%>%
  select(geoid,name,weighting_group,everything())

# Bind together population counts ----
df<-rbind(age_counts,sex_counts,spa_counts)

# check result
df%>%group_by(weighting_group)%>%summarise(pop=sum(count),total=sum(percent))
# looks good

# Push to postgres ----
#### Relational searches stops ----
table_name <- "acs_weighting_population_counts"
schema <- 'youth_thriving'

indicator <- "Age, sex, and SPA counts for use in survey weighting based on 5-year ACS data for LA County 2018-2022 subject table S0101"
source <- "See QA doc for details: W:\\Project\\OSI\\Bold Vision\\Youth Thriving Survey\\Documentation\\QA_acs_weights_population_counts.docx
Script: W:/Project/OSI/Bold Vision/Youth Thriving Survey/GitHub/EMG/boldvision_youththriving/Survey Weighting/population_counts.R"
table_comment <- paste0(indicator, source)

# write table
# dbWriteTable(con, c(schema, table_name),df,
#              overwrite = FALSE, row.names = FALSE)

# comment on table and columns
column_names <- colnames(df) # get column names

column_comments <- c(
  "GEOID or object ID--mostly LA County with IDs for SPAs for SPA level weighting",
  "name of county or SPA",
  "Weighting level-age, sex, or SPA",
  "Category or variable for that weighting level",
  "Total estimated count of target youth population in category or variable",
  "Percent of target youth population in category or variable out of youth population for that weighting group"
)

add_table_comments(con, schema, table_name, indicator, source, column_names, column_comments)

#disconnect
dbDisconnect(con)

# Push 2022 tracts to postgres because they aren't there
cb_2022_06_tract_500k<-ca_tracts

# # not working
# st_write(cb_2022_06_tract_500k, dsn = rda_con, schema="geographies_ca",layer = "cb_2022_06_tract_500k",
#          delete_layer = FALSE, append = FALSE)

indicator <- "2022 Tract Boundaries for California from TIGRIS package"
source <- "See QA doc for details: W:\\Project\\OSI\\Bold Vision\\Youth Thriving Survey\\Documentation\\QA_acs_weights_population_counts.docx
Script: W:/Project/OSI/Bold Vision/Youth Thriving Survey/GitHub/EMG/boldvision_youththriving/Survey Weighting/population_counts.R"
table_comment <- paste0(indicator, source)


# add table comment
# dbSendQuery(rda_con, paste0("comment on table geographies_ca.cb_2022_06_tract_500k is
#                         '2022 Tract Boundaries for California from TIGRIS package
#                           See QA doc for details: W:\\Project\\OSI\\Bold Vision\\Youth Thriving Survey\\Documentation\\QA_acs_weights_population_counts.docx
#                           Script: W:/Project/OSI/Bold Vision/Youth Thriving Survey/GitHub/EMG/boldvision_youththriving/Survey Weighting/population_counts.R';"))

dbDisconnect(rda_con)