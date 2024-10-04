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
#get spa shapefile and reduce fields
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

tract_spa_xwalk_check<-st_transform(tract_spa_xwalk,4326)
# create a color palette for the map
pal <- colorFactor(
  palette = 'Dark2',
  domain = tract_spa_xwalk$SPA
)

# map the data
leaflet(bg_access) %>%
  addTiles() %>%
  addPolygons(
    stroke = FALSE, fillOpacity = 0.5,
    smoothFactor = 0.5,  fillColor = ~ mypalettez(indicator_final),
  )%>%
  addLegend(
    pal = mypalettez, values = ~indicator_final, opacity = 0.9,
    position = "bottomleft"
  )

#convert to df and reduce columns
tract_spa_xwalk <- as.data.frame(tract_spa_xwalk) %>% select(GEOID, SPA, SPA_NAME) %>% 
  rename(geoid = GEOID, spa = SPA, spa_name = SPA_NAME)

#add back one tract that fails in spatial join
tract_spa_xwalk[nrow(tract_spa_xwalk) + 1, ] = c("06037599100", "8", "South Bay")

# add missing tract

#disconnect
dbDisconnect(con)