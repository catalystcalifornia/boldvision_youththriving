# Data Dictionary to PGAMIN Database 

####Set Up ####
packages <- c("dplyr", "RPostgreSQL", "usethis", "readxl", "janitor", "stringr") 

for(pkg in packages){
  library(pkg, character.only = TRUE)
}

options(scipen = 100) # disable scientific notation

# create connection for bold_vision
source("W:\\RDA Team\\R\\credentials_source.R")
con_bv <- connect_to_db("bold_vision") 

#### screened_svy_data_dictionary ####
#Step 1: pull master data dictionary 
master_dd <- dbGetQuery(con_bv, "SELECT * FROM youth_thriving.bvys_datadictionary_2024")

#Step 2: clean up data dictionary to only keep relevant columns 
clean_dd <- master_dd %>%
  filter(response_domain != "Fun") %>% #dropping survey break fun questions 
  select(-c(variable_category, response_domain, primary_key)) %>% #dropping columns not relevant 
  arrange(question_number) %>% #dropped primary_key since we've deleted rows so we should create a new one 
  mutate(primary_id = row_number()) %>%#new primary_id 
  select(primary_id, everything())

#Step 3: push data dictionary to pgAdmin with table comment 
dbWriteTable(con_bv, Id(schema ='bvyts_data_sharing', table = 'screened_svy_data_dictionary'), clean_dd,
                            overwrite = FALSE, row.names = FALSE)

dbSendQuery(con_bv, "COMMENT ON TABLE bvyts_data_sharing.screened_svy_data_dictionary IS 
'The following data dictionary created on 12/11/2025 aims to decode the data for the Bold Vision Youth Thriving Survey Data screened for sharing with external partners.
Dictionary produced in //boldvision_youththriving//Data Prep and Quality//data dictionaries//data_dictionaries_12_2025.R'")

#### screened_sogi_data_dictionary ####
#Step 1: pull column comments for variable definitions
query <-  "
SELECT
    cols.column_name,
    cols.data_type,
    pgd.description AS column_comment
FROM information_schema.columns cols
LEFT JOIN pg_catalog.pg_class pc
    ON pc.relname = cols.table_name
LEFT JOIN pg_catalog.pg_namespace pn
    ON pn.oid = pc.relnamespace
LEFT JOIN pg_catalog.pg_description pgd
    ON pgd.objoid = pc.oid
    AND pgd.objsubid = cols.ordinal_position
WHERE cols.table_schema = 'youth_thriving'
  AND cols.table_name = 'gender_sexuality_data'
ORDER BY cols.ordinal_position;
"

sogi_dd <- dbGetQuery(con_bv, query)

#pull just the column names from screened data table 
sogi_cols <- dbListFields(con_bv, DBI::Id(schema = "bvyts_data_sharing",
                                         table = "screened_sogi_data"))

#Step 2: clean up data dictionary to only keep columns that are in the screened dataset 
sogi_clean_dd <- sogi_dd %>%
  filter(column_name %in% sogi_cols)

#Step 3: push data dictionary to pgAdmin with table comment 
dbWriteTable(con_bv, Id(schema ='bvyts_data_sharing', table = 'screened_sogi_data_dictionary'), sogi_clean_dd,
             overwrite = FALSE, row.names = FALSE)

dbSendQuery(con_bv, "COMMENT ON TABLE bvyts_data_sharing.screened_sogi_data_dictionary IS 
'The following data dictionary created on 12/11/2025 aims to decode the data for the Bold Vision Youth Thriving Survey Sexual Orientation and Gender Identity data.
Dictionary produced in //boldvision_youththriving//Data Prep and Quality//data dictionaries//data_dictionaries_12_2025.R'")

#### screened_race_data_dictionary ####
#Step 1: pull column comments for variable definitions
race_query <-  "
SELECT
    cols.column_name,
    cols.data_type,
    pgd.description AS column_comment
FROM information_schema.columns cols
LEFT JOIN pg_catalog.pg_class pc
    ON pc.relname = cols.table_name
LEFT JOIN pg_catalog.pg_namespace pn
    ON pn.oid = pc.relnamespace
LEFT JOIN pg_catalog.pg_description pgd
    ON pgd.objoid = pc.oid
    AND pgd.objsubid = cols.ordinal_position
WHERE cols.table_schema = 'youth_thriving'
  AND cols.table_name = 'race_ethnicity_data'
ORDER BY cols.ordinal_position;
"

race_dd <- dbGetQuery(con_bv, race_query)

#pull just the column names from screened data table 
race_cols <- dbListFields(con_bv, DBI::Id(schema = "bvyts_data_sharing",
                                          table = "screened_race_data"))

#Step 2: clean up data dictionary to only keep columns that are in the screened dataset 
race_clean_dd <- race_dd %>%
  filter(column_name %in% race_cols)

#Step 3: push data dictionary to pgAdmin with table comment 
dbWriteTable(con_bv, Id(schema ='bvyts_data_sharing', table = 'screened_race_data_dictionary'), race_clean_dd,
             overwrite = FALSE, row.names = FALSE)

dbSendQuery(con_bv, "COMMENT ON TABLE bvyts_data_sharing.screened_race_data_dictionary IS 
'The following data dictionary created on 12/11/2025 aims to decode the data for the Bold Vision Youth Thriving Survey racial data.
Dictionary produced in //boldvision_youththriving//Data Prep and Quality//data dictionaries//data_dictionaries_12_2025.R'")

#### screened_systems_unhoused_imgrtion_data_dictionary ####
#Step 1: pull column comments for variable definitions
demo_query <-  "
SELECT
    cols.column_name,
    cols.data_type,
    pgd.description AS column_comment
FROM information_schema.columns cols
LEFT JOIN pg_catalog.pg_class pc
    ON pc.relname = cols.table_name
LEFT JOIN pg_catalog.pg_namespace pn
    ON pn.oid = pc.relnamespace
LEFT JOIN pg_catalog.pg_description pgd
    ON pgd.objoid = pc.oid
    AND pgd.objsubid = cols.ordinal_position
WHERE cols.table_schema = 'youth_thriving'
  AND cols.table_name = 'demographics_binary_data'
ORDER BY cols.ordinal_position;
"

demo_dd <- dbGetQuery(con_bv, demo_query)

#pull just the column names from screened data table 
demo_cols <- dbListFields(con_bv, DBI::Id(schema = "bvyts_data_sharing",
                                          table = "screened_systems_unhoused_imgrtion_data"))

#Step 2: clean up data dictionary to only keep columns that are in the screened dataset 
demo_clean_dd <- demo_dd %>%
  filter(column_name %in% demo_cols)

#Step 3: push data dictionary to pgAdmin with table comment 
dbWriteTable(con_bv, Id(schema ='bvyts_data_sharing', table = 'screened_systems_unhoused_imgrtion_data_dictionary'), demo_clean_dd,
             overwrite = FALSE, row.names = FALSE)

dbSendQuery(con_bv, "COMMENT ON TABLE bvyts_data_sharing.screened_race_data_dictionary IS 
'The following data dictionary created on 12/11/2025 aims to decode the data for the Bold Vision Youth Thriving Survey binary demographic data such as immigration status, unhoused, etc.
Dictionary produced in //boldvision_youththriving//Data Prep and Quality//data dictionaries//data_dictionaries_12_2025.R'")


#### close connection ####
dbDisconnect(con_bv)


