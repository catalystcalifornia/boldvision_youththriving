## Average the component/factor scores for all youth and by youth demographics based on imputed factor scores

# Step 0: Setting up work space ------
library(dplyr)
library(RPostgreSQL)
library(tidyr)
library(tidyverse)
library(srvyr)
library(survey)
library(purrr)
options(scipen=999)

# connect to postgres and source functions
source("W:\\RDA Team\\R\\credentials_source.R")

con <- connect_to_db("bold_vision")

# pulling raw survey data
svy_data_raw <- dbGetQuery(con, "SELECT response_id, age_minor_adult, spa_final_respondent, spa_name_final_respondent, weights_final 
                       FROM youth_thriving.raw_survey_data")

# pulling data dictionary 
svy_dd <- dbGetQuery(con, "SELECT * FROM youth_thriving.bvys_datadictionary_2024 ")

# pulling in imputed scores
imputed_scores <- dbGetQuery(con, "SELECT * FROM youth_thriving.factor_analysis_predicted ")


# pulling in demographics
binary <- dbGetQuery(con, "SELECT * FROM youth_thriving.demographics_binary_data ")

sogi <- dbGetQuery(con, "SELECT * FROM youth_thriving.gender_sexuality_data ")

race <- dbGetQuery(con, "SELECT * FROM youth_thriving.race_ethnicity_data ")

# Step 1: Setting up dataframe for average scores by demographics -----
# join data frames
svy_df_orig <- svy_data_raw %>%
  left_join(imputed_scores) %>%
  left_join(binary) %>%
  left_join(sogi%>%select(response_id,cisgender_mf:detailed_sexuality)) %>%
  left_join(race%>%select(response_id,isSouthAsian:detailed_nhpi))

# rename values for 0/1 variables so it's easier to read data results later
# new df and saving original df
svy_df <- svy_df_orig

# list of binary vars it can apply to systematically
binary_vars <- svy_df_orig %>% 
  select(bipoc:unhoused) %>%
  names()

# loop statement
for (i in binary_vars) { 
svy_df <- svy_df %>%
  mutate(!!paste({{i}}):=
           case_when(
             !!sym(i)==1 ~ !!paste({{i}}),
             !!sym(i)==0 ~ !!paste("not",{{i}}),
             TRUE ~ NA
           ) )
}

# test results
table(svy_df$bipoc,useNA='always')
table(svy_df_orig$bipoc,useNA='always')


# Step 2: Create function to calculate average component scores by youth demographics ----
# component names
avg_scores<-function (df,demo_var) {
  
components <- imputed_scores %>% 
  select(subcomponent_psychological_distress:subcomponent_experiences_of_racism_and_discrimination) %>%
  names()

mylist <- list()

for (i in components) { 
  
temp_df <- df %>% 
  as_survey(weights=c(weights_final)) %>% # apply survey weights to dataframe
  filter(!is.na(!!sym(i)), !is.na(!!as.symbol(demo_var))) %>% # filter out NAs for the demographic var and component var
  group_by(!!sym(demo_var)) %>% # group by demographic variable
  summarise(count=n(), # calculate the unweighted count for demo category that has values for component var
            avg=survey_mean(!!sym(i), vartype=c("se","ci"),level=.90)) %>% # get the average score
  rename(
    # !!paste({{i}},"avg",sep="_"):=avg, # old code that had data wide instead of long
    #      !!paste({{i}},"count",sep="_"):=count,
    #      !!paste({{i}},"se",sep="_"):=avg_se,
    #      !!paste({{i}},"low_ci",sep="_"):=avg_low,
    #      !!paste({{i}},"upp_ci",sep="_"):=avg_upp,
         subgroup=!!sym(demo_var)
 ) %>%
  mutate(component_model=!!paste({{i}}))
mylist[[i]] <- temp_df # assign table/list output to my empty list

}

all_vars_df<-dplyr::bind_rows(mylist) %>%
  select(subgroup, component_model, everything())

# final_df <-mylist %>% reduce(left_join,by="subgroup")


}

# test function
df_bipoc<-avg_scores(df=svy_df,demo_var="bipoc")

# test function results
test_df <- svy_df %>% 
  as_survey(weights=c(weights_final)) %>% # apply survey weights to dataframe
  filter(!is.na(subcomponent_psychological_distress), !is.na(bipoc)) %>% # filter out NAs for the demographic var and component var
  group_by(bipoc) %>% # group by demographic variable
  summarise(count=n(), # calculate the unweighted count meeting demo var and with values for component var
            avg=survey_mean(subcomponent_psychological_distress, vartype=c("se","ci"),level=.90))  # get the average score

         
# Step 3: Compute average scores per component in model for all youth -----------------
svy_df <- svy_df %>%
  mutate(total='all youth')

df_total<-avg_scores(df=svy_df,demo_var="total")

# check that weighting is working by running mean without the srvy package
mean(svy_df$subcomponent_psychological_distress,na.rm=TRUE)

# Step 4: Compute for binary vars -----------------
df_disconnected<-avg_scores(df=svy_df,demo_var="disconnected")

df_si<-avg_scores(df=svy_df,demo_var="systems_impacted")

df_arrested<-avg_scores(df=svy_df,demo_var="arrested")

df_suspended<-avg_scores(df=svy_df,demo_var="suspended")

df_undocumented<-avg_scores(df=svy_df,demo_var="undocumented")

df_unhoused<-avg_scores(df=svy_df,demo_var="unhoused")

# Step 5: Compute for SOGI -----------------
svy_df <- svy_df %>%
  mutate(cisgender_mf=
                   case_when(
                     cisgender_mf==1 ~ 'Cisgender Female',
                     cisgender_mf==0 ~ 'Cisgender Male',
                     TRUE ~ NA
                   ),
         cishet_lgbtqia=
           case_when(
             cishet_lgbtqia==1 ~ 'LGBTQIA+',
             cishet_lgbtqia==0 ~ 'not LGBTQIA+',
             TRUE ~ NA))

df_cis<-avg_scores(df=svy_df,demo_var="cisgender_mf")

df_lgbtia<-avg_scores(df=svy_df,demo_var="cishet_lgbtqia")

# Step 6: Compute for Race -----------------
# acs race categories, non-overlapping
df_race <- avg_scores(df=svy_df,demo_var="nh_race") # use our race category

table(df_race$subgroup)

df_race <- df_race %>% filter(!subgroup %in% c("NA","do_not_wish")) # filter out those that didn't respond

# aian, nhpi, swana alone or in combo
# list of alone or in combo vars
aoic_vars <- svy_df %>% 
  select(race_aian, race_nhpi, race_swana) %>%
  names()


# loop statement
for (i in aoic_vars) { 
  svy_df <- svy_df %>%
    mutate(!!paste({{i}}):=
             case_when(
               !!sym(i)==1 ~ !!paste({{i}}),
               !!sym(i)==0 ~ !!paste("not",{{i}}),
               TRUE ~ NA
             ) )
}


# test results
table(svy_df$race_aian,useNA='always')
table(svy_df_orig$race_aian,useNA='always')

# replace race_ in columns
svy_df[aoic_vars] <- lapply(svy_df[aoic_vars], gsub, pattern = "race_", replacement = "")

# test results
table(svy_df$race_aian,useNA='always')
table(svy_df_orig$race_aian,useNA='always')

# run function
df_aian<-avg_scores(df=svy_df,demo_var="race_aian")

df_nhpi<-avg_scores(df=svy_df,demo_var="race_nhpi")

df_swana<-avg_scores(df=svy_df,demo_var="race_swana")


# Step 6: Compute for age and SPA -----------------
df_spa<-avg_scores(df=svy_df,demo_var="spa_final_respondent")

# recode age
svy_df <- svy_df %>%
  mutate(age_minor_adult=
           case_when(
             age_minor_adult==1 ~ '17 and under',
             age_minor_adult==2 ~ '18-24',
             TRUE ~ NA
           ))

table(svy_df_orig$age_minor_adult, useNA='always')

table(svy_df$age_minor_adult, useNA='always')

df_age<-avg_scores(df=svy_df,demo_var="age_minor_adult")


# Step 7: Export to postgres -----------------
# functions to help export
source("W:\\RDA Team\\R\\Github\\RDA Functions\\main\\RDA-Functions\\Utility_Functions.R")

# overall metadata for each table
schema <- "youth_thriving"
indicator <- " A table that has the average imputed factor or component scores of those being used in the youth thriving model by selected demographics "
source <- "
Script:W:/Project/OSI/Bold Vision/Youth Thriving Survey/GitHub/EMG/boldvision_youththriving/Factor Analysis and Correlations/factor_analysis_predicted_avg_scores.R"
qa_filepath <- " See QA doc for details: W:/Project/OSI/Bold Vision/Youth Thriving Survey/Documentation/QA_circular_plots_scores.docx "

column_comments <- c(
  "the demographic group for the analysis ",
  "the factor or component model summarized",
  "the number of youth in the group and component model with imputed scores ",
  "the weighted average of the component model imputed score ",
  "the SE of the weighted average at 90%",
  "the lower 90% confidence interval of the avg imputed scores",
  "the upper 90% confidence interval of the avg imputed scores"
  )

## Race table export ------
df_final_race <- rbind(df_race, df_aian, df_nhpi, df_swana, df_bipoc)

table_name <- "factor_analysis_avg_scores_race"
demographic <- " for non-hispanic race + aian + nhpi + swana alone or in combination + bipoc "
table_comment <- paste0(indicator, demographic, source)
column_names <- colnames(df_final_race) # Get column names
# 
# # write table
# dbWriteTable(con, Id(schema, table_name), df_final_race,
#              overwrite = FALSE, row.names = FALSE)
# 
# # Comment on table and columns
# add_table_comments(con, schema, table_name, indicator, source, qa_filepath, column_names, column_comments)
# 

## SOGI table export ------
df_final_sogi <- rbind(df_cis, df_lgbtia)

table_name <- "factor_analysis_avg_scores_sogi"
demographic <- " for gender and sexuality data including cisgender male/female and lgbtqia+ "
table_comment <- paste0(indicator, demographic, source)
column_names <- colnames(df_final_sogi) # Get column names

# # write table
# dbWriteTable(con, Id(schema, table_name), df_final_sogi,
#              overwrite = FALSE, row.names = FALSE)
# 
# # Comment on table and columns
# add_table_comments(con, schema, table_name, indicator, source, qa_filepath, column_names, column_comments)

## Age table export ------
table_name <- "factor_analysis_avg_scores_age"
demographic <- " for age minor or adult "
table_comment <- paste0(indicator, demographic, source)
column_names <- colnames(df_age) # Get column names

# # write table
# dbWriteTable(con, Id(schema, table_name), df_age,
#              overwrite = FALSE, row.names = FALSE)
# 
# # Comment on table and columns
# add_table_comments(con, schema, table_name, indicator, source, qa_filepath, column_names, column_comments)

## SPA table export ------
table_name <- "factor_analysis_avg_scores_spa"
demographic <- " for service planning area "
table_comment <- paste0(indicator, demographic, source)
column_names <- colnames(df_spa) # Get column names
# 
# # write table
# dbWriteTable(con, Id(schema, table_name), df_spa,
#              overwrite = FALSE, row.names = FALSE)
# 
# # Comment on table and columns
# add_table_comments(con, schema, table_name, indicator, source, qa_filepath, column_names, column_comments)

## Other binary vars table export ------
df_final_sys <- rbind(df_arrested, df_disconnected, df_suspended, df_undocumented, df_unhoused,df_si)

table_name <- "factor_analysis_avg_scores_systems_involved"
demographic <- " for systems involved indicators, e.g., ever arrested, disconnected frome ducation/school, ever suspended, undocumented, or unhoused "
table_comment <- paste0(indicator, demographic, source)
column_names <- colnames(df_final_sys) # Get column names

# # write table
# dbWriteTable(con, Id(schema, table_name), df_final_sys,
#              overwrite = FALSE, row.names = FALSE)
# 
# # Comment on table and columns
# add_table_comments(con, schema, table_name, indicator, source, qa_filepath, column_names, column_comments)

## All youth table export ------
table_name <- "factor_analysis_avg_scores_total"
demographic <- " for all youth"
table_comment <- paste0(indicator, demographic, source)
column_names <- colnames(df_total) # Get column names

# write table
dbWriteTable(con, Id(schema, table_name), df_total,
             overwrite = FALSE, row.names = FALSE)

# Comment on table and columns
# add_table_comments(con, schema, table_name, indicator, source, qa_filepath, column_names, column_comments)

dbDisconnect(con)
