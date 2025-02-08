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
svy_data <- dbGetQuery(con, "SELECT response_id, age_minor_adult, spa_final_respondent, spa_name_final_respondent, weights_final 
                       FROM youth_thriving.raw_survey_data")

# pulling data dictionary 
svy_dd <- dbGetQuery(con, "SELECT * FROM youth_thriving.bvys_datadictionary_2024 ")

# pulling in imputed scores
imputed_scores <- dbGetQuery(con, "SELECT * FROM youth_thriving.factor_analysis_predicted ")


# pulling in demographics
binary <- dbGetQuery(con, "SELECT * FROM youth_thriving.demographics_binary_data ")

sogi <- dbGetQuery(con, "SELECT * FROM youth_thriving.gender_sexuality_data ")

race <- dbGetQuery(con, "SELECT * FROM youth_thriving.race_ethnicity_data ")

# join data frames
svy_df_orig <- svy_data %>%
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


# Step 1: Create function to calculate average component scores by youth demographics ----
# component names
avg_scores<-function (df,demo_var) {
  
components <- imputed_scores %>% 
  select(subcomponent_psychological_distress:subcomponent_experiences_of_racism_and_discrimination_sum) %>%
  names()

mylist <- list()

for (i in components) { 
  
temp_df <- df %>% 
  as_survey(weights=c(weights_final)) %>% # apply survey weights to dataframe
  filter(!is.na(!!sym(i)), !is.na(!!as.symbol(demo_var))) %>% # filter out NAs for the demographic var and component var
  group_by(!!sym(demo_var)) %>% # group by demographic variable
  summarise(count=n(), # calculate the unweighted count meeting demo var and with values for component var
            avg=survey_mean(!!sym(i), vartype=c("se","ci"),level=.90)) %>% # get the average score
  rename(
    # !!paste({{i}},"avg",sep="_"):=avg,
    #      !!paste({{i}},"count",sep="_"):=count,
    #      !!paste({{i}},"se",sep="_"):=avg_se,
    #      !!paste({{i}},"low_ci",sep="_"):=avg_low,
    #      !!paste({{i}},"upp_ci",sep="_"):=avg_upp,
         subgroup=!!sym(demo_var)
 ) %>%
  mutate(component=!!paste({{i}}))
mylist[[i]] <- temp_df # assign table/list output to my empty list

}

all_vars_df<-dplyr::bind_rows(mylist)

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

         
# Step 2: Compute average scores per component in model for all youth -----------------
svy_df <- svy_df %>%
  mutate(total='all youth')

df_total<-avg_scores(df=svy_df,demo_var="total")

# Step 3: Compute for binary vars -----------------
df_total<-avg_scores(df=svy_df,demo_var="total")

