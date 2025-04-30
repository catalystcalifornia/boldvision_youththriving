# Functions for running crosstabs for two or three variables

library(tidyverse)
library(srvyr)
library(RPostgres)
library(ggplot2)
library(dplyr)
library(forcats)

# Get the weighted rates and unweighted counts for a two variable crosstab
crosstab_two_vars <- function(df,var1, var2){
  df %>% as_survey_design(weights = weights_final) %>%
    filter(!is.na(!!sym(var1)))%>%
    group_by(!!sym(var1), !!sym(var2)) %>%
    summarise(count=n(), # participant records count
              rate = survey_prop(vartype=c("cv","se"),level=.90)) %>% 
    mutate(rate = rate * 100,
           rate_se = rate_se * 100,
           rate_cv=rate_cv*100) %>%  # Convert to percentages
    ungroup()%>%
    filter(!is.na(!!sym(var2)))
}

# Get the weighted rates and unweighted counts for a three variable crosstab
crosstab_three_vars <- function(df, var1,var2, var3){
  df %>% as_survey_design(weights = weights_final) %>%
    filter(!is.na(!!sym(var1)) & !is.na(!!sym(var2)) )%>%
    group_by(!!sym(var1), !!sym(var2),!!sym(var3)) %>%
    summarise(count=n(), # participant records count
              rate = survey_prop(vartype=c("cv","se"),level=.90)) %>% 
    mutate(rate = rate * 100,
           rate_se = rate_se * 100,
           rate_cv=rate_cv*100) %>%  # Convert to percentages
    ungroup()%>%
    filter(!is.na(!!sym(var3)))
}

# Add data dictionary for one variable that has been grouped
datadict_one_var <- function(df,var1, response_0, response_1, dict_var){
  dict_var1 <- svy_dd %>%
    filter(variable == dict_var) %>%
    select(variable, question, sub_question)
  
  temp_df<-cbind(df,dict_var1)
  
  final_df<-temp_df%>%
    mutate(response_label=
             case_when(
               !!sym(var1)==0 ~ response_0,
               !!sym(var1)==1 ~ response_1            
             ))
}

# Add data dictionary for two variables that have been grouped
datadict_two_var <- function(df,var1, var1_response_0, var1_response_1, dict_var_1, var2, var2_response_0, var2_response_1, dict_var_2){
  dict_var1 <- svy_dd %>%
    filter(variable == dict_var_1) %>%
    select(variable, question, sub_question)
  
  colnames(dict_var1) <- paste("var1", colnames(dict_var1), sep="_")
  
  dict_var2 <- svy_dd %>%
    filter(variable == dict_var_2) %>%
    select(variable, question, sub_question)
  
  colnames(dict_var2) <- paste("var2",colnames(dict_var2), sep="_")
  
  temp_df<-cbind(df,dict_var1,dict_var2)
  
  final_df<-temp_df%>%
    mutate(var1_response_label=
             case_when(
               !!sym(var1)==0 ~ var1_response_0,
               !!sym(var1)==1 ~ var1_response_1            
             ),
           var2_response_label=
             case_when(
               !!sym(var2)==0 ~ var2_response_0,
               !!sym(var2)==1 ~ var2_response_1            
             )
    )
}
