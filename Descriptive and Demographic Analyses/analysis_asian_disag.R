# The following script will create crosstab tables for the variables by Asian Subgroups
# Author: Maria Khan 
# QA DOC: W:\Project\OSI\Bold Vision\Youth Thriving Survey\Documentation\QA_asian_youth_analysis.docx

#### STEP 1: Setting Up and Downloading Tables ####
library(dplyr)
library(RPostgreSQL)
library(srvyr)
library(stringr)
options(scipen=999)

# connect to postgres and pull credentials
source("W:\\RDA Team\\R\\credentials_source.R")
con <- connect_to_db("bold_vision")

#raw survey data 
svy_data <- dbGetQuery(con, "SELECT * FROM youth_thriving.raw_survey_data")
#data dictionary
svy_dd <- dbGetQuery(con, "SELECT * FROM youth_thriving.bvys_datadictionary_2024 where response_type = 'mc' AND response_domain !='Demographics' AND response_domain != 'Info'")
#race data
race_df <- dbGetQuery(con, 'SELECT response_id, nh_race, race_asian, detailed_asian, detailed_race, 
                      "isSouthAsian" AS south_asian, 
                       "isSoutheastAsian" AS southeast_asian
                         FROM youth_thriving.race_ethnicity_data where detailed_asian IS NOT NULL')
#outputs 476 response_ids that identified as some form of Asian (multiracial included)

#close connections
dbDisconnect(con)

#join to have a df with the detailed asian categories and filter out any survey respondents that did not identify with this category
svy_data <- race_df %>%
  left_join(svy_data, by = "response_id")


####STEP 2: Create a function for detailed asian crosstabing with a survey question####
fx_detailed_asian <- function(variable_input) {
  dict <- svy_dd %>%
    filter(variable == variable_input) %>%
    pivot_longer(cols = response_1:response_12,
                 names_to = "response_numbers",
                 values_to = "response",
                 values_drop_na = TRUE)
  
  dict_var <- dict %>%
    mutate(variable_merge_col = 1:nrow(dict))
  
  df_var <- as_survey(svy_data, weights = !!sym("weights_final")) %>%
    filter(!is.na(!!sym(variable_input))) %>%  # Remove NAs
    group_by(detailed_asian, !!sym(variable_input)) %>%
    summarise(count = n(), 
              rate = survey_mean(vartype = c("cv", "se"), level = .90), .groups = "drop") %>%
    mutate(rate = rate * 100, 
           rate_cv = rate_cv * 100, 
           moe = rate_se * 1.645 * 100) 
  
  df_final <- merge(x = df_var, y = dict_var, 
                    by.x = variable_input, by.y = "variable_merge_col") %>%
    relocate(response) %>%
    select(-notes)
  
  return(df_final)
}

####STEP 3: Run functions for variables of interest####
df_co <- fx_detailed_asian("co")  

df_dl <- fx_detailed_asian("dl")

####STEP 4: Create function for south asian and southeast asian with a survey question ####
fx_subgroups_asian <- function(variable_input) {
  dict <- svy_dd %>%
    filter(variable == variable_input) %>%
    pivot_longer(cols = response_1:response_12,
                 names_to = "response_numbers",
                 values_to = "response",
                 values_drop_na = TRUE)
  
  dict_var <- dict %>%
    mutate(variable_merge_col = 1:nrow(dict))
  
  svy_data <- svy_data %>%
    mutate(
      subgroup_asian = case_when(
        south_asian == "1" ~ "South Asian",
        southeast_asian == "1" ~ "Southeast Asian",
        race_asian == "1" ~ "Asian",
        TRUE ~ NA_character_
      )
    )
  df_asian_combined <- as_survey(svy_data, weights = !!sym("weights_final")) %>%
    filter(!is.na(!!sym(variable_input)), !is.na(subgroup_asian)) %>%
    group_by(subgroup_asian, !!sym(variable_input)) %>%
    summarise(
      count = n(),
      rate = survey_mean(vartype = c("cv", "se"), level = .90),
      .groups = "drop"
    ) %>%
    mutate(
      rate = rate * 100,
      rate_cv = rate_cv * 100,
      moe = rate_se * 1.645 * 100
    )
  
  df_final <- merge(x = df_asian_combined, y = dict_var, 
                    by.x = variable_input, by.y = "variable_merge_col") %>%
    relocate(response) %>%
    select(-notes)
  
  return(df_final)
}

####STEP 5: Run subgroup function for variables of interest ####
df_co_subgroup <- fx_subgroups_asian("co")

df_dl_subgroup <-fx_subgroups_asian("dl")

# test recoding
svy_data_check <- svy_data %>%
  mutate(
    subgroup_asian = case_when(
      south_asian == "1" ~ "South Asian",
      southeast_asian == "1" ~ "Southeast Asian",
      race_asian == "1" ~ "Asian",
      TRUE ~ NA_character_
    )
  )

svy_data_check%>%filter(south_asian==1)%>%group_by(detailed_asian)%>%summarise(count=n())

svy_data_check%>%filter(southeast_asian==1)%>%group_by(detailed_asian)%>%summarise(count=n())

# svy weights check
qa<-svy_data%>%
  filter(!is.na(detailed_asian) & !is.na(co))%>%
  group_by(detailed_asian,co)%>%
  summarise(count=n(), .groups = "drop")%>%
  group_by(detailed_asian)%>%
  mutate(total=sum(count),
         rate=count/total)

table(svy_data$co)

svy_data_qa<-svy_data %>% mutate(co_binary=ifelse(is.na(co), NA,
                                                  ifelse(co==5, NA,
                                                         ifelse(co<=2,0,
                                                                ifelse(co>=3, 1,
                                                                       NA)))))

table(svy_data_qa$co_binary)

qa_binary <- as_survey(svy_data_qa, weights = weights_final) %>%
  filter(!is.na(co_binary), !is.na(detailed_asian)) %>%
  group_by(detailed_asian, co_binary) %>%
  summarise(
    count = n(),
    rate = survey_mean(vartype = c("cv", "se"), level = .90),
    .groups = "drop"
  )%>%
  group_by(detailed_asian)%>%
  mutate(total=sum(count))


table(svy_data$dl)

table(svy_data$dk) # more skewed

svy_data_qa<-svy_data_qa %>% mutate(dl_binary=ifelse(is.na(dl), NA,
                                                  ifelse(dl==5, NA,
                                                         ifelse(dl<=2,0,
                                                                ifelse(dl>=3, 1,
                                                                       NA)))))

qa_binary_dl <- as_survey(svy_data_qa, weights = weights_final) %>%
  filter(!is.na(dl_binary), !is.na(detailed_asian)) %>%
  group_by(detailed_asian, dl_binary) %>%
  summarise(
    count = n(),
    rate = survey_mean(vartype = c("cv", "se"), level = .90),
    .groups = "drop"
  )%>%
  group_by(detailed_asian)%>%
  mutate(total=sum(count))

# test visual
library(ggplot2)
plot_data <- df_co %>%
  filter(co==4)

ggplot(plot_data, aes(x = detailed_asian, y = rate)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0("   = ", round(count,0))), hjust = -0.1, size = 3) + 
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +  # Add space on the right
  theme_minimal() +
  guides(fill = "none") +
  labs(x = NULL, y = "")

plot_data <- df_co_subgroup

ggplot(plot_data, aes(x = subgroup_asian, y = rate,fill=response)) +
  geom_bar(stat = "identity") +
  # geom_text(aes(label = paste0(response,"=", round(count,0)))) + 
  # coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +  # Add space on the right
  # theme_minimal() +
  # guides(fill = "none") +
  labs(x = NULL, y = "")

plot_data <- df_dl_subgroup

ggplot(plot_data, aes(x = subgroup_asian, y = rate,fill=response)) +
  geom_bar(stat = "identity") +
  # geom_text(aes(label = paste0(response,"=", round(count,0)))) + 
  # coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +  # Add space on the right
  # theme_minimal() +
  # guides(fill = "none") +
  labs(x = NULL, y = "")


plot_data <- qa_binary %>%
  filter(total>=9)

ggplot(plot_data, aes(x = detailed_asian, y = rate, fill=as.character(co_binary))) +
  geom_bar(stat = "identity") +
  # geom_text(aes(label = paste0("   = ", round(count,0))), hjust = -0.1, size = 3) + 
  # coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +  # Add space on the right
  # theme_minimal() +
  # guides(fill = "none") +
  labs(x = NULL, y = "")


plot_data <- qa_binary_dl %>%
  filter(total>=9)

ggplot(plot_data, aes(x = detailed_asian, y = rate, fill=as.character(dl_binary))) +
  geom_bar(stat = "identity") +
  # geom_text(aes(label = paste0("   = ", round(count,0))), hjust = -0.1, size = 3) + 
  # coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +  # Add space on the right
  # theme_minimal() +
  # guides(fill = "none") +
  labs(x = NULL, y = "")