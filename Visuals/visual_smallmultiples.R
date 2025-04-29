# The purpose of this script is to develop a visual template for producing small multiple visuals for the BVYTS data analysis. 
# Author: Maria Khan 
# QA DOCUMENT: W:\Project\OSI\Bold Vision\Youth Thriving Survey\Documentation\QA_smallmultiplesfunction.docx

####STEP 1: Setting Up (Libraries, BV style, database connecction, etc)####
library(extrafont)
library(tidyverse)
library(here)
library(dplyr)
library(data.table)
library(sf)
library(ggplot2)
library(RPostgres)
library(formattable)
library(svglite)
library(stringr)
library(tidyr)
library(showtext)
library(scales)
library(kableExtra)
library(flextable)
library(ggchicklet)
library(GGRidge)


#Load BV styling, colors and fonts
source('Visuals\\BV_styling.R')

source("W:\\RDA Team\\R\\credentials_source.R")
con <- connect_to_db("bold_vision")

####STEP 2: Function to download tables and merge into one df for visualization ####

fx_create_df <- function(con, tables, response_domain, variable, response_domain_table) {
  
  # Function to fetch data from a single table
  fetch_data <- function(table) {
    query <- sprintf("SELECT * FROM youth_thriving.%s WHERE response_domain = '%s' AND variable = '%s'",
                     table, response_domain, variable)
    
    # Use tryCatch to handle potential query errors
    df <- tryCatch(
      dbGetQuery(con, query),
      error = function(e) {
        message(sprintf("Error querying table %s: %s", table, e$message))
        return(NULL)
      }
    )
    
    # Ensure df is a valid data frame before proceeding
    if (is.null(df) || nrow(df) == 0) return(NULL)
    
    # Rename first column to youth
    colnames(df)[1] <- "youth"
    
    # Add 'source_table' and 'youth_label' columns
    df <- df %>%
      filter(!youth %in% c("nh_aian", "nh_nhpi", "nh_swana")) %>%  # remove nh because we will be using aoic results 
      mutate(source_table = table,
             youth_label = youth %>%
               str_replace_all("^nh_", "") %>%  # Remove "nh_" at the start of the string
               str_replace_all("_", " "), # Replace underscores with spaces
             youth_label = 
               case_when(
                 str_detect(youth_label, "lgbtqia") ~ str_to_upper(youth_label),
                 str_detect(youth_label, "bipoc") ~ str_to_upper(youth_label),
                 str_detect(youth_label, "swana") ~ str_to_upper(youth_label),
                 str_detect(youth_label, "aian") ~ str_to_upper(youth_label),
                 str_detect(youth_label, "nhpi") ~ str_to_upper(youth_label),
                 str_detect(youth_label, "twoormor") ~ "Multiracial",  # Rename "Twoormor" to "Two or More"
                 str_detect(youth_label, "latinx") ~ "Latine",  # Rename "Latinx" to "Latine
                 TRUE ~ str_to_title(youth_label)  # Capitalize first letter of each word otherwise
               )) 
    
    return(df)
  }
  
  #fetch and combine data from all tables, removing NULLs
  all_demo_data <- bind_rows(lapply(tables, fetch_data))
  
  #adding all youth data 
  tot_freq_all_youth <- dbGetQuery(con, sprintf("SELECT response, frequency AS count, weighted_percent AS rate, percent_cv AS rate_cv, variable, question, sub_question, likert_type, variable_name, domain AS response_domain
                                 FROM youth_thriving.%s WHERE variable = '%s'", response_domain_table, variable)) %>%
    mutate(youth = "all youth",
           youth_label = "All Youth",
           source_table = response_domain_table)
  
  #combine with other data 
  df_combined <- bind_rows(all_demo_data, tot_freq_all_youth)

  #filter for rows that are not needed like not bipoc, not lgbtqia, etc.and those that have responses we don't want like Don't knows, etc. 
  df_final <- df_combined %>%
    filter(!grepl("^not ", .[[1]], ignore.case = TRUE)) %>%  # Filters out rows where first column starts with "not "
    filter(!response %in% c("Don't wish to answer", "Don't know", "Does not apply to me")) %>% #Fiters out rows with responses we don't want to visualize
    mutate(youth_label = case_when(
      str_count(youth_label, " ") == 0 & nchar(youth_label) > 8 ~ str_replace(youth_label, "(.{4,5})", "\\1-\n"),  # Insert break for long single words
      TRUE ~ str_wrap(youth_label, width = 8.5)))  %>%
    arrange(desc(rate)) 
  
  return(df_final)
  
}


####STEP 3: Run Function with the list of tables that are of interest See example, set factor levels ####
# List of tables
tables <- c("response_analysis_per_race", 
            "response_analysis_per_bipoc", 
            # "response_analysis_per_disconnected", 
            "response_analysis_per_lgbtqia",
            # "response_analysis_per_suspended",
            # "response_analysis_per_arrested",
            "response_analysis_per_systems_impacted", 
            "response_analysis_per_undocumented", 
            "response_analysis_per_unhoused",
            "response_analysis_per_cisgender",
            "response_analysis_per_swana",
            "response_analysis_per_nhpi",
            "response_analysis_per_aian")

# Running function
df_ex <- fx_create_df(con, tables, "Vibrant Communities", "ds", "tot_freq_vibrant_community") 

# View(df_ex) #check example table, does everything look like it is working okay? 

#define factor levels in order we want them
#NOTE THAT Don't Wish to Answer, Not Sure, and Does Not Apply to Me ARE OMMITTED IN THIS STEP
true_factors<-c("Never true","Sometimes true","Often true","Always true")
time_factors<-c("None of the time","A little of the time","Some of the time","Most of the time","All of the time")
time_factors_reverse<-c("All of the time", "Most of the time", "Some of the time", "A little of the time", "None of the time") #reverse so a greater number means a good outcome and a smaller number means a bad outcome
yes_factors<-c("No", "Yes")
yes_factors_reverse <- c("Yes", "No", "Not sure") #reverse so a greater number means a good outcome and a smaller number means a bad outcome
count_factors<-c("None","One","Two","Three or more")
freq_factors<-c("Never","Rarely","Sometimes","Most of the time","All of the time")
freq_factors_reverse<-c("All of the time", "Most of the time","Sometimes", "Rarely","Never") #reverse so a greater number means a good outcome and a smaller number means a bad outcome

####STEP 4: Create a function to produce small multiple visuals from the df just produced####

fx_vis_smallmultiples <- function(df, title_text, subtitle_text, likert_factors, graph_orderby
                                  ) {
  
  #order the individual graphs by descending order of desired response 
  df <- df %>%
    group_by(youth_label) %>%
    mutate(max_order = rate[response == graph_orderby]) %>%
    ungroup() %>%
    mutate(youth_label = reorder(youth_label, -max_order))  # Negative sign for descending order
  
  #now order response category in associated factor level
  df$response <- factor(df$response, levels = likert_factors)
  
  
  df_visual <- ggplot(df, aes(x = response, y = rate
                              , fill = response
                              )) +
  geom_bar(stat = "identity") +  # Use identity to plot actual counts
  # Define custom BV colors 
  scale_fill_manual(values = c(yellow, pink, dark_pink, orange, "#FFA55C")) + 
  facet_wrap(~ youth_label, scales = "free_x", nrow = 2, strip.position = "bottom") +  # Create small multiples
  #bar labels
  geom_text(data = df,
            aes(label = paste0(round(rate, digits = 0), "%")),
            size = 2.6,
            stat="identity", colour = "black",
            # fontface = "bold", 
            family=font_bar_label,
            vjust = -0.75) +  #move bar labels above
  theme_minimal() +
  labs(title = paste(str_wrap(title_text, whitespace_only = TRUE, width = 70), collapse = "\n"),
       subtitle = paste(str_wrap(paste0("Survey Question: ", subtitle_text), whitespace_only = TRUE, width = 85), collapse = "\n"),
       x = "",  #"paste(str_wrap("Youth Thriving Survey Responses", whitespace_only = TRUE, width = 95), collapse = "\n")",
       y = "",
       fill = "",  # Legend title
       caption= paste(str_wrap(paste0(
         " Data Source: Catalyst California calculations of Bold Vision Youth Thriving Survey, 2024.",
         " Note: AIAN=American Indian & Alaska Native; BIPOC=Black, Indigenous, People of Color; LGBTQIA+=Lesbian, Gay, Bisexual, Transgender, Queer, Intersex, Asexual, & Gender Nonconforming; NHPI: Native Hawaiian & Pacific Islander; SWANA=Southwest Asian & North African; Systems Impacted=Youth at any point in foster care, juvenile hall/probation camp jail/prison, group home/residential program, or lived with legal guardians."),
                               whitespace_only = TRUE, width = 115), collapse = "\n")) +
  theme(legend.position = "bottom",  # Show legend on the top/bottom
     # remove axis text
     axis.text.x = element_blank(), 
     axis.text.y = element_blank(),
     # define style for legend
     legend.text = element_text(size = 14, colour = "black", family = font_subtitle, 
                                # face = "bold",
                                margin = margin(t = 5)),
     legend.title = element_text(size = 12, colour = "black", family = font_axis_label, face = "plain", margin = margin(t = 5)),
     strip.text=element_text(size=12, family=font_axis_label),
     # define style for title and caption
     plot.caption = element_text(hjust = 0.0, size = 11, colour = "black", family = font_caption, face = "plain"),
     plot.title = element_text(hjust = 0.0, size = 18, colour = "black", family = font_title),
     plot.subtitle = element_text(hjust = 0.0, size = 15, colour = "black", family = font_subtitle, 
                                  margin = margin(b = 23)), #increase space between subtitle and plots because the high ones are getting cut off
     # grid line style
     panel.grid.minor = element_blank(),
     panel.grid.major = element_blank(),
     #space between facts/small multiple rows
     panel.spacing.y = unit(4, "lines"))  # Increase spacing between facet rows 
  
  ggsave(plot = df_visual, 
         file = paste0("W:/Project/OSI/Bold Vision/Youth Thriving Survey/Deliverables/", 
                       unique(df$response_domain), "/", unique(df$variable), "_smallmultiples.svg"),
         units = "in", width = 8, height = 10)
  ggsave(plot = df_visual, 
         file = paste0("W:/Project/OSI/Bold Vision/Youth Thriving Survey/Deliverables/", 
                       unique(df$response_domain), "/", unique(df$variable), "_smallmultiples.pdf"),
         units = "in", width = 8, height = 10)

  showtext_opts(dpi=300)
  
  ggsave(plot = df_visual, 
         file = paste0("W:/Project/OSI/Bold Vision/Youth Thriving Survey/Deliverables/", 
                       unique(df$response_domain), "/", unique(df$variable), "_smallmultiples.png"),
         units = "in", width = 8, height = 10)
  
  return(df_visual)

}


####Step 5: Run function to create visual ####
fx_vis_smallmultiples(df = df_ex,
                      title_text = 'Unhoused youth are least likely to report access to Libraries',
                      subtitle_text = '',
                      likert_factors = yes_factors, graph_orderby = "Yes")
#See example here: W:\Project\OSI\Bold Vision\Youth Thriving Survey\Deliverables\Vibrant Communities 

###Step 6: Close database connection ####
dbDisconnect(con)

####Notes Saved to fix function for input of likert type if time ####


# #define factor levels in order we want them 
# #NOTE THAT Don't Wish to Answer, Not Sure, and Does Not Apply to Me ARE OMMITTED IN THIS STEP
# true_factors<-c("Never true","Sometimes true","Often true","Always true") 
# time_factors<-c("None of the time","A little of the time","Some of the time","Most of the time","All of the time")
# time_factors_reverse<-c("All of the time", "Most of the time", "Some of the time", "A little of the time", "None of the time") #reverse so a greater number means a good outcome and a smaller number means a bad outcome
# yes_factors<-c("No", "Yes")
# yes_factors_reverse <- c("Yes", "No") #reverse so a greater number means a good outcome and a smaller number means a bad outcome
# count_factors<-c("None","One","Two","Three or more")
# freq_factors<-c("Never","Rarely","Sometimes","Most of the time","All of the time")
# freq_factors_reverse<-c("All of the time", "Most of the time","Sometimes", "Rarely","Never") #reverse so a greater number means a good outcome and a smaller number means a bad outcome
# 
# df_final <- df_almost_final %>%
#   mutate(
#     factor_levels = if_else(likert_type == 'true_scale', list(true_factors),
#                             if_else(likert_type == 'count_scale', list(count_factors),
#                                     if_else(likert_type == 'freq_scale', list(freq_factors),
#                                             if_else(likert_type == 'freq_scale_rev', list(freq_factors_reverse),
#                                                     if_else(likert_type == 'yes_scale', list(yes_factors),
#                                                             if_else(likert_type == 'yes_scale_rev', list(yes_factors_reverse),
#                                                                     if_else(likert_type == 'time_scale', list(time_factors),
#                                                                             if_else(likert_type == 'time_scale_rev', list(time_factors_reverse),
#                                                                                     list(character(0))))))))))) %>%
#   rowwise() %>%  
#   mutate(
#     response = factor(response, levels = unlist(factor_levels)),  # Ensure `factor_levels` is unlisted
#     factor_score = as.numeric(response)  # Convert to numeric for scoring
#   ) %>%
#   ungroup() %>%  
#   filter(!is.na(response))  # Remove rows where response is NA
