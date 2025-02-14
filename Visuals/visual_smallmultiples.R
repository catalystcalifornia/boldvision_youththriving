# The purpose of this script is to develop a visual template for producing small multiple visuals for the BVYTS data analysis. 
# Author: Maria Khan 
# QA DODCUMENT: 

####STEP 1: Setting Up (Libraries, BV style, etc)####
library(extrafont)
library(tidyverse)
library(here)
library(dplyr)
library(data.table)
library(sf)
library(ggplot2)
library(RPostgreSQL)
library(formattable)
library(svglite)
library(stringr)
library(tidyr)
library(showtext)
library(scales)
library(kableExtra)
library(flextable)
library(ggchicklet)

##Colors
gray <- "#D6D7D6"
pink <- "#F75EC1"
dark_pink <- "#EF4A66"
orange <- "#F57E20"
yellow <- "#FFBF00"
light_green <- "#00A75A"
dark_green <- "#00864A"
blue  <- "#2A12B2"
light_blue <- "#465adc"
## FONTS 
font_add(family = "Manifold CF", regular = "W:/Project/OSI/Bold Vision/BV 2021/Deliverables/Bold Vision Fonts/Manifold/Fonts/manifoldcf-heavy.otf")
font_add(family = "HelveticaNeueLTStdMdCn", regular = "W:/Project/OSI/Bold Vision/BV 2021/Deliverables/Bold Vision Fonts/Helvetica Neue LT Std/HelveticaNeueLTStd-MdCn.otf")
font_add(family = "HelveticaNeueLTStdHvCn", regular = "W:/Project/OSI/Bold Vision/BV 2021/Deliverables/Bold Vision Fonts/Helvetica Neue LT Std/HelveticaNeueLTStd-HvCn.otf")
font_add(family = "HelveticaNeueLTStdMdCnO", regular = "W:/Project/OSI/Bold Vision/BV 2021/Deliverables/Bold Vision Fonts/Helvetica Neue LT Std/HelveticaNeueLTStd-MdCnO.otf")
# font_import()
loadfonts(device = "win")
windowsFonts()
showtext_auto()
# define fonts in chart
font_title <- "Manifold CF"
font_subtitle <- "Manifold CF"
font_caption <- "HelveticaNeueLTStdMdCn"
font_bar_label <- "HelveticaNeueLTStdHvCn"
font_axis_label <- "HelveticaNeueLTStdMdCn"

####STEP 2: Function to download tables and merge into one df for visualization ####

source("W:\\RDA Team\\R\\credentials_source.R")
con <- connect_to_db("bold_vision")

fx_create_df <- function(con, tables, response_domain, variable, response) {
  
  # Function to fetch data from a single table
  fetch_data <- function(table) {
    query <- sprintf("SELECT * FROM youth_thriving.%s WHERE response_domain = '%s' AND variable = '%s' AND response = '%s'", 
                     table, response_domain, variable, response)
    
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
    
    # Add 'source_table' and 'youth' columns
    df <- df %>%
      mutate(source_table = table)
    
    return(df)
  }
  
  # Fetch and combine data from all tables, removing NULLs
  all_data <- bind_rows(lapply(tables, fetch_data))
  
  return(all_data)
}


####STEP 3: Run Function with the list of tables that are of interest ####
# List of tables
tables <- c("tot_freq_bipoc", "tot_freq_disconnected", "tot_freq_lgbtqia",
            "tot_freq_suspended", "tot_freq_systems_impacted", 
            "tot_freq_undocumented", "tot_freq_unhoused")
# Running function
df_combined <- fx_create_df(con, tables, "Vibrant Communities", "ds", "Yes") 

#Adding all youth data 
tot_freq_all_youth <- dbGetQuery(con, "SELECT response, frequency AS count, weighted_percent AS rate, percent_cv AS rate_cv, variable, question, sub_question, variable_name, domain AS response_domain
                                 FROM youth_thriving.tot_freq_vibrant_community WHERE variable = 'ds' AND response = 'Yes'") %>%
  mutate(youth = "all youth",
         source_table = "tot_freq_vibrant_community")

#combine with other data 
df_combined <- bind_rows(df_combined, tot_freq_all_youth)

ggplot(df_combined, aes(x = youth, y = count, fill = source_table)) +
  geom_bar(stat = "identity") +  # Use identity to plot actual counts
  facet_wrap(~ source_table, scales = "free_y") +  # Create small multiples
  labs(
    title = "Small Multiple Bar Graphs by Table Name",
    x = "Youth",
    y = "Count"
  ) +
  theme_minimal() +
  theme(legend.position = "none")  # Remove legend since bars are labeled
