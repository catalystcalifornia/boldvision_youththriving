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
library(GGridge)

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

fx_create_df <- function(con, tables, response_domain, variable) {
  
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
    
    # Add 'source_table' and 'youth' columns
    df <- df %>%
      mutate(source_table = table,
             youth_label = str_replace_all(youth, "_", " ") %>%  # Replace underscores with spaces
               str_to_title(),  # Capitalize first letter of each word
             youth_label = if_else(str_detect(youth_label, "Youth$"), youth_label, paste(youth_label, "Youth"))
      )
    
    
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
df_combined <- fx_create_df(con, tables, "Vibrant Communities", "ds") 

#Adding all youth data 
tot_freq_all_youth <- dbGetQuery(con, "SELECT response, frequency AS count, weighted_percent AS rate, percent_cv AS rate_cv, variable, question, sub_question, variable_name, domain AS response_domain
                                 FROM youth_thriving.tot_freq_vibrant_community WHERE variable = 'ds'") %>%
  mutate(youth = "all youth",
         youth_label = "All Youth",
         source_table = "tot_freq_vibrant_community")

#combine with other data 
df_combined <- bind_rows(df_combined, tot_freq_all_youth)

df_filtered <- df_combined %>%
  filter(!grepl("^not ", .[[1]], ignore.case = TRUE)) %>%  # Filters out rows where first column starts with "not "
mutate(response = str_wrap(response, width = 8))

#visual
df_visual <- ggplot(df_filtered, aes(x = response, y = rate, fill = youth)) +
  geom_bar(stat = "identity") +  # Use identity to plot actual counts
  # scale_fill_manual(values = c(
  #   "All Youth" = gray,
  #   "Bipoc Youth" = pink,
  #   "Disconnected Youth" = dark_pink,
  #   "Lgbtqia Youth" = orange,
  #   "Suspended Youth" = yellow,
  #   "Systems Impacted Youth" = light_green,
  #   "Undocumented Youth" = blue,
  #   "Unhoused Youth" = light_blue
  # )) +
  facet_wrap(~ youth_label, scales = "free_y", nrow = 2) +  # Create small multiples
  #bar labels
  geom_text(data = df_filtered,
            aes(label = paste0(round(rate, digits = 1), "%")),
            size = 4,
            stat="identity", colour = "black",
            # position = position_dodge(width = 1), 
            # vjust = -.20 , 
            # hjust= 1.15,
            fontface = "bold", family=font_bar_label) +  
  theme_minimal() +
  labs(title = paste(str_wrap("Unhoused youth are least likely to report access to Libraries", whitespace_only = TRUE, width = 75), collapse = "\n"),
       x = paste(str_wrap("Youth Thriving Survey Responses", whitespace_only = TRUE, width = 95), collapse = "\n"),
       y = "",
       caption= paste(str_wrap(paste0("Question: ", unique(df_filtered$question), " ",
                                      unique(df_filtered$sub_question), ".\n",
                                      " Component: ", unique(df_filtered$response_domain), ",\n",
                                      " Subcomponent: ", unique(df_filtered$variable_name), ".\n",
                                      " Data Source: Bold Vision Youth Thriving Survey, 2024."),
                               whitespace_only = TRUE, width = 165), collapse = "\n")) +
  theme(legend.position = "none",
     # define style for axis text
     axis.text.x = element_text(hjust = 0.5, vjust = 1, lineheight = .75),  # x-axis labels
     axis.text.y = element_text(size = 9, colour = "black", family= font_axis_label, face = "bold"),
     axis.title.x = element_text(size = 12, colour = "black", family = font_axis_label, face = "bold", margin = margin(t = 5)),
     # define style for title and caption
     plot.caption = element_text(hjust = 0.0, size = 8, colour = "black", family = font_caption, face = "plain"),
     plot.title = element_text(hjust = 0.0, size = 18, colour = "black", family = font_title, face = "bold"),
     # grid line style
     panel.grid.minor = element_blank(),
     panel.grid.major = element_blank()) 

df_visual

ggsave(plot=df_visual, 
       file=paste0("W:/Project/OSI/Bold Vision/Youth Thriving Survey/Deliverables/testing_small_multiples", ".svg"),
       units = c("in"),  width = 8, height = 5.5)



