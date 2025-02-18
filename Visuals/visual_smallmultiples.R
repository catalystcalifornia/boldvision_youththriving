# The purpose of this script is to develop a visual template for producing small multiple visuals for the BVYTS data analysis. 
# Author: Maria Khan 
# QA DODCUMENT: 

####STEP 1: Setting Up (Libraries, BV style, database connecction, etc)####
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
library(GGRidge)

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
    
    # Add 'source_table' and 'youth' columns
    df <- df %>%
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
                 str_detect(youth_label, "twoormor") ~ "Two or More",  # Rename "Twoormor" to "Two or More"
                 TRUE ~ str_to_title(youth_label)  # Capitalize first letter of each word otherwise
               )) 
    
    return(df)
  }
  
  #fetch and combine data from all tables, removing NULLs
  all_demo_data <- bind_rows(lapply(tables, fetch_data))
  
  #adding all youth data 
  tot_freq_all_youth <- dbGetQuery(con, sprintf("SELECT response, frequency AS count, weighted_percent AS rate, percent_cv AS rate_cv, variable, question, sub_question, variable_name, domain AS response_domain
                                 FROM youth_thriving.%s WHERE variable = '%s'", response_domain_table, variable)) %>%
    mutate(youth = "all youth",
           youth_label = "All Youth",
           source_table = response_domain_table)
  
  #combine with other data 
  df_combined <- bind_rows(all_demo_data, tot_freq_all_youth)
  
  #filter for rows that are not needed like not bipoc, not lgbtqia, etc.and those that have responses we don't want like Don't knows, etc. 
  df_final <- df_combined %>%
    filter(!grepl("^not ", .[[1]], ignore.case = TRUE)) %>%  # Filters out rows where first column starts with "not "
    filter(!response %in% c("Don't wish to answer", "Don't know", "Not sure", "Not Sure", "Does not apply to me")) %>% #Fiters out rows with responses we don't want to visualize
    mutate(response = str_wrap(response, width = 8)) %>% #wrapping for better labeling in the visuals
    mutate(youth_label = str_wrap(youth_label, width = 11)) %>%
    # group_by(youth_label) %>% # Order by rate in descending order (highest to lowest)
    arrange(desc(rate)) 
    # ungroup()  # Remove the grouping
  return(df_final)
  
}


####STEP 3: Run Function with the list of tables that are of interest See example ####
# List of tables
tables <- c("response_analysis_per_race", "response_analysis_per_bipoc", 
            "response_analysis_per_disconnected", "response_analysis_per_lgbtqia",
            "response_analysis_per_suspended", "response_analysis_per_systems_impacted", 
            "response_analysis_per_undocumented", "response_analysis_per_unhoused")

# Running function
df_ex <- fx_create_df(con, tables, "Vibrant Communities", "ds", "tot_freq_vibrant_community") 

View(df_ex) #check example table, does everything look like it is working okay? 

####STEP 4: Create a function to produce small multiple visuals from the df just produced####

fx_vis_smallmultiples <- function(df, title_text, domain_text, variable_text) {

  df_visual <- ggplot(df, aes(x = response, y = rate, fill = response)) +
  geom_bar(stat = "identity") +  # Use identity to plot actual counts
  facet_wrap(~ youth_label, scales = "free_y", nrow = 2) +  # Create small multiples
  #bar labels
  geom_text(data = df,
            aes(label = paste0(round(rate, digits = 1), "%")),
            size = 4,
            stat="identity", colour = "black",
            # position = position_dodge(width = 1), 
            # vjust = -.20 , 
            # hjust= 1.15,
            fontface = "bold", family=font_bar_label) +  
  theme_minimal() +
  labs(title = paste(str_wrap(title_text, whitespace_only = TRUE, width = 75), collapse = "\n"),
       x = "",  #"paste(str_wrap("Youth Thriving Survey Responses", whitespace_only = TRUE, width = 95), collapse = "\n")",
       y = "",
       fill = "Youth Thriving Survey Responses:",  # Legend title
       caption= paste(str_wrap(paste0("Question: ", unique(df$question), " ",
                                      unique(df$sub_question), ".\n",
                                      " Component: ", unique(df$response_domain), ",\n",
                                      " Subcomponent: ", unique(df$variable_name), ".\n",
                                      " Data Source: Bold Vision Youth Thriving Survey, 2024."),
                               whitespace_only = TRUE, width = 165), collapse = "\n")) +
  # Define custom BV colors 
  scale_fill_manual(values = c(yellow, pink, dark_pink, orange)) + 
  theme(legend.position = "bottom",  # Show legend on the right
     # remove axis text
     axis.text.x = element_blank(), 
     axis.text.y = element_blank(),
     # define style for legend
     legend.text = element_text(size = 12, colour = "black", family = font_axis_label, face = "bold", margin = margin(t = 5)),
     legend.title = element_text(size = 12, colour = "black", family = font_axis_label, face = "bold", margin = margin(t = 5)),
     # define style for title and caption
     plot.caption = element_text(hjust = 0.0, size = 8, colour = "black", family = font_caption, face = "plain"),
     plot.title = element_text(hjust = 0.0, size = 18, colour = "black", family = font_title, face = "bold"),
     # grid line style
     panel.grid.minor = element_blank(),
     panel.grid.major = element_blank()) 

return(df_visual)

ggsave(plot=df_visual, 
       file=paste0("W:/Project/OSI/Bold Vision/Youth Thriving Survey/Deliverables/", domain_text, "/", variable_text,
                   # unique(df$response_domain),"/", unique(df$variable), NOT SURE WHY THIS LINE Is NOT WORKING! 
                   "_smallmultiples", ".svg"),
       units = c("in"),  width = 8, height = 5.5)
}


####Step 5: Run function to create visual ####
fx_vis_smallmultiples(df = df_ex, 
                      title_text = 'Unhoused youth are least likely to report access to Libraries',
                      domain_text = 'Vibrant Communities',
                      variable_text = 'ds_TEST')


###Step 6: Close database connection ####
dbDisconnect(con)
       