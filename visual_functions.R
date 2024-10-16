####User-Defined R Functions to create Visuals for the Bold vision Youth Thriving Survey Data 
#Author: Maria Khan

# The following file aims to serve as a visuals template for the Bold Vision Youth Thriving Survey 

#### Step 1: Loading Libraries ####
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

#### Step 2: Setting Bold Vision Style Guide####

##Colors

gray <- "#D6D7D6"
pink <- "#F75EC1"
dark_pink <- "#EF4A66"
orange <- "#F57E20"
yellow <- "#FFBF00"
light_green <- "#00A75A"
dark_green <- "#00864A"
blue  <- "#2A12B2"


## FONTS ## 

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

#### Step 3: Connect to Database and Pull Data Table of Interest ####

# connect to postgres and pull credentials
source("W:\\RDA Team\\R\\credentials_source.R")
con <- connect_to_db("bold_vision")

data_per_race <- dbGetQuery(con, "SELECT * FROM youth_thriving.response_analysis_per_race") %>%
  mutate(race_labels = if_else(race == 'latinx', 'Latine',
                              if_else(race == 'nh_aian', 'American Indian & Alaska Native',
                                      if_else(race == 'nh_black', 'Black',
                                              ifelse(race == 'nh_white', 'White',
                                                     if_else(race == 'nh_asian', 'Asian',
                                                              if_else(race == 'nh_swana', 'Southwest Asian & North African',
                                                                      if_else(race == 'nh_twoormor', 'Multiracial', 
                                                                              if_else(race == 'nh_nhpi', 'Native Hawaiian & Pacific Islander' ,'NA')))))))))

#### Step 4: Function- GROUPED Horizontal Bar Chart by Subgroup (RACE/ETHNICITY) ####

fx_grouped_barchart <- function(question_number_i, sub_question_i, #these are the inputs, i stands for insert/input of the variable of interest we want to look at
                                response_label_1, response_label_2, response_label_3, #insert the responses we want to capture data for, up to 3
                                title_text, x_axis_text #insert text on graph that is customized
                                ) { 
  
  df_filter <- data_per_race %>% filter(question_number == question_number_i, 
                                        sub_question == sub_question_i, 
                                        response == response_label_1 | response == response_label_2 | response == response_label_3)
  
  
  df_visual <- ggplot(df_filter, aes(x = rate, y = reorder(race_labels, rate), fill = response)) +
    geom_bar(stat = "identity", position = "dodge") + 
    scale_y_discrete(labels = function(race_labels) str_wrap(race_labels, width = 18)) +
    # bar labels
    geom_text(data = df_filter,
              aes(label = paste0(round(rate, digits = 1), "%")),
              size = 3,
              stat="identity", colour = "white",
              position = position_dodge(width = 1), 
              # vjust = 2.25 , 
              hjust= 1.15,
              fontface = "bold", family=font_bar_label) +  
    labs(title = paste(str_wrap(title_text, whitespace_only = TRUE, width = 57), collapse = "\n"),
         x = paste(str_wrap(x_axis_text, whitespace_only = TRUE, width = 95), collapse = "\n"),
         y = "",
         caption= paste(str_wrap(paste0("Question: ", unique(df_filter$question), "\n",
                                        " Sub Question: ", unique(df_filter$sub_question), "\n",
                                        " Category: ", unique(df_filter$response_domain), "\n",
                                        " Data Source: Catalyst California, Bold Vision Youth Thriving Survey, 2024."),
                                 whitespace_only = TRUE, width = 120), collapse = "\n")) +
    #theme/aesthetics
    theme_minimal() +
    theme(legend.title = element_blank(),
          legend.position = "bottom", # no legend title 
          # define style for axis text
          axis.text.y = element_text(size = 9, colour = "black", family= font_axis_label, face = "bold"),
          axis.title.x = element_text(size = 12, colour = "black", family = font_axis_label, face = "bold"),
          # define style for title and caption
          plot.caption = element_text(hjust = 0.0, size = 8, colour = "black", family = font_caption),
          plot.title = element_text(hjust = 0.0, size = 18, colour = "black", family = font_title, face = "bold"),
          # grid line style
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank())  + 
    scale_fill_manual(values = c(pink, dark_pink, orange))
  
  ggsave(plot=df_visual, 
         file=paste0("W:/Project/OSI/Bold Vision/Youth Thriving Survey/Deliverables/", unique(df_filter$response_domain), "/",
                     unique(df_filter$variable), "_question_", unique(df_filter$question_number),"_race_grouped_barchart", ".svg"),
         units = c("in"),  width = 8, height = 5.5)
  
}


#### Step 5: Function- SINGLE Horizontal Bar Chart by Subgroup (RACE/ETHNICITY) ####

fx_single_barchart <- function(question_number_i, sub_question_i, #these are the inputs, i stands for insert/input of the variable of interest we want to look at
                                response_label_1, response_label_2, response_label_3, #insert the responses we want to capture data for, up to 3
                                title_text, x_axis_text #insert text on graph that is customized
) { 
  
  df_filter <- data_per_race %>% filter(question_number == question_number_i, 
                                        sub_question ==  sub_question_i, 
                                        response == response_label_1 | response == response_label_2 | response == response_label_3) %>%
    group_by(race_labels, question, sub_question, response_domain, question_number, variable) %>%
    summarise(total_count = sum(count),
              total_rate = sum(rate))
  
  df_visual <- ggplot(df_filter, aes(x = total_rate, y = reorder(race_labels, total_rate), fill = ifelse(response_domain == 'Caring Families And Relationships', pink, 
                                                                                                         ifelse(response_domain == 'Cultural Identity', dark_pink,
                                                                                                                ifelse(response_domain == 'Demographics', gray,
                                                                                                                       ifelse(response_domain == 'Positive Identity And Self-Worth', yellow, 
                                                                                                                              ifelse(response_domain == 'Racial Justice, Equity, And Inclusion', light_green,
                                                                                                                                     ifelse(response_domain == 'Safety', dark_green,
                                                                                                                                            ifelse(response_domain == 'Strong Minds', blue,
                                                                                                                                                   ifelse(response_domain == 'Vibrant Communities', orange,
                                                                                                                                                          gray)))))))))) +
    geom_bar(stat = "identity", position = "dodge") + 
    scale_y_discrete(labels = function(race_labels) str_wrap(race_labels, width = 18)) +
    # bar labels
    geom_text(data = df_filter,
              aes(label = paste0(round(total_rate, digits = 1), "%")),
              size = 4,
              stat="identity", colour = "white",
              position = position_dodge(width = 1), 
              # vjust = 2.25 , 
              hjust= 1.15,
              fontface = "bold", family=font_bar_label) +  
    labs(title = paste(str_wrap(title_text, whitespace_only = TRUE, width = 57), collapse = "\n"),
         x = paste(str_wrap(x_axis_text, whitespace_only = TRUE, width = 95), collapse = "\n"),
         y = "",
         caption= paste(str_wrap(paste0("Question: ", unique(df_filter$question), "\n",
                                        " Sub Question: ", unique(df_filter$sub_question), "\n",
                                        " Category: ", unique(df_filter$response_domain), "\n",
                                        " Data Source: Catalyst California, Bold Vision Youth Thriving Survey, 2024."),
                                 whitespace_only = TRUE, width = 120), collapse = "\n")) +
    #theme/aesthetics
    theme_minimal() +
    theme(legend.position = "none",
          # define style for axis text
          axis.text.y = element_text(size = 9, colour = "black", family= font_axis_label, face = "bold"),
          axis.title.x = element_text(size = 12, colour = "black", family = font_axis_label, face = "bold"),
          # define style for title and caption
          plot.caption = element_text(hjust = 0.0, size = 8, colour = "black", family = font_caption),
          plot.title = element_text(hjust = 0.0, size = 18, colour = "black", family = font_title, face = "bold"),
          # grid line style
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank())  
  
  ggsave(plot=df_visual, 
         file=paste0("W:/Project/OSI/Bold Vision/Youth Thriving Survey/Deliverables/", unique(df_filter$response_domain), "/",
                     unique(df_filter$variable), "_question_", unique(df_filter$question_number),"_race_single_barchart", ".svg"),
         units = c("in"),  width = 8, height = 5.5)
  
  
}



#### Step 6: Function- STACKED Horizontal Bar Chart by Subgroup (RACE/ETHNICITY) ####

fx_stacked_barchart <- function(question_number_i, sub_question_i, #these are the inputs, i stands for insert/input of the variable of interest we want to look at
                                response_label_1, response_label_2, response_label_3, #insert the responses we want to capture data for, up to 3
                                title_text, x_axis_text #insert text on graph that is customized
) { 
  
  df_filter <- data_per_race %>% filter(question_number == question_number_i, 
                                        sub_question == sub_question_i, 
                                        response == response_label_1 | response == response_label_2 | response == response_label_3)
  
  
  df_visual <- ggplot(df_filter, aes(x = rate, y = reorder(race_labels, rate), fill = response)) +
    geom_bar(stat = "identity") + 
    scale_y_discrete(labels = function(race_labels) str_wrap(race_labels, width = 18)) +
    # bar labels
    geom_text(data = df_filter,
              aes(label = paste0(round(rate, digits = 1), "%")),
              size = 3,
              stat="identity", colour = "white",
              position = position_stack(vjust = 0.5), 
              # hjust= 1.15,
              fontface = "bold", family=font_bar_label) +  
    labs(title = paste(str_wrap(title_text, whitespace_only = TRUE, width = 57), collapse = "\n"),
         x = paste(str_wrap(x_axis_text, whitespace_only = TRUE, width = 95), collapse = "\n"),
         y = "",
         caption= paste(str_wrap(paste0("Question: ", unique(df_filter$question), "\n",
                                        " Sub Question: ", unique(df_filter$sub_question), "\n",
                                        " Category: ", unique(df_filter$response_domain), "\n",
                                        " Data Source: Catalyst California, Bold Vision Youth Thriving Survey, 2024."),
                                 whitespace_only = TRUE, width = 120), collapse = "\n")) +
    #theme/aesthetics
    theme_minimal() +
    theme(legend.title = element_blank(),
          legend.position = "bottom", # no legend title 
          # define style for axis text
          axis.text.y = element_text(size = 9, colour = "black", family= font_axis_label, face = "bold"),
          axis.title.x = element_text(size = 12, colour = "black", family = font_axis_label, face = "bold"),
          # define style for title and caption
          plot.caption = element_text(hjust = 0.0, size = 8, colour = "black", family = font_caption),
          plot.title = element_text(hjust = 0.0, size = 18, colour = "black", family = font_title, face = "bold"),
          # grid line style
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank())  + 
    scale_fill_manual(values = c(pink, dark_pink, orange))
  
  ggsave(plot=df_visual, 
         file=paste0("W:/Project/OSI/Bold Vision/Youth Thriving Survey/Deliverables/", unique(df_filter$response_domain), "/",
                     unique(df_filter$variable), "_question_", unique(df_filter$question_number),"_race_stacked_barchart", ".svg"),
         units = c("in"),  width = 8, height = 5.5)
  
}


#### Step 7: Function- SINGLE Horizontal Bar Chart by Question/FREQUENCIES ####

fx_single_barchart_freq <- function(domain_pgname, #name of domain as it is in the pgadmine table// for example, "strong_minds"
                                    variable_i, #these are the inputs, i stands for insert/input of the variable of interest we want to look at
                               title_text, x_axis_text #insert text on graph that is customized
) { 
  data_freq_ques <- dbGetQuery(con, paste0("SELECT * FROM youth_thriving.", domain_pgname, "_frequencies"))
  
  df_filter <- data_freq_ques %>% filter(variable == variable_i) 
  
  df_visual <- ggplot(df_filter, aes(x = weighted_percent, y = reorder(response, weighted_percent), fill = ifelse(domain == 'Caring Families And Relationships', pink, 
                                                                                                         ifelse(domain == 'Cultural Identity', dark_pink,
                                                                                                                ifelse(domain == 'Demographics', gray,
                                                                                                                       ifelse(domain == 'Positive Identity And Self-Worth', yellow, 
                                                                                                                              ifelse(domain == 'Racial Justice, Equity, And Inclusion', light_green,
                                                                                                                                     ifelse(domain == 'Safety', dark_green,
                                                                                                                                            ifelse(domain == 'Strong Minds', blue,
                                                                                                                                                   ifelse(domain == 'Vibrant Communities', orange,
                                                                                                                                                          gray)))))))))) +
    geom_bar(stat = "identity", position = "dodge") + 
    scale_y_discrete(labels = function(response) str_wrap(response, width = 18)) +
    # bar labels
    geom_text(data = df_filter,
              aes(label = paste0(round(weighted_percent, digits = 1), "%")),
              size = 4,
              stat="identity", colour = "white",
              position = position_dodge(width = 1), 
              vjust = 1.25 ,
              # hjust= 1.15,
              fontface = "bold", family=font_bar_label) +  
    labs(title = paste(str_wrap(title_text, whitespace_only = TRUE, width = 57), collapse = "\n"),
         x = paste(str_wrap(x_axis_text, whitespace_only = TRUE, width = 65), collapse = "\n"),
         y = "",
         caption= paste(str_wrap(paste0("Question: ", unique(df_filter$question), "\n",
                                        " Sub Question: ", unique(df_filter$sub_question), "\n",
                                        " Category: ", unique(df_filter$domain), "\n",
                                        " Data Source: Catalyst California, Bold Vision Youth Thriving Survey, 2024."),
                                 whitespace_only = TRUE, width = 120), collapse = "\n")) +
    #theme/aesthetics
    theme_minimal() +
    theme(legend.position = "none",
          # define style for axis text
          axis.text.y = element_text(size = 9, colour = "black", family= font_axis_label, face = "bold"),
          axis.title.x = element_text(size = 10, colour = "black", family = font_axis_label, face = "bold"),
          # define style for title and caption
          plot.caption = element_text(hjust = 0.0, size = 8, colour = "black", family = font_caption),
          plot.title = element_text(hjust = 0.0, size = 18, colour = "black", family = font_title, face = "bold"),
          # grid line style
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank())  +
    coord_flip()
  
  ggsave(plot=df_visual, 
         file=paste0("W:/Project/OSI/Bold Vision/Youth Thriving Survey/Deliverables/", unique(df_filter$domain), "/",
                     unique(df_filter$variable),"_freq_single_barchart", ".svg"),
         units = c("in"),  width = 8, height = 5.5)
  
  
}



#### Step 8: Running Examples ####

fx_grouped_barchart(question_number_i = '16', sub_question_i = 'At your job', #these are the inputs, i stands for insert/input of the variable of interest we want to look at
                    response_label_1 = 'Sometimes', response_label_2 = 'Most of the time', response_label_3 = 'All of the time', #insert the responses we want to capture data for, up to 3
                    title_text = 'insert title', x_axis_text = 'Rate of youth surveyed reporting being treated unfairly due to their race at their job' #insert text on graph that is customized
                    )


fx_single_barchart(question_number_i = '16', sub_question_i = 'At your job', #these are the inputs, i stands for insert/input of the variable of interest we want to look at
                    response_label_1 = 'Sometimes', response_label_2 = 'Most of the time', response_label_3 = 'All of the time', #insert the responses we want to capture data for, up to 3
                    title_text = 'insert title', x_axis_text = 'Rate of youth surveyed reporting being treated unfairly due to their race at their job' #insert text on graph that is customized
                  )  

fx_stacked_barchart(question_number_i = '16', sub_question_i = 'At your job', #these are the inputs, i stands for insert/input of the variable of interest we want to look at
                    response_label_1 = 'Sometimes', response_label_2 = 'Most of the time', response_label_3 = 'All of the time', #insert the responses we want to capture data for, up to 3
                    title_text = 'insert title', x_axis_text = 'Rate of youth surveyed reporting being treated unfairly due to their race at their job' #insert text on graph that is customized
)
  

fx_single_barchart_freq(domain_pgname = 'racial_justice', #name of domain as it is in the pgadmine table// for example, "strong_minds"
                        variable_i = 'ef', #these are the inputs, i stands for insert/input of the variable of interest we want to look at
                        title_text = 'insert title', x_axis_text = 'Rate of youth surveyed reporting being treated unfairly due to their race at their job' #insert text on graph that is customized
                        )
#### Last Step: Disconnect ####
  dbDisconnect(con)