# The following file creates visuals for the demographic data of the BV Youth Thriving Survey

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

df_dem_race <- dbGetQuery(con, "SELECT * FROM youth_thriving.demographics_nh_race") %>%
  mutate(race_labels = if_else(race == 'latinx', 'Latine',
                               if_else(race == 'nh_aian', 'American Indian & Alaska Native',
                                       if_else(race == 'nh_black', 'Black',
                                               ifelse(race == 'nh_white', 'White',
                                                      if_else(race == 'nh_asian', 'Asian',
                                                              if_else(race == 'nh_swana', 'Southwest Asian & North African',
                                                                      if_else(race == 'nh_twoormor', 'Multiracial', 
                                                                              if_else(race == 'nh_nhpi', 'Native Hawaiian & Pacific Islander' ,
                                                                                      if_else(race == 'nh_other', 'Other', 'NA')))))))))) %>%
  filter(race_labels != 'NA')




viz_dem_race <- ggplot(df_dem_race, aes(x = rate, y = reorder(race_labels, rate))) +
  geom_bar(stat = "identity", position = "dodge", fill = yellow) + 
  scale_y_discrete(labels = function(race_labels) str_wrap(race_labels, width = 18)) +
  # bar labels
  geom_text(data = df_dem_race,
            aes(label = paste0(round(rate, digits = 1), "%")),
            size = 4,
            stat="identity", colour = "black",
            position = position_dodge(width = 1), 
            # vjust = 2.25 , 
            hjust= 0.05,
            fontface = "bold", family=font_bar_label) +  
  labs(title = paste(str_wrap("Youth Thriving Survey Participants' Race Groups", whitespace_only = TRUE, width = 57), collapse = "\n"),
       x = paste(str_wrap("", whitespace_only = TRUE, width = 95), collapse = "\n"),
       y = "",
       caption= paste(str_wrap(paste0("Data Source: Bold Vision Youth Thriving Survey, 2024."),
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
# print(viz_dem_race)

ggsave(plot=viz_dem_race, 
       file=paste0("W:/Project/OSI/Bold Vision/Youth Thriving Survey/Deliverables/", "Demographics", "/",
                   "race_barchart", ".svg"),
       units = c("in"),  width = 10, height = 5.5)


df_dem_asian_sub <- dbGetQuery(con, "SELECT * FROM youth_thriving.demographics_asian_ethnicity") %>%
  filter(rate > 1)  %>% filter(detailed_asian != 'NA')


viz_dem_asian <- ggplot(df_dem_asian_sub, aes(x = rate, y = reorder(detailed_asian, rate), width = 0.8)) +
  geom_bar(stat = "identity", position = "dodge", fill = blue) + 
  scale_y_discrete(labels = function(detailed_asian) str_wrap(detailed_asian, width = 30)) +
  # bar labels
  geom_text(data = df_dem_asian_sub,
            aes(label = paste0(round(rate, digits = 1), "%")),
            size = 3,
            stat="identity", colour = "black",
            position = position_dodge(width = 1), 
            # vjust = 2.25 , 
            hjust= -0.01,
            fontface = "bold", family=font_bar_label) +  
  labs(title = paste(str_wrap("Youth Thriving Survey Asian Participants' Subgroups", whitespace_only = TRUE, width = 57), collapse = "\n"),
       x = paste(str_wrap("", whitespace_only = TRUE, width = 95), collapse = "\n"),
       y = "",
       caption= paste(str_wrap(paste0("Data Source: Bold Vision Youth Thriving Survey, 2024."),
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
 # print(viz_dem_asian)

ggsave(plot=viz_dem_asian, 
       file=paste0("W:/Project/OSI/Bold Vision/Youth Thriving Survey/Deliverables/", "Demographics", "/",
                   "race_asian_sub_barchart", ".svg"),
       units = c("in"),  width = 10, height = 5.5)

dbDisconnect(con)