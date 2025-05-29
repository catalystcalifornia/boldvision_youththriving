# Create simple tables of sem results

# Step 0: Setting up work space ------
library(dplyr)
library(ggplot2)
library(tidyverse)
library(stringr)
library(extrafont)
library(showtext)
library(ggtext)
library(RPostgres)
library(fontawesome)
library(gt)
library(gtExtras)
library(emojifont)
font_import()

# connect to postgres and source functions
source("W:\\RDA Team\\R\\credentials_source.R")

con <- connect_to_db("bold_vision")

# Load BV styling, colors and fonts
source('Visuals\\BV_styling.R')

# Step 1: Prepare data ----
# load table
sem_df <- dbGetQuery(con, "SELECT * FROM youth_thriving.factor_analysis_sem_results_weighted 
                       where model_name='2f.6_weighted_fiml' or model_name='1j.5_weighted_fiml'")

sem_sig_df <- sem_df %>% filter(p_is_significant)


# Step 2: Table for psychological distress ----
extrafont::font_import (path="W:\\Project\\OSI\\Bold Vision\\Youth Thriving Survey\\Fonts", pattern = "fa-", prompt =  FALSE)
loadfonts(device = "win") 

font_add(family = "FontAwesome5Free-Solid", regular = "W:\\Project\\OSI\\Bold Vision\\Youth Thriving Survey\\Fonts\\fa-solid-900.ttf")

font_add("fa", "W:\\Project\\OSI\\Bold Vision\\Youth Thriving Survey\\Fonts\\fontawesome-free-6.7.2-web\\fontawesome-free-6.7.2-web.ttf")
showtext_auto()

test <- "C:\\Users\\EGraves\\Downloads\\arrow-circle-down.svg"


  
# Prepare the data with arrow and color
df <- sem_sig_df %>%
  filter(model_name=='1j.5_weighted_fiml')%>%
  select(rhs,std.all) %>%
  mutate(
    icon = ifelse(std.all >= 0, "\u25B2", "\u25BC"),
  )

ggplot(df, aes(y = reorder(rhs, abs(std.all)))) +
  geom_text(aes(x=1,label=rhs),hjust=0,size=12,family= font_axis_label) +
  geom_text(aes(x=3,label = icon, color = abs(std.all), size=18)) +
  scale_color_gradient(low = "#FDDFF3", high = "#F75EC1") +
  scale_size(range = c(5, 10)) +
  # Hide default y-axis labels and gridlines
  scale_x_continuous(limits = c(0.8, 3.2), breaks = NULL) +
  labs(
    title = "Average Predicted <span style ='color: #F75EC1;'>Freedom from Psychological <br>Distress</span>", 
    subtitle = paste
    ("\nLA County youth vary in how they are thriving emotionally. LGBTQIA+,",
      "unhoused, undocumented, and systems impacted youth experience",
      "the most differences compared to their counterparts.",
      sep = "\n"
    ),
    caption = paste("\nCatalyst California's calculations of Bold Vision Youth Thriving Survey, 2024.",
                    "Note: AIAN=American Indian & Alaska Native; BIPOC=Black, Indigeneous, People of Color;", 
                    "LGBTQIA+=Lesbian, Gay, Bisexual, Transgender, Queer, Intersex, Asexual, & Gender", 
                    "Nonconforming; NHPI: Native Hawaiian & Pacific Islander; SWANA=Southwest Asian & North",
                    "African; Systems Impacted=Youth at any point in foster care, juvenile hall/probation camp",
                    "jail/prison, group home/residential program, or lived with legal guardians.",
                    sep="\n")) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none",
    legend.title = element_text(hjust = 0.5,size = 12, family= font_axis_label),
                                                                         legend.text = element_text(hjust = 0.5,size = 12, family= font_axis_label),
                                                                         legend.position = "bottom", # no legend title
                                                                         legend.margin=margin(l = 0),
                                                                         # legend.margin=margin(-2,-2,-2,-2),
                                                                         # legend.box.margin=margin(-2,-2,-2,-2),
                                                                         # define style for axis text
                                                                         axis.text.y=element_blank(),
                                                                         # axis.text.y = element_text(size = 9, colour = "black", family= font_axis_label, face = "bold"),
                                                                         axis.text.x=element_blank(),
                                                                         axis.ticks=element_blank(),
                                                                         axis.ticks.length = unit(0, "pt"),
                                                                         # axis.text.x=element_text(size = 11, colour = "black", family = font_axis_label),
                                                                         axis.title.x=element_blank(),
                                                                         # axis.title.x = element_text(size = 12, colour = "black", family = font_axis_label, face = "bold"),
                                                                         # define style for title and caption
                                                                         plot.caption = element_text(hjust = 0.0, size = 10, colour = "black", family = font_caption),
                                                                         plot.subtitle = 
                                                                           element_text(hjust = 0.0, size = 14, family = font_subtitle), 
                                                                         plot.title = 
                                                                           element_markdown(hjust = 0.0, size = 20, family = font_title)
                                                                         # ,
                                                                         #   element_text(hjust = 0.0, size = 20, colour = "black", family = font_title)
                                                                         , 
                                                                         # grid line style
                                                                         panel.border=element_blank(),
                                                                         panel.grid = element_blank())