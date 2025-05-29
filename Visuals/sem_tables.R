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
library(gt)
library(gtExtras)


# connect to postgres and source functions
source("W:\\RDA Team\\R\\credentials_source.R")

con <- connect_to_db("bold_vision")

# Load BV styling, colors and fonts
source('Visuals\\BV_styling.R')

# Step 1: Prepare data ----
# load table
sem_df <- dbGetQuery(con, "SELECT * FROM youth_thriving.factor_analysis_sem_results_weighted 
                       where model_name='2f.6_weighted_fiml' or model_name='1j.5_weighted_fiml'")

sem_sig_df <- sem_df %>% filter(p_is_significant) %>%
  mutate(rhs_label=rhs) %>%
  mutate(rhs_label=rhs_label %>% str_replace_all("^component_", ""),
         rhs_label=rhs_label %>% str_replace_all("^subcomponent_", ""),
         rhs_label=rhs_label %>% str_replace_all("_re$", ""),
         rhs_label=rhs_label %>% str_replace_all("nh_", ""),
         rhs_label=rhs_label %>% str_replace_all("_", " "),
         rhs_label=str_to_title(rhs_label)) %>%
  mutate(rhs_label = 
  case_when(
    str_detect(rhs_label, "Cishet Lgbtqia") ~ "LGBTQIA+ Identity", 
    str_detect(rhs_label, "Personal Safety") ~ "Feelings of Personal Safety",
    str_detect(rhs_label, "Race Other") ~ "Other Racial Identity",
    str_detect(rhs_label, "Race Aian") ~ "AIAN Racial Identity",
    str_detect(rhs_label, "Race Asian") ~ "Asian Racial Identity",
    str_detect(rhs_label, "Microaggressions") ~ "Freedom From Microaggressions",  
    str_detect(rhs_label, "Structural Racism") ~ "Freedom From Structural Racism", 
    TRUE ~ rhs_label
  ))
         
# check
  sem_sig_df$rhs_label         
         


# Step 2: Table for psychological distress ----

# Prepare the data with arrow and color
df <- sem_sig_df %>%
  filter(model_name=='1j.5_weighted_fiml')%>%
  select(rhs_label,std.all)%>%
  mutate(
    icon = ifelse(std.all >= 0, "\u25B2", "\u25BC"),
    direction = ifelse(std.all >= 0, "positive", "negative")
  )

ggplot(df, aes(y = reorder(rhs_label, abs(std.all)))) +
  geom_text(aes(x=1,label=rhs_label),hjust=0,size=12,family= font_axis_label) +
  geom_text(aes(x=3,label = icon, color = abs(std.all), size=50)) +
  # scale_color_manual(
  #   values = c(
  #     "positive" = pink,
  #     "negative" = light_blue
  #   )
  # ) + 
  scale_color_gradient(low = "#FDDFF3", high = "#F75EC1") +
  # Hide default y-axis labels and gridlines
  scale_x_continuous(limits = c(0.8, 3.2), breaks = NULL) +
  labs(
    title = "Drivers of Freedom of Psychological Distress", 
    subtitle = paste
    ("Ordered from greatest to least effect",
      "\u25B2 indicates a positive effect",
      "\u25BC indicates a negative effect",
      sep = "\n"
    )
    # ,
    # caption = paste("\nCatalyst California's calculations of Bold Vision Youth Thriving Survey, 2024.",
    #                 "Note: AIAN=American Indian & Alaska Native; BIPOC=Black, Indigenous, People of Color;", 
    #                 "LGBTQIA+=Lesbian, Gay, Bisexual, Transgender, Queer, Intersex, Asexual, & Gender", 
    #                 "Nonconforming; NHPI: Native Hawaiian & Pacific Islander; SWANA=Southwest Asian & North",
    #                 "African; Systems Impacted=Youth at any point in foster care, juvenile hall/probation camp",
    #                 "jail/prison, group home/residential program, or lived with legal guardians.",
    #                 sep="\n")
    ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none",
    # legend.title = element_text(hjust = 0.5,size = 12, family= font_axis_label),
    #                                                                      legend.text = element_text(hjust = 0.5,size = 12, family= font_axis_label),
    #                                                                      legend.position = "bottom", # no legend title
    #                                                                      legend.margin=margin(l = 0),
    #                                                                      # legend.margin=margin(-2,-2,-2,-2),
    #                                                                      # legend.box.margin=margin(-2,-2,-2,-2),
    #                                                                      # define style for axis text
    #                                                                      axis.text.y=element_blank(),
    #                                                                      # axis.text.y = element_text(size = 9, colour = "black", family= font_axis_label, face = "bold"),
    #                                                                      axis.text.x=element_blank(),
    #                                                                      axis.ticks=element_blank(),
    #                                                                      axis.ticks.length = unit(0, "pt"),
    #                                                                      # axis.text.x=element_text(size = 11, colour = "black", family = font_axis_label),
    #                                                                      axis.title.x=element_blank(),
    #                                                                      # axis.title.x = element_text(size = 12, colour = "black", family = font_axis_label, face = "bold"),
    #                                                                      # define style for title and caption
    #                                                                      plot.caption = element_text(hjust = 0.0, size = 10, colour = "black", family = font_caption),
                                                                         plot.subtitle =
                                                                           element_text(hjust = 0.0, size = 15, family = font_subtitle),
                                                                         plot.title =
                                                                           element_text(hjust = 0.0, size = 18, family = font_title)
    #                                                                      # ,
    #                                                                      #   element_text(hjust = 0.0, size = 20, colour = "black", family = font_title)
    #                                                                      , 
    #                                                                      # grid line style
    #                                                                      panel.border=element_blank(),
    #                                                                      panel.grid = element_blank()
    
    )