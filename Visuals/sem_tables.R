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
showtext_auto()


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
  )) %>%
  mutate(std.all=std.all*-1)
         
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
  ) %>%
    mutate(
      y_pos = as.numeric(factor(rhs_label, levels = rev(unique(rhs_label)))) * 2  # multiply by 2 to double spacing
    )

  n_rows <- length(unique(df$rhs_label))
  
  line_positions <- seq(min(df$y_pos) - 1, max(df$y_pos) - 1, by = 2)
  

table <- ggplot(df, aes(y = y_pos)) +
  geom_text(aes(x=.9,label=rhs_label),hjust=0,size=3.9,family= font_axis_label,colour = "black") +
  geom_text(aes(x=.85,label = icon, color = abs(std.all), size=5.2)) +
  geom_hline(yintercept = line_positions, color = gray, size = 0.1) +
  # scale_color_manual(
  #   values = c(
  #     "positive" = pink,
  #     "negative" = light_blue
  #   )
  # ) + 
  scale_color_gradient2(low = "#FA9EDA", mid= "#F75EC1", high = "#943874") +
  # Hide default y-axis labels and gridlines
  scale_x_continuous(limits = c(0.8, 1.5), expand = c(0, 0), breaks = NULL) +
  scale_y_continuous(
    breaks = df$y_pos,
    labels = df$rhs_label,
    expand = expansion(add = 1)
  ) +
  labs(
    title = "Drivers of Psychological Distress", 
    subtitle = paste
    ("Ordered from greatest to least effect",
      sep = "\n"
    )
    ,
    caption = paste("up arrow means an increase in psychological distress", 
                    "down arrow means a decrease in psychological distress",
                    "All effects are statistically significant at the p<0.05 level",
                    sep="\n")
    ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none",
     # define style for title and caption
   plot.caption = element_text(hjust = 0.0, size = 9, colour = "black", family = font_caption),
   plot.subtitle =element_text(hjust = 0.0, size = 12, colour = "black",family = font_subtitle),
   plot.title =element_text(hjust = 0.0, size = 14, colour = "black",family = font_title),
   panel.background = element_rect(fill = "#F5F5F5", color = NA),  # light gray background
   plot.background = element_rect(fill = "white", color = NA),     # keep outer plot white
    )


showtext_opts(dpi=300)

# ggsave(plot=table, 
#        file="./Visuals/Strong Minds/sem_table_Psychological_Distress.png",
#        units = c("in"),  width = 3, height = 2, dpi=300)

ggsave(plot=table,
       file="./Visuals/Strong Minds/sem_table_Psychological_Distress.pdf", device = cairo_pdf,
       units = c("in"),  width = 4, height = 3)
