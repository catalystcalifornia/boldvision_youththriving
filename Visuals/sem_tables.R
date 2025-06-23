# Create simple tables of sem results for significant variables only

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
# load table and filter for final models
sem_df <- dbGetQuery(con, "SELECT * FROM youth_thriving.factor_analysis_sem_results_weighted 
                       where model_name='2f.6_weighted_fiml' or model_name='1j.5_weighted_fiml'")

# clean up names of components and demographics
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
    str_detect(rhs_label, "Microaggressions") ~ "Microaggressions",  
    str_detect(rhs_label, "Structural Racism") ~ "Structural Racism", 
    str_detect(rhs_label, "Caring Families") ~ "Caring Families and Relationships", 
    str_detect(rhs_label, "Opportunities For") ~ "Opportunities for Community Involvement", 
    TRUE ~ rhs_label
  )) %>%
  filter(rhs_label!='Other Racial Identity') # we are dropping other racial identity because this is based on less than 10 respondents
         
# check
  sem_sig_df$rhs_label         
         


# Step 2: Table for psychological distress ----
 # filter for psyc distress model
   df <- sem_sig_df %>%
    filter(model_name=='1j.5_weighted_fiml')
  
   # reverse some coefficients so it's easier to understand given we made all COMPONENT questions in the same direction in the model - higher score equals better
  # we need to reverse the positive components right now higher=higher freedom from psyc distress so once we make psyc distress negative (higher psyc distress), these components need to be reversed
  reverse_c <- c("Caring Families and Relationships", "Feelings of Personal Safety","Sparks","Cultural Identity")
   # we'll also want to reverse all demographic coefficients since we didn't reverse those in the models
  reverse_d <- c("LGBTQIA+ Identity", "Asian Racial Identity","Systems Impacted","AIAN Racial Identity")
  
  
# Reverse coefficients and prepare the data with arrow
df <- df %>%
  mutate(std=ifelse(rhs_label %in% reverse_c | rhs_label %in% reverse_d, std.all*-1, std.all), # reverse
    icon = ifelse(std >= 0, "+", "\u2212"), # add + or -, - code is for nicer looking minus sign
    direction = ifelse(std >= 0, "increase", "decrease") # indicate increase or decrease
  ) %>%
    mutate(
      y_pos = as.numeric(factor(rhs_label, levels = rev(unique(rhs_label)))) * 2.2  # multiply by 2 to have double spacing in the table
    )

df <- df %>% select(rhs_label, std, icon, direction, y_pos) # reduce columns

  # n_rows <- length(unique(df$rhs_label)) # step to add lines between rows
  
  line_positions <- seq(min(df$y_pos) - 1, max(df$y_pos) + 2.2, by = 2.2) # line positions with the double spacing
  
  header_y <- max(df$y_pos) + 2.2  # push it above the top row # add header row
  
table <- ggplot(df, aes(y = y_pos)) + # rows with double spacing
  geom_text(aes(x=.95,label=rhs_label),hjust=0,size=3.9,family= font_axis_label,colour = "black") + # coefficient label
  geom_text(aes(x = 0.85, label = icon), # outline color for arrow
            color = "grey22",  
            size = 5.2 + 0.2  # slightly bigger than top layer
           ) +
  geom_text(aes(x=.85,label = icon, color = abs(std), size=5.2)) + # arrow and then color based on absolute value
  geom_hline(yintercept = line_positions, color = gray, size = 0.1) + # lines between rows
  # scale_color_manual(
  #   values = c(
  #     "positive" = pink,
  #     "negative" = light_blue
  #   )
  # ) + 
  scale_color_gradient2(low = "#FDE1F3", high = "#F75EC1") + # color gradient for as coefficient goes from weaker to stronger effect (based on absolute value)
  scale_x_continuous(limits = c(0.8, 1.5), expand = c(0, 0), breaks = NULL) + # x limits
  scale_y_continuous(
    breaks = df$y_pos,
    labels = df$rhs_label,
    expand = expansion(add = 1)
  ) +
  labs(
    title = "Psychological Distress Predictors", 
    subtitle = "Ordered from <span style ='color: #F75EC1;'>largest</span> to <span style ='color: #FFD7EE;'>smallest </span>effect",
    caption = paste("+ means an increase in psychological distress",
                    "- means a decrease in psychological distress",
                    "All effects are statistically significant at the p<0.05 level. For more information", 
                    "on the methodology, please refer to the 2025 Bold Vision Youth Thriving report.",
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
   plot.subtitle =element_markdown(hjust = 0.0, size = 12, colour = "black",family = font_subtitle),
   plot.title =element_text(hjust = 0.0, size = 14, colour = "black",family = font_title),
   panel.background = element_rect(fill = "#F5F5F5", color = NA),  # light gray background
   plot.background = element_rect(fill = "white", color = NA),     # keep outer plot white
    ) +
  # header row
  annotate("text", x = 0.85, y = header_y, label = "Effect", hjust = 0.4, 
           family = font_axis_label, size = 4, fontface = "bold") +
  annotate("text", x = 0.95, y = header_y, label = "Predictor", hjust = 0,
           family = font_axis_label, size = 4, fontface = "bold")


showtext_opts(dpi=300)

# ggsave(plot=table,
#        file="./Visuals/Strong Minds/sem_table_Psychological_Distress.png",
#        units = c("in"),  width = 4, height = 3, dpi=300)
# 
# ggsave(plot=table,
#        file="./Visuals/Strong Minds/sem_table_Psychological_Distress.pdf", device = cairo_pdf,
#        units = c("in"),  width = 4, height = 3)



# Step 2: Table for self-efficacy and hope ----
# filter for self-efficacy and hope model
df <- sem_sig_df %>%
  filter(model_name=='2f.6_weighted_fiml')

# because self-efficacy and hope wasn't reversed in the final model (since higher was already better) no need to reverse coefficients


# Prepare the data with arrow
df <- df %>%
  mutate(std=std.all, 
         icon = ifelse(std >= 0, "\u25B2", "\u25BC"), # add arrow
         direction = ifelse(std >= 0, "increase", "decrease") # indicate increase or decrease
  ) %>%
  mutate(
    y_pos = as.numeric(factor(rhs_label, levels = rev(unique(rhs_label)))) * 2.2  # multiply by 2 to have double spacing in the table
  )

df <- df %>% select(rhs_label, std, icon, direction, y_pos) # reduce columns

n_rows <- length(unique(df$rhs_label)) # step to add lines between rows

line_positions <- seq(min(df$y_pos) - 1, max(df$y_pos) + 2.2, by = 2.2) # line positions with the double spacing

header_y <- max(df$y_pos) + 2.2  # push it above the top row # add header row

table <- ggplot(df, aes(y = y_pos)) + # rows with double spacing
  geom_text(aes(x=.95,label=rhs_label),hjust=0,size=3.9,family= font_axis_label,colour = "black") + # coefficient label
  geom_text(aes(x = 0.85, label = icon), # outline color for arrow
            color = "grey22",  
            size = 5.2 + 0.2  # slightly bigger than top layer
  ) +
  geom_text(aes(x=.85,label = icon, color = abs(std), size=5.2)) + # arrow and then color based on absolute value
  geom_hline(yintercept = line_positions, color = gray, size = 0.1) + # lines between rows
  # scale_color_manual(
  #   values = c(
  #     "positive" = pink,
  #     "negative" = light_blue
  #   )
  # ) + 
  scale_color_gradient2(low = "#FDD8DD", high = "#EF4A66") + # color gradient for as coefficient goes from weaker to stronger effect (based on absolute value)
  scale_x_continuous(limits = c(0.8, 1.5), expand = c(0, 0), breaks = NULL) + # x limits
  scale_y_continuous(
    breaks = df$y_pos,
    labels = df$rhs_label,
    expand = expansion(add = 1)
  ) +
  labs(
    title = "Self-Efficacy And Hope Predictors", 
    subtitle = "Ordered from <span style ='color: #EF4A66;'>greatest</span> to <span style ='color: #FDD8DD;'>lowest </span>effect",
    caption = paste("up arrow means an increase in self-efficacy and hope",
                    "down arrow means a decrease in self-efficacy and hope",
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
    plot.subtitle =element_markdown(hjust = 0.0, size = 12, colour = "black",family = font_subtitle),
    plot.title =element_text(hjust = 0.0, size = 14, colour = "black",family = font_title),
    panel.background = element_rect(fill = "#F5F5F5", color = NA),  # light gray background
    plot.background = element_rect(fill = "white", color = NA),     # keep outer plot white
  ) +
  # header row
  annotate("text", x = 0.85, y = header_y, label = "Effect", hjust = 0.4, 
           family = font_axis_label, size = 4, fontface = "bold") +
  annotate("text", x = 0.95, y = header_y, label = "Predictor", hjust = 0,
           family = font_axis_label, size = 4, fontface = "bold")


showtext_opts(dpi=300)

ggsave(plot=table,
       file="./Visuals/Positive Identity and Self-Worth/sem_table_Self_Efficacy_And_Hope.png",
       units = c("in"),  width = 4, height = 3, dpi=300)

ggsave(plot=table,
       file="./Visuals/Positive Identity and Self-Worth/sem_table_Self_Efficacy_And_Hope.pdf", device = cairo_pdf,
       units = c("in"),  width = 4, height = 3)

