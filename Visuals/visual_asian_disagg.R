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
source("Visuals//BV_styling.R")

#### Step 3: Downloading dataset ####
source("W:\\RDA Team\\R\\credentials_source.R")
con <- connect_to_db("bold_vision")

df <- dbGetQuery(con, "SELECT * FROM youth_thriving.asian_disagg_co WHERE subgroup_asian NOT IN (
  'Central Asian Alone',
  'Central Asian Aoic',
  'East Asian Aoic',
  'South Asian Aoic',
  'Southeast Asian Aoic') ") %>%
  mutate(subgroup_asian = gsub(" Alone", "", subgroup_asian),  #taking away alone
         subgroup_asian = case_when(
           subgroup_asian=='Multiracial' ~ 'Multiracial Asian', # recoding categories for clarity and better representation
           subgroup_asian=='Multi-Asian' ~ 'Multiethnic Asian', # recoding categories for clarity and better representation
           TRUE ~ subgroup_asian
         ))
  
  order_levels <- df %>%
  filter(response_group == "Often/Always True") %>%
  arrange(desc(rate)) %>%  # ordering
  pull(subgroup_asian)

df <- df %>%
  mutate(subgroup_asian = factor(subgroup_asian, levels = order_levels),
         label = case_when(
           count <= 5 ~ NA, # threshold to leave out any data has a count of 5 or less
           rate_cv > 40 ~ paste0(round(rate, 0), "%*"),
           TRUE ~ paste0(round(rate, 0), "%")
         ),
         rate=case_when(
           count <= 5 ~ NA, # threshold to leave out any data has a count of 5 or less
           TRUE ~ rate))


#### Step 4: Run Visual ####
df_visual <- ggplot(df, aes(x = subgroup_asian, y = rate, fill = response_group )) + 
  geom_bar(stat = "identity", width = 0.8, position = "dodge") + 
  # bar labels
  geom_text(data = df,
            aes(label = label),
            size = 2.75,
            stat="identity", colour = "black",
            position = position_dodge(width = 1), 
            vjust = -0.1 ,
            family=font_bar_label) +  
  labs(title = paste(str_wrap("Multiethnic and East Asian youth are least likely to feel hopeful about their future among all Asian youth", whitespace_only = TRUE, width = 55), collapse = "\n"),
      subtitle = paste("Survey Question: I feel hopeful about my future"),
        x = "",
       y = "",
       fill = "",
       caption= paste("Data Source: Catalyst California's calculations of Bold Vision Youth Thriving Survey,",
       "2024. Groups with fewer than five individuals are omitted for privacy purposes.", 
       "For more information, see the 2025 Bold Vision Youth Thriving Report Methodology.",
                               sep = "\n")) +
  #theme/aesthetics
  theme_minimal() +
  theme(legend.position = "top",  # Show legend on the top/bottom
        # remove axis text
        # axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
         axis.text.y = element_blank(),
        # define style for legend
        legend.text = element_text(size = 12, colour = "black", family = font_subtitle
                                   ),
        legend.title = element_text(size = 12, colour = "black", family = font_subtitle
                                    ),
        legend.margin=margin(-6,0,-2,0), # make legend margins tighter
        legend.box.margin=margin(-6,0,-2,0), # make legend margins tighter
        # define style for title and caption
        plot.caption = element_text(hjust = 0.0, size = 11, colour = "black", family = font_caption, face = "plain"),
        plot.title = element_text(hjust = 0.0, size = 18, colour = "black", family = font_title),
        plot.subtitle = element_text(hjust = 0.0, size = 15, colour = "black", family = font_subtitle, 
                                     margin = margin(b = 23)), #increase space between subtitle and plots because the high ones are getting cut off
        # grid line style
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        #space between facts/small multiple rows
        panel.spacing.y = unit(4, "lines")) + # Increase spacing between 
  scale_fill_manual(values = c(pink, orange)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) #adding padding to avoid labels getting cut off 

# print(df_visual)

ggsave(plot=df_visual, 
       file=paste0("./Visuals/", "/Positive Identity and Self-Worth/",
                   "asian_disagg_co", ".pdf"),
       device = "pdf", units = c("in"),  width = 6, height = 4)
