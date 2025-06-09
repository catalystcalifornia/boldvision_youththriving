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
  'East Asian Aoic',
  'South Asian Alone',
  'Southeast Asian Alone') ")


#### Step 4: Run Visual ####
df_visual <- ggplot(df, aes(x = subgroup_asian, y = rate, fill = response_group )) + 
  geom_bar(stat = "identity", position = "dodge") + 
  # bar labels
  geom_text(data = df,
            aes(label = paste0(round(rate, digits = 1), "%")),
            size = 4,
            stat="identity", colour = "black",
            position = position_dodge(width = 1), 
            vjust = 1.25 ,
            # hjust= 1.15,
            fontface = "bold", 
            family=font_bar_label) +  
  labs(title = paste(str_wrap("title_text", whitespace_only = TRUE, width = 57), collapse = "\n"),
       x = paste(str_wrap("x_axis_text", whitespace_only = TRUE, width = 65), collapse = "\n"),
       y = "",
       caption= paste(str_wrap(paste0("Data Source: Bold Vision Youth Thriving Survey, 2024."),
                               whitespace_only = TRUE, width = 120), collapse = "\n")) +
  #theme/aesthetics
  theme(legend.position = "bottom",  # Show legend on the top/bottom
        # remove axis text
        axis.text.x = element_blank(), 
        # axis.text.y = element_blank(),
        # define style for legend
        legend.text = element_text(size = 14, colour = "black", family = font_subtitle, 
                                   # face = "bold",
                                   margin = margin(t = 5)),
        legend.title = element_text(size = 12, colour = "black", family = font_axis_label, face = "plain", margin = margin(t = 5)),
        # strip.text=element_text(size=12, family=font_axis_label),
        # define style for title and caption
        plot.caption = element_text(hjust = 0.0, size = 11, colour = "black", family = font_caption, face = "plain"),
        plot.title = element_text(hjust = 0.0, size = 18, colour = "black", family = font_title),
        plot.subtitle = element_text(hjust = 0.0, size = 15, colour = "black", family = font_subtitle, 
                                     margin = margin(b = 23)), #increase space between subtitle and plots because the high ones are getting cut off
        # grid line style
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        #space between facts/small multiple rows
        panel.spacing.y = unit(4, "lines")) + # Increase spacing between facet rows 
  scale_fill_manual(values = c(pink, orange)) 

print(df_visual)

ggsave(plot=df_visual, 
       file=paste0("W:/Project/OSI/Bold Vision/Youth Thriving Survey/Deliverables/", unique(df_filter$domain), "/",
                   unique(df_filter$variable),"_freq_single_barchart", ".svg"),
       units = c("in"),  width = 8, height = 5.5)
