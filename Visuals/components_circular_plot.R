# Create visuals of average component scores/subcomponent scores for all youth and by demographics

# Step 0: Setting up work space ------
library(dplyr)
library(ggplot2)
library(tidyverse)
library(stringr)
library(extrafont)
library(showtext)
library(ggtext)
library(ggchicklet)
library(RPostgres)
library(RPostgreSQL)
library(patchwork)

# connect to postgres and source functions
source("W:\\RDA Team\\R\\credentials_source.R")

con <- connect_to_db("bold_vision")

# pulling in data for each demographic
df_total <- dbGetQuery(con, "SELECT * 
                       FROM youth_thriving.factor_analysis_avg_scores_total
                       ")


df_race <- dbGetQuery(con, "SELECT * 
                       FROM youth_thriving.factor_analysis_avg_scores_race
                       ")

df_sogi <- dbGetQuery(con, "SELECT * 
                       FROM youth_thriving.factor_analysis_avg_scores_sogi
                      ")

df_age <- dbGetQuery(con, "SELECT * 
                       FROM youth_thriving.factor_analysis_avg_scores_age
                      ")

df_spa <- dbGetQuery(con, "SELECT * 
                       FROM youth_thriving.factor_analysis_avg_scores_spa
                       ")

df_systems <- dbGetQuery(con, "SELECT * 
                       FROM youth_thriving.factor_analysis_avg_scores_systems_involved
                       ")

# Step 1: Prep dataframe -----
# join together all dataframes and remove any "not" categories
df_all <- rbind(df_total,
                df_age,
                df_race %>% filter(!subgroup %in% c("nh_aian","nh_nhpi","nh_swana")), # remove NH categories that we want to use aoic for
                df_systems,
                df_sogi) %>%
  filter(!str_detect(subgroup,"not ")) 

table(df_all$subgroup, useNA='always')

# clean up naming
df_all <- df_all %>%
  mutate(
         youth_label = subgroup %>% str_replace_all("^nh_", ""),
         youth_label = 
           case_when(
             str_detect(youth_label, "LGBTQIA+") ~ "LGBTQIA+", # so it doesn't get uncapitalized later
             str_detect(youth_label, "bipoc") ~ str_to_upper(youth_label),
             str_detect(youth_label, "swana") ~ str_to_upper(youth_label),
             str_detect(youth_label, "aian") ~ str_to_upper(youth_label),
             str_detect(youth_label, "nhpi") ~ str_to_upper(youth_label),
             str_detect(youth_label, "twoormor") ~ "Multiracial",  # Rename "Twoormor" to "Multiracial"
             str_detect(youth_label, "latinx") ~ "Latine",  # Rename latinx to Latine
             str_detect(youth_label, "other") ~ "Another Race",  # Rename other to Another Race
             str_detect(youth_label, "systems_impacted") ~ "Systems Impacted", 
             str_detect(youth_label, "Cisgender Male") ~ "Cis Man/Boy", 
             str_detect(youth_label, "Cisgender Female") ~ "Cis Woman/Girl", 
             str_detect(youth_label, "undocumented") ~ "Immigrant", 
             TRUE ~ str_to_title(youth_label)  # Capitalize first letter of each word otherwise
           ))

table(df_all$youth_label, useNA='always')

# adjust minimum across data frames so axis starts at 0
min(df_all$avg)
max(df_all$avg)

df_all <- df_all %>%
  group_by(component_model) %>%
  mutate(min=min(avg),
    avg_adjusted = avg+abs(min(avg))+.0015)

min(df_all$avg_adjusted)
max(df_all$avg_adjusted)

# create labels for components
## data dictionary for component labels
component_labels <- select(df_total, component_model) %>%
  mutate(component_label=gsub(component_model,pattern="component_",replacement=""),
         component_label=gsub(component_label,pattern="sub",replacement=""),
         component_label=gsub(component_label,pattern="_", replacement=" "),
         component_label=gsub(component_label,pattern="experiences of racism and ", replacement=""),
         component_label=str_to_title(component_label))%>%
  mutate(component_label=ifelse(component_label=="Self Efficacy Hope", "Self-Efficacy And Hope",
                                       component_label))


 ## check
component_labels$component_label

# join labels
df_all <- df_all %>%
  left_join(component_labels)


# Step 2: Setting Bold Vision Style Guide ----

## Colors

gray <- "#D6D7D6"
pink <- "#F75EC1"
dark_pink <- "#EF4A66"
orange <- "#F57E20"
yellow <- "#FFBF00"
light_green <- "#00A75A"
dark_green <- "#00864A"
blue  <- "#2A12B2"
light_blue <- "#465adc"
dark_blue <-'#220f8c'

# https://colorampgen.vercel.app/

## FONTS ## 
font_add(family = "Manifold Regular", regular = "W:/Project/OSI/Bold Vision/BV 2021/Deliverables/Bold Vision Fonts/Manifold/Fonts/manifoldcf-regular.otf")
font_add(family = "Manifold CF", regular = "W:/Project/OSI/Bold Vision/BV 2021/Deliverables/Bold Vision Fonts/Manifold/Fonts/manifoldcf-heavy.otf")
font_add(family = "HelveticaNeueLTStdMdCn", regular = "W:/Project/OSI/Bold Vision/BV 2021/Deliverables/Bold Vision Fonts/Helvetica Neue LT Std/HelveticaNeueLTStd-MdCn.otf")
font_add(family = "HelveticaNeueLTStdHvCn", regular = "W:/Project/OSI/Bold Vision/BV 2021/Deliverables/Bold Vision Fonts/Helvetica Neue LT Std/HelveticaNeueLTStd-HvCn.otf")
font_add(family = "HelveticaNeueLTStdMdCnO", regular = "W:/Project/OSI/Bold Vision/BV 2021/Deliverables/Bold Vision Fonts/Helvetica Neue LT Std/HelveticaNeueLTStd-MdCnO.otf")
font_add(family = "HelveticaNeueLTStdMd", regular = "W:/Project/OSI/Bold Vision/BV 2021/Deliverables/Bold Vision Fonts/Helvetica Neue LT Std/HelveticaNeueLTStd-Md.otf")


# font_import()
loadfonts(device = "win")
windowsFonts()
showtext_auto()


# define fonts in chart
font_title <- "HelveticaNeueLTStdHvCn"
font_subtitle <- "HelveticaNeueLTStdMdCn"
font_caption <- "Manifold Regular"
font_bar_label <- "Manifold Regular"
font_axis_label <- "Manifold Regular"



# Step 3: Filter for selected components and for selected demographics -----
# list of demographics to focus on
unique(df_all$youth_label)
subgroups<-c("AIAN","All Youth","Asian","Black","Latine","Multiracial","NHPI","SWANA","White","BIPOC","Systems Impacted","Immigrant","Unhoused","Cis Man/Boy","Cis Woman/Girl","LGBTQIA+")
subgroups

# list of components to focus on
component_labels$component_label
components<-c("Psychological Distress","Self-Efficacy And Hope","Microaggressions","Caring Families And Relationships","Cultural Identity","Structural Racism","Vibrant Communities")
components

# filter dataframe
df_all <- df_all %>%
  filter(youth_label %in% subgroups & # filter for subgroups
         component_label %in% components) # filter for components
  
  
# Step 4: Run circular bar plot just by one component to test - PSYCHOLOGICAL DISTRESS-------
# filter for the component
df <- df_all %>% filter(component_label=='Psychological Distress')

# factor labels for ordering subgroups
df$youth_label_factor <- factor(df$youth_label, levels = subgroups)

df <- df %>%
  arrange(avg_adjusted)


# ----- This section prepare a dataframe for labels ---- #
# add id
df$id<- seq(1, nrow(df))

# Get the name and the y position of each label
label_data <- df

# calculate the ANGLE of the labels
number_of_bar <- nrow(df)
angle <-  90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)

# calculate the alignment of labels: right or left
# If I am on the left part of the plot, my labels have currently an angle < -90
label_data$hjust<-ifelse( angle < -90, 1, 0)

# flip angle BY to make them readable
label_data$angle<-ifelse(angle < -90, angle+180, angle)
# ----- ------------------------------------------- ---- #

# ----- #
# Title work separately to fix alignment
# Define the textual header using ggplot + ggtext::element_markdown
title_block <- ggplot() +
  theme_void() +
  labs(
    title = "Average Predicted <span style ='color: #F75EC1;'>Psychological Distress</span>",
    subtitle = paste(
      "L.A. County youth are not all thriving equally. LGBTQIA+, unhoused, immigrant, and",
      "systems impacted youth are experiencing more psychological distress than other youth.",
      sep = "\n"
    )
    ) +
  theme(
    plot.title = element_markdown(hjust = 0, size = 18, family = font_title),
    plot.subtitle = element_text(hjust = 0, size = 13, family = font_subtitle),
    plot.margin = margin(t = 0, r = 0, b = -4, l = 0)
  )

caption_block <- ggplot() +
  theme_void() +
  labs(
       caption = paste(
      "\nCatalyst California's calculations of Bold Vision Youth Thriving Survey, 2024. Note: AIAN=American Indian",
      "& Alaska Native; NHPI: Native Hawaiian & Pacific Islander; SWANA=Southwest Asian & North African. For",
      "more information on each group's definition, please refer to the 2025 Bold Vision Youth Thriving report.",
      sep = "\n"
    )
  ) +
  theme(
      plot.caption = element_text(hjust = 0, size = 11, family = font_caption),
    plot.margin = margin(t = -4, r = 0, b = 0, l = 0)
  )


p <- ggplot(df, aes(x=as.factor(id), y=avg_adjusted, group=component_label)) +
  geom_bar(aes(fill=avg_adjusted),stat = "identity", 
           alpha=1, show.legend=TRUE) +  
  scale_fill_gradientn("Psychological Distress",
  colours=
    # c("#FDDFF3","#FA9EDA","#F97ECD","#F75EC1") 
  c("#FDE1F3", "#FCAEDC", "#F979CA", "#F75EC1")
  ,
  labels=c("<- Lower","","Higher ->")
  )+
  # Make the guide for the fill discrete
  guides(
    fill = guide_colorsteps(title.position = "top", title.hjust = .5
    )
  ) +
  scale_x_discrete(expand = c(0, 0)) +
  ylim(-.25,1.1) +
  ylab("")+
  xlab("")+
   theme_void() +
  theme(aspect.ratio=1,
        legend.title = element_text(hjust = 0.5,size = 11, family= font_axis_label),
        legend.text = element_text(hjust = 0.5,size = 11, family= font_axis_label),
        legend.position = "bottom", # no legend title
        legend.margin=margin(0,0,-5,0),
        legend.box.margin=margin(-8,0,-8,0),
        legend.key.height = unit(0.2, "cm"),
        legend.key.width = unit(1.5, "cm"),
        axis.text.y=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks=element_blank(),
        axis.ticks.length = unit(0, "pt"),
        axis.title.x=element_blank(),
         # grid line style
        panel.border=element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(c(-.6,-.1,-.6,-.1),"cm")
        ) + 
  coord_polar(clip="off") +
  # Add the labels, using the label_data dataframe that we have created before
  geom_text(data=label_data, aes(x=id, y=avg_adjusted+.006, label=youth_label, hjust=hjust), color="black", family=font_axis_label,alpha=0.6, size=3.4, angle= label_data$angle, inherit.aes = FALSE ) 

final_plot <- title_block / p  / caption_block + plot_layout(heights=c(.06,.93,.01))

showtext_opts(dpi=300)

ggsave(plot=final_plot, 
       file="./Visuals/Strong Minds/circular_plot_Psychological_Distress.png",
       units = c("in"),  width = 7, height = 6)

ggsave(plot=final_plot, 
       file="./Visuals/Strong Minds/circular_plot_Psychological_Distress.pdf",
       units = c("in"),  width = 7, height = 6)


# Step 5: Make a function for circular bar plot -------

circular_plot <- function(component_input,component_folder,component_colors,legend_labels, title_text,subtitle_text) {
  
  # ----- This section prepares the data for the visual ---- #
  # filter for the component
  df <- df_all %>% filter(component_label==component_input)
  
  # factor labels for ordering subgroups
  df$youth_label_factor <- factor(df$youth_label, levels = subgroups)
  
  df <- df %>%
    arrange(avg_adjusted) # order by rate
  
  
  # ----- This section prepares a dataframe for angled labels ---- #
  # add id
  df$id<- seq(1, nrow(df))
  
  # Get the name and the y position of each label
  label_data <- df
  
  # calculate the ANGLE of the labels
  number_of_bar <- nrow(df)
  angle <-  90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
  
  # calculate the alignment of labels: right or left
  # If I am on the left part of the plot, my labels have currently an angle < -90
  label_data$hjust<-ifelse( angle < -90, 1, 0)
  
  # flip angle BY to make them readable
  label_data$angle<-ifelse(angle < -90, angle+180, angle)
  
  # ----- This section creates the title and caption block ---- #
    title_block <- ggplot() +
    theme_void() +
    labs(
      title = title_text,
      subtitle = subtitle_text
    ) +
    theme(
      plot.title = element_markdown(hjust = 0, size = 18, family = font_title),
      plot.subtitle = element_text(hjust = 0, size = 13, family = font_subtitle),
      plot.margin = margin(t = 0, r = 0, b = -4, l = 0)
    )
  
  caption_block <- ggplot() +
    theme_void() +
    labs(
      caption = paste(
        "\nCatalyst California's calculations of Bold Vision Youth Thriving Survey, 2024. Note: AIAN=American Indian",
        "& Alaska Native; NHPI: Native Hawaiian & Pacific Islander; SWANA=Southwest Asian & North African. For",
        "more information on each group's definition, please refer to the 2025 Bold Vision Youth Thriving report.",
        sep = "\n"
      )
    ) +
    theme(
      plot.caption = element_text(hjust = 0, size = 11, family = font_caption),
      plot.margin = margin(t = -4, r = 0, b = 0, l = 0)
    )
  
  # ----- This section creates the plot ---- #
  
  p <- ggplot(df, aes(x=as.factor(id), y=avg_adjusted, group=component_label)) +
    geom_bar(aes(fill=avg_adjusted),stat = "identity", 
             alpha=1, show.legend=TRUE) +  
    scale_fill_gradientn(component_input, # legend title
                         colours=component_colors, # legend color ramp
                         labels=legend_labels # upper and lower labels for legend
    )+
    # Make the guide for the fill discrete
    guides(
      fill = guide_colorsteps(title.position = "top", title.hjust = .5
      )
    ) +
    scale_x_discrete(expand = c(0, 0)) +
    ylim(-.25,1.1) +
    ylab("")+
    xlab("")+
    theme_void() +
    theme(aspect.ratio=1,
          legend.title = element_text(hjust = 0.5,size = 11, family= font_axis_label),
          legend.text = element_text(hjust = 0.5,size = 11, family= font_axis_label),
          legend.position = "bottom", # no legend title
          legend.margin=margin(0,0,-5,0),
          legend.box.margin=margin(-8,0,-8,0),
          legend.key.height = unit(0.2, "cm"),
          legend.key.width = unit(1.5, "cm"),
          axis.text.y=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks=element_blank(),
          axis.ticks.length = unit(0, "pt"),
          axis.title.x=element_blank(),
          # grid line style
          panel.border=element_blank(),
          panel.grid = element_blank(),
          plot.margin = unit(c(-.6,-.1,-.6,-.1),"cm")
    ) + 
    coord_polar(clip="off") +
    # Add the labels, using the label_data dataframe that we have created before
    geom_text(data=label_data, aes(x=id, y=avg_adjusted+.006, label=youth_label, hjust=hjust), color="black", family=font_axis_label,alpha=0.6, size=3.4, angle= label_data$angle, inherit.aes = FALSE ) 
  
  # ----- This section saves and outputs the plot ---- #
  
  final_plot <- title_block / p  / caption_block + plot_layout(heights=c(.06,.93,.01))
  
  showtext_opts(dpi=300)
  
  ggsave(plot=final_plot, 
         file=paste0("./Visuals/",component_folder,"/circular_plot_",component_input,".png"),
         units = c("in"),  width = 7, height = 6)
  
  ggsave(plot=final_plot, 
         file=paste0("./Visuals/",component_folder,"/circular_plot_",component_input,".pdf"),
         units = c("in"),  width = 7, height = 6)
  
}

# Step 6: Run the function ------

### Structural Racism -------
component_input <- "Structural Racism" # component being visualized for filtering and legend title
component_folder <- "Racial Justice, Equity, And Inclusion" # name of folder in deliverables to save to
component_colors <-c("#D9D5FA", "#A7A0F4", "#756AEA", "#2A12B2") # color ramp for the legend
legend_labels <- c("<- Lower","","Higher ->") # if function doesn't work, e.g., error in get_labels might need to add another blank value
title_text <- "Average Predicted <span style ='color: #2A12B2;'>Structural Racism</span>" # replace color hex and name between <>
subtitle_text <- paste("All youth should live without experiencing structural racism, but in L.A. County, immigrant,", # text breaks in the subtitle after running initial visual
                       "unhoused, and LGBTQIA+ are predicted to experience structural racism the most.",
                       sep = "\n")

circular_plot(component_input,component_folder,component_colors,legend_labels,title_text,subtitle_text)
# works

### Microaggressions -------
component_input <- "Microaggressions" # component being visualized for filtering and legend title
component_folder <- "Racial Justice, Equity, And Inclusion" # name of folder in deliverables to save to
component_colors <- c("#D9D5FA", "#A7A0F4", "#756AEA", "#2A12B2")# color ramp for the legend
legend_labels <- c("<- Lower","","","Higher ->") # if function doesn't work, e.g., error in get_labels might need to add another blank value
title_text <- "Average Predicted <span style ='color: #2A12B2;'>Microaggressions </span>" # replace color hex and name between <>
subtitle_text <- paste("L.A. County youth vary in how likely they are to experience micoaggressions. Immigrant, Black,", # text breaks in the subtitle after running initial visual
  "unhoused, and SWANA youth are most likely to be subject to microaggressions.",
  sep = "\n")

circular_plot(component_input,component_folder,component_colors,legend_labels,title_text,subtitle_text)


### Caring Families and Relationships -------
component_input <- "Caring Families And Relationships" # component being visualized for filtering and legend title
component_folder <- "Caring Families And Relationships" # name of folder in deliverables to save to
component_colors <-c("#C4F0DC", "#7CDDBA", "#33C898", "#00864A")# color ramp for the legend
legend_labels <- c("<- Lower","","","Higher ->") # if function doesn't work, e.g., error in get_labels might need to add or remove another blank value
title_text <- "Average Predicted <span style ='color: #00864A;'>Caring Families and Relationships</span>" # replace color hex and name between <>
subtitle_text <- paste("All youth should have support from their families and other adults in their lives. Unhoused",
                        "systems impacted, and immigrant youth are least likely to feel they have someone to go to.",
                        sep = "\n")

circular_plot(component_input,component_folder,component_colors,legend_labels,title_text,subtitle_text)


### Self-Efficacy and Hope -------
component_input <- "Self-Efficacy And Hope" # component being visualized for filtering and legend title
component_folder <- "Positive Identity And Self-Worth" # name of folder in deliverables to save to
component_colors <-c("#FDD8DD", "#F9A3AF",  "#F14968", "#EF4A66")  # color ramp for the legend
legend_labels <- c("<- Lower","","","Higher ->") # if function doesn't work, e.g., error in get_labels might need to add another blank value
title_text <- "Average Predicted <span style ='color: #EF4A66;'>Self-Efficacy And Hope</span>" # replace color hex and name between <>
subtitle_text <- paste("All youth should feel hope and confidence. LGBTQIA+, Asian, and Multiracial youth on average", # text breaks in the subtitle after running initial visual
                       "feel less confidence and hope for their future compared to other youth",
                       sep = "\n"
)

circular_plot(component_input,component_folder,component_colors,legend_labels,title_text,subtitle_text)

### Cultural Identity -------
component_input <- "Cultural Identity" # component being visualized for filtering and legend title
component_folder <- "Cultural Identity" # name of folder in deliverables to save to
component_colors <- c("#D9D5FA", "#A7A0F4", "#756AEA", "#2A12B2") # color ramp for the legend
legend_labels <- c("<- Lower","","","","Higher ->") # if function doesn't work, e.g., error in get_labels might need to add another blank value
title_text <- "Average Predicted Feelings of <span style ='color: #2A12B2;'>Cultural Identity</span>" # replace color hex and name between <>
subtitle_text <- paste("All youth should be able to have a strong cultural identity. Multiracial, immigrant, LGBTQIA+,", # text breaks in the subtitle after running initial visual
                       "White, unhoused, and systems impacted youth derive less strength from their cultural identity.",
                       sep = "\n"
)

circular_plot(component_input,component_folder,component_colors,legend_labels,title_text,subtitle_text)

