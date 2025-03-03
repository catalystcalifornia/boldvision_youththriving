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
library(RPostgreSQL)
# library(geofacet)
# # install.packages("geofacet")

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
                                ifelse(component_label %in% c("Psychological Distress","Microaggressions",
                                                              "Discrimination",
                                                              "Structural Racism"),
                                       paste0("Freedom From ",component_label), component_label)
                                       ))
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

# # define fonts in chart - old ones
# font_title <- "Manifold CF"
# font_subtitle <- "Manifold CF"
# font_caption <- "HelveticaNeueLTStdMdCn"
# font_bar_label <- "HelveticaNeueLTStdHvCn"
# font_axis_label <- "HelveticaNeueLTStdMdCn"

# define fonts in chart
font_title <- "HelveticaNeueLTStdHvCn"
font_subtitle <- "HelveticaNeueLTStdMdCn"
font_caption <- "Manifold Regular"
font_bar_label <- "Manifold Regular"
font_axis_label <- "Manifold Regular"



# Step 3: Filter for selected components and for selected demographics -----
# list of demographics to focus on
unique(df_all$youth_label)
subgroups<-c("AIAN","Asian","Black","Latine","Multiracial","NHPI","SWANA","White","BIPOC","Systems Impacted","Undocumented","Unhoused","Cis Man/Boy","Cis Woman/Girl","LGBTQIA+")
subgroups

# list of components to focus on
component_labels$component_label
components<-c("Freedom From Psychological Distress","Self-Efficacy And Hope","Freedom From Microaggressions","Caring Families And Relationships","Cultural Identity","Freedom From Structural Racism","Vibrant Communities")
components

# filter dataframe
df_all <- df_all %>%
  filter(youth_label %in% subgroups & # filter for subgroups
         component_label %in% components) # filter for components
  
  
# Step 4: Run circular bar plot just by one component to test - PSYCHOLOGICAL DISTRESS-------
# filter for the component
df <- df_all %>% filter(component_label=='Freedom From Psychological Distress')

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

p <- ggplot(df, aes(x=as.factor(id), y=avg_adjusted, group=component_label)) +
  geom_bar(aes(fill=avg_adjusted),stat = "identity", 
           alpha=1, show.legend=TRUE) +  
  scale_fill_gradientn("Freedom From Psychological Distress",
  colours=c("#FDDFF3","#FA9EDA","#F97ECD","#F75EC1")
  ,
  labels=c("<- Lower","","Higher ->")
  )+
  # Make the guide for the fill discrete
  guides(
    fill = guide_colorsteps(
      barwidth = 15, barheight = .5, title.position = "top", title.hjust = .5
    )
  ) +
  # scale_fill_manual(values = c(
  #   "Caring Families And Relationships" = light_green,
  #   "Freedom From Microaggressions" = light_blue,
  #   "Self-Efficacy And Hope" = dark_pink,
  #   "Freedom From Structural Racism" = dark_blue,
  #   "Freedom From Discrimination"=blue,
  #   "Cultural Identity" = yellow,
  #   "Freedom From Psychological Distress" = pink,
  #   "Vibrant Communities" = dark_green
  # )) +
  ylim(-.25,1.3) +
  ylab("")+
  xlab("")+
  # Add labels
  labs(
    title = "Average Predicted <span style ='color: #F75EC1;'>Freedom from Psychological <br>Distress</span>", 
    subtitle = paste
      ("\nLA County youth vary in how they are thriving emotionally. LGBTQIA+,",
      "systems impacted, cisgender women/girl, and Asian youth experience",
      "the most significant differences compared to their counterparts.",
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
  theme(legend.title = element_text(hjust = 0.5,size = 14, family= font_axis_label),
        legend.text = element_text(hjust = 0.5,size = 14, family= font_axis_label),
        legend.position = "bottom", # no legend title
        legend.margin=margin(0,0,0,0),
        legend.box.margin=margin(-2,-2,-2,-2),
        # define style for axis text
        axis.text.y=element_blank(),
        # axis.text.y = element_text(size = 9, colour = "black", family= font_axis_label, face = "bold"),
        axis.text.x=element_blank(),
        # axis.text.x=element_text(size = 11, colour = "black", family = font_axis_label),
        axis.title.x=element_blank(),
        # axis.title.x = element_text(size = 12, colour = "black", family = font_axis_label, face = "bold"),
        # define style for title and caption
        plot.caption = element_text(hjust = 0.0, size = 10, colour = "black", family = font_caption, face = "plain"),
        plot.subtitle = 
          element_text(hjust = 0.0, size = 14, family = font_subtitle), 
        plot.title = 
          element_markdown(hjust = 0.0, size = 20, family = font_title)
        # ,
        #   element_text(hjust = 0.0, size = 20, colour = "black", family = font_title)
        , 
        # grid line style
        panel.grid = element_blank(),
        # plot.margin = unit(rep(-1,4), "cm")  
        # plot.margin=margin(0,0,0,0)
        ) + 
  coord_polar() +
  # Add the labels, using the label_data dataframe that we have created before
  geom_text(data=label_data, aes(x=id, y=avg_adjusted+.02, label=youth_label, hjust=hjust), color="black", family=font_axis_label,alpha=0.6, size=4, angle= label_data$angle, inherit.aes = FALSE ) 


showtext_opts(dpi=300)

ggsave(plot=p, 
       file="W:/Project/OSI/Bold Vision/Youth Thriving Survey/Deliverables/Strong Minds/circular_plot_Psychological_Distress.png",
       units = c("in"),  width = 8, height = 8)

ggsave(plot=p, 
       file="W:/Project/OSI/Bold Vision/Youth Thriving Survey/Deliverables/Strong Minds/circular_plot_Psychological_Distress.pdf",
       units = c("in"),  width = 8, height = 8)


# Step 5: Make a function for circular bar plot -------

circular_plot <- function(component_input,component_folder,component_colors,legend_labels, title_text,subtitle_text) {
  
  # ----- This section prepares the data for the visual ---- #
  
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
      fill = guide_colorsteps(
        barwidth = 15, barheight = .5, title.position = "top", title.hjust = .5
      )
    ) +
    ylim(-.25,1.3) + # limits of the chart based on range across the whole dataframe
    ylab("")+
    xlab("")+
    # Add labels
    labs(
      title = title_text,
      subtitle = subtitle_text,
      caption =  paste("\nCatalyst California's calculations of Bold Vision Youth Thriving Survey, 2024.",
                       "Note: AIAN=American Indian & Alaska Native; BIPOC=Black, Indigeneous, People of Color;", 
                       "LGBTQIA+=Lesbian, Gay, Bisexual, Transgender, Queer, Intersex, Asexual, & Gender", 
                       "Nonconforming; NHPI: Native Hawaiian & Pacific Islander; SWANA=Southwest Asian & North",
                       "African; Systems Impacted=Youth at any point in foster care, juvenile hall/probation camp",
                       "jail/prison, group home/residential program, or lived with legal guardians.",
                       sep="\n"))+
    theme_minimal() +
    theme(
      # define legend style
      legend.title = element_text(hjust = 0.5,size = 14, family= font_axis_label),
      legend.text = element_text(hjust = 0.5,size = 14, family= font_axis_label),
      legend.position = "bottom", 
      legend.margin=margin(0,0,0,0),
      legend.box.margin=margin(-2,-2,-2,-2),
      # define style for axis text
      axis.text.y=element_blank(),
      axis.text.x=element_blank(),
      axis.title.x=element_blank(),
      # define style for title, subtitle, and caption
      plot.caption = element_text(hjust = 0.0, size = 11, colour = "black", family = font_caption, face = "plain"),
      plot.subtitle = 
        element_text(hjust = 0.0, size = 14, family = font_subtitle), 
      plot.title = 
        element_markdown(hjust = 0.0, size = 20, family = font_title, lineheight=1), 
      # grid line style
      panel.grid = element_blank(),
    ) + 
    coord_polar() +
    # Add the labels, using the label_data dataframe that we have created before
    geom_text(data=label_data, aes(x=id, y=avg_adjusted+.02, label=youth_label, hjust=hjust), color="black", family=font_axis_label,alpha=0.6, size=4, angle= label_data$angle, inherit.aes = FALSE ) 
  
  
  # ----- This section saves and outputs the plot ---- #
  
  showtext_opts(dpi=300)
  
  ggsave(plot=p, 
         file=paste0("W:/Project/OSI/Bold Vision/Youth Thriving Survey/Deliverables/",component_folder,"/circular_plot_",component_input,".png"),
         units = c("in"),  width = 8, height = 8)
  
  ggsave(plot=p, 
         file=paste0("W:/Project/OSI/Bold Vision/Youth Thriving Survey/Deliverables/",component_folder,"/circular_plot_",component_input,".pdf"),
         units = c("in"),  width = 8, height = 8)
  
}

# Step 6: Run the function ------

### Structural Racism -------
component_input <- "Freedom From Structural Racism" # component being visualized for filtering and legend title
component_folder <- "Racial Justice, Equity, And Inclusion" # name of folder in deliverables to save to
component_colors <-c("#A79FD1","#7A6FBA","#4E3FA3","#220f8c") # color ramp for the legend
legend_labels <- c("<- Lower","","","Higher ->") # if function doesn't work, e.g., error in get_labels might need to add another blank value
title_text <- "Average Predicted <span style ='color: #220f8c;'>Freedom From Structural Racism</span>" # replace color hex and name between <>
subtitle_text <- paste("\nLA County youth vary in how freely they can live without experiencing", # text breaks in the subtitle after running initial visual
                       "structural racism. Undocumented, unhoused, LGBTQIA+, and systems",
                       "impacted youth and many youth of color groups are most likely to",
                       "experience structural racism.",
                       sep = "\n"
)

circular_plot(component_input,component_folder,component_colors,legend_labels,title_text,subtitle_text)
# works

### Microaggressions -------
component_input <- "Freedom From Microaggressions" # component being visualized for filtering and legend title
component_folder <- "Racial Justice, Equity, And Inclusion" # name of folder in deliverables to save to
component_colors <- c("#D4D0F0","#AAA0E0","#7F71D1","#2A12B2")# color ramp for the legend
legend_labels <- c("<- Lower","","","Higher ->") # if function doesn't work, e.g., error in get_labels might need to add another blank value
title_text <- "Average Predicted <span style ='color: #2A12B2;'>Freedom from Microaggressions </span>" # replace color hex and name between <>
subtitle_text <- paste("\nLA County youth vary in how likely they are to live their lives free from", # text breaks in the subtitle after running initial visual
  "microaggressions. Undocumented, Black, unhoused, and SWANA youth",
  "are more likely to experience microaggressions.",
  sep = "\n")

circular_plot(component_input,component_folder,component_colors,legend_labels,title_text,subtitle_text)


### Caring Families and Relationships -------
component_input <- "Caring Families And Relationships" # component being visualized for filtering and legend title
component_folder <- "Caring Families And Relationships" # name of folder in deliverables to save to
component_colors <-c("#99CFB7","#66B692","#339E6E","#00864A") # color ramp for the legend
legend_labels <- c("<- Lower","","","","Higher ->") # if function doesn't work, e.g., error in get_labels might need to add or remove another blank value
title_text <- "Average Predicted <span style ='color: #00864A;'>Caring Families and <br>Relationships</span>" # replace color hex and name between <>
subtitle_text <- paste("\nLA County youth vary in the support they have from family, adults,",
                        "and other relationships. Unhoused, systems impacted, undocumented,",
                        "and LGBTQIA+ youth, and many BIPOC youth, are least likely to feel they",
                        "have people to go to in good and bad times.",
                        sep = "\n")

circular_plot(component_input,component_folder,component_colors,legend_labels,title_text,subtitle_text)


### Self-Efficacy and Hope -------
component_input <- "Self-Efficacy And Hope" # component being visualized for filtering and legend title
component_folder <- "Positive Identity And Self-Worth" # name of folder in deliverables to save to
component_colors <-c("#F9B7C2","#F592A3","#F26E85","#EF4A66") # color ramp for the legend
legend_labels <- c("<- Lower","","","Higher ->") # if function doesn't work, e.g., error in get_labels might need to add another blank value
title_text <- "Average Predicted Feeling of <span style ='color: #EF4A66;'>Self-Efficacy And Hope</span>" # replace color hex and name between <>
subtitle_text <- paste("\nLA County youth vary in how much they believe in themselves", # text breaks in the subtitle after running initial visual
                       "and have hope for their future. LGBTQIA+, Asian, and Multiracial youth",
                       "on average feel less confidence and hope for their future compared",
                       "to their counterparts.",
                       sep = "\n"
)

circular_plot(component_input,component_folder,component_colors,legend_labels,title_text,subtitle_text)

### Cultural Identity -------
component_input <- "Cultural Identity" # component being visualized for filtering and legend title
component_folder <- "Cultural Identity" # name of folder in deliverables to save to
component_colors <- c("#FFE599","#FFD966","#FFCC33","#FFBF00") # color ramp for the legend
legend_labels <- c("<- Lower","","","","","Higher ->") # if function doesn't work, e.g., error in get_labels might need to add another blank value
title_text <- "Average Predicted Feelings of <span style ='color: #FFBF00;'>Cultural Identity</span>" # replace color hex and name between <>
subtitle_text <- paste("\nLA County youth vary in how they are connected to their cultural identity.", # text breaks in the subtitle after running initial visual
                       "Multiracial, undocumented, LGBTQIA+, White, unhoused, and systems",
                       "impacted youth on average have less strength in their cultural identity",
                       "compared to their counterparts.",
                       sep = "\n"
)

circular_plot(component_input,component_folder,component_colors,legend_labels,title_text,subtitle_text)

# # ----- Saving old code
# 
# # factor labels for ordering in the summative graph
# df_all$component_label <- factor(df_all$component_label, levels = c("Freedom From Psychological Distress",
#                                                                     "Self-Efficacy And Hope",
#                                                                     "Cultural Identity",
#                                                                     "Caring Families And Relationships",
#                                                                     "Vibrant Communities", 
#                                                                     "Freedom From Microaggressions",
#                                                                     "Freedom From Discrimination",
#                                                                     "Freedom From Structural Racism"))
# 
# 
# # Make the plot
# df_test <- df_all %>% filter(subgroup=='all youth')
# 
# p <- ggplot(df_test, aes(x=component_label, y=avg_adjusted, fill=component_label)) +
#   geom_bar(stat = "identity", position = "dodge") +   
#   scale_fill_manual(values = c(
#     "Caring Families And Relationships" = light_green,
#     "Freedom From Microaggressions" = light_blue,
#     "Self-Efficacy And Hope" = dark_pink,
#     "Freedom From Structural Racism" = dark_blue,
#     "Freedom From Discrimination"=blue,
#     "Cultural Identity" = yellow,
#     "Freedom From Psychological Distress" = pink,
#     "Vibrant Communities" = dark_green
#   )) +    
#   ylim(-.25,1) +
#   ylab("")+
#   labs(
#     subtitle="All Youth"
#   ) +
#   theme_minimal() +
#   theme(legend.title = element_blank(),
#         # legend.position = "bottom", # no legend title 
#         # define style for axis text
#         axis.text.y=element_blank(),
#         # axis.text.y = element_text(size = 9, colour = "black", family= font_axis_label, face = "bold"),
#         axis.text.x=element_blank(),
#         axis.title.x=element_blank(),
#         # axis.title.x = element_text(size = 12, colour = "black", family = font_axis_label, face = "bold"),
#         # define style for title and caption
#         plot.caption = element_text(hjust = 0.0, size = 8, colour = "black", family = font_caption, face = "plain"),
#         # plot.title = element_text(hjust = 0.0, size = 18, colour = "black", family = font_title, face = "bold"),
#         plot.subtitle = 
#           element_text(hjust = 0.0, size = 12, colour = "black", family = font_title), 
#         # grid line style
#         panel.grid.minor = element_blank(),
#         panel.grid.major = element_blank()) + 
#   coord_polar()
# 
# p
# 
# ggsave(plot=p, 
#        file="W:/Project/OSI/Bold Vision/Youth Thriving Survey/Deliverables/Component_Summary_test.png",
#        units = c("in"),  width = 8, height = 8)
# 
# # Set a number of 'empty bar' to add at the end of each group
# df <- df_all %>% filter(!subgroup %in% c("all youth", "nh_aian","nh_nhpi"))
# 
# df$component_label <- factor(df$component_label, levels = c("Freedom From Psychological Distress",
#                                                             "Self-Efficacy And Hope",
#                                                             "Cultural Identity",
#                                                             "Caring Families And Relationships",
#                                                             "Vibrant Communities", 
#                                                             "Freedom From Microaggressions",
#                                                             "Freedom From Discrimination",
#                                                             "Freedom From Structural Racism"))
# 
# 
# empty_bar <- 4
# to_add <- data.frame( matrix(NA, empty_bar*nlevels(df$component_label), ncol(df)) )
# colnames(to_add) <- colnames(df)
# to_add$component_label <- rep(levels(df$component_label), each=empty_bar)
# df <- rbind(df, to_add)
# df <- df %>% arrange(component_label)
# df$id<- seq(1, nrow(df))
# 
# # Get the name and the y position of each label
# label_data <- df
# number_of_bar <- nrow(label_data)
# angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
# label_data$hjust <- ifelse( angle < -90, 1, 0)
# label_data$angle <- ifelse(angle < -90, angle+180, angle)
# 
# # prepare a data frame for base lines
# base_data <- df %>% 
#   group_by(component_label) %>% 
#   summarize(start=min(id), end=max(id) - empty_bar) %>% 
#   rowwise() %>% 
#   mutate(title=mean(c(start, end)))
# 
# 
# p<-ggplot(df, aes(x=as.factor(id), y=avg_adjusted, fill=component_label, group=as.factor(component_label))) +
#   geom_bar(stat = "identity", position = "dodge") +   
#   scale_fill_manual(values = c(
#     "Caring Families And Relationships" = light_green,
#     "Freedom From Microaggressions" = light_blue,
#     "Self-Efficacy And Hope" = dark_pink,
#     "Freedom From Structural Racism" = dark_blue,
#     "Freedom From Discrimination"=blue,
#     "Cultural Identity" = yellow,
#     "Freedom From Psychological Distress" = pink,
#     "Vibrant Communities" = dark_green
#   )) +    
#   ylim(-.25,1) +
#   ylab("")+
#   xlab("")+
#   theme_minimal() +
#   theme(legend.title = element_blank(),
#         # legend.position = "none", # no legend title
#         # define style for axis text
#         axis.text.y=element_blank(),
#         # axis.text.y = element_text(size = 9, colour = "black", family= font_axis_label, face = "bold"),
#         axis.text.x=element_blank(),
#         axis.title.x=element_blank(),
#         # axis.title.x = element_text(size = 12, colour = "black", family = font_axis_label, face = "bold"),
#         # define style for title and caption
#         plot.caption = element_text(hjust = 0.0, size = 8, colour = "black", family = font_caption, face = "plain"),
#         # plot.title = element_text(hjust = 0.0, size = 18, colour = "black", family = font_title, face = "bold"),
#         plot.subtitle = 
#           element_text(hjust = 0.0, size = 12, colour = "black", family = font_title), 
#         # grid line style
#         panel.grid.minor = element_blank(),
#         panel.grid.major = element_blank()) + 
#   coord_polar() +
#   geom_text(data=label_data, aes(x=id, y=avg_adjusted+.025, label=subgroup, hjust=hjust), color="black", fontface="plain", family=font_axis_label, alpha=0.6, size=5, angle= label_data$angle, inherit.aes = FALSE ) 
# 
# p
# ggsave(plot=p, 
#        file="W:/Project/OSI/Bold Vision/Youth Thriving Survey/Deliverables/Component_Summary.png",
#        units = c("in"),  width = 8, height = 8)
# 
# 
# # Set a number of 'empty bar' to add at the end of each group
# df <- df_all %>% filter(!subgroup %in% c("all youth", "nh_aian","nh_nhpi"))
# 
# # empty_bar <- 4
# # to_add <- data.frame( matrix(NA, empty_bar*nlevels(df$subgroup), ncol(df)) )
# # colnames(to_add) <- colnames(df)
# # to_add$subgroup <- rep(levels(df$subgroup), each=empty_bar)
# # df <- rbind(df, to_add)
# # df <- df %>% arrange(subgroup)
# # df$id<- seq(1, nrow(df))
# 
# # # Get the name and the y position of each label
# # label_data <- df
# # number_of_bar <- nrow(label_data)
# # angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
# # label_data$hjust <- ifelse( angle < -90, 1, 0)
# # label_data$angle <- ifelse(angle < -90, angle+180, angle)
# 
# # # prepare a data frame for base lines
# # base_data <- df %>% 
# #   group_by(component_label) %>% 
# #   summarize(start=min(id), end=max(id) - empty_bar) %>% 
# #   rowwise() %>% 
# #   mutate(title=mean(c(start, end)))
# 
# 
# p<- ggplot(df, aes(x=subgroup, y=avg_adjusted, fill=component_label, group=component_label)) +
#   geom_bar(stat = "identity", position = "dodge") +   
#   scale_fill_manual(values = c(
#     "Caring Families And Relationships" = light_green,
#     "Freedom From Microaggressions" = light_blue,
#     "Self-Efficacy And Hope" = dark_pink,
#     "Freedom From Structural Racism" = dark_blue,
#     "Freedom From Discrimination"=blue,
#     "Cultural Identity" = yellow,
#     "Freedom From Psychological Distress" = pink,
#     "Vibrant Communities" = dark_green
#   )) +    
#   ylim(-.25,1) +
#   ylab("")+
#   xlab("")+
#   theme_minimal() +
#   theme(legend.title = element_blank(),
#         # legend.position = "none", # no legend title
#         # define style for axis text
#         axis.text.y=element_blank(),
#         # axis.text.y = element_text(size = 9, colour = "black", family= font_axis_label, face = "bold"),
#         # axis.text.x=element_blank(),
#         axis.title.x=element_blank(),
#         # axis.title.x = element_text(size = 12, colour = "black", family = font_axis_label, face = "bold"),
#         # define style for title and caption
#         plot.caption = element_text(hjust = 0.0, size = 8, colour = "black", family = font_caption, face = "plain"),
#         # plot.title = element_text(hjust = 0.0, size = 18, colour = "black", family = font_title, face = "bold"),
#         plot.subtitle = 
#           element_text(hjust = 0.0, size = 12, colour = "black", family = font_title), 
#         # grid line style
#         panel.grid.minor = element_blank(),
#         panel.grid.major = element_blank()) + 
#   coord_polar() 
# 
# 
# p
# ggsave(plot=p, 
#        file="W:/Project/OSI/Bold Vision/Youth Thriving Survey/Deliverables/Component_Summary_subgroup.png",
#        units = c("in"),  width = 8, height = 8)