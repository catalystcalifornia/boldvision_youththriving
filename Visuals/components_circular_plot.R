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
# library(RPostgreSQL)
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
subgroups<-c("AIAN","Another Race","Asian","Black","Latine","Multiracial","NHPI","SWANA","White","BIPOC","Systems Impacted","Undocumented","Unhoused","Cis Man/Boy","Cis Woman/Girl","LGBTQIA+")
subgroups

# list of components to focus on
component_labels$component_label
components<-c("Freedom From Psychological Distress","Self-Efficacy And Hope","Freedom From Microaggressions","Caring Families And Relationships","Cultural Identity","Freedom From Structural Racism","Vibrant Communities")
components

# filter dataframe
df_all <- df_all %>%
  filter(youth_label %in% subgroups & 
         component_label %in% components) 
  
  
# Step 4: Run circular bar plot just by component -------
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
           # position = "dodge2",
           alpha=1, show.legend=TRUE) +  
  # scale_fill_gradient(low="#FDDFF3",high="#F75EC1") +
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
  ylim(-.25,1.5) +
  ylab("")+
  xlab("")+
  # Add labels
  labs(
    title = "Average Predicted <span style ='color: #F75EC1;'>Freedom from Psychological Distress </span>",
    subtitle = paste
      ("\nLA County youth vary in how they are thriving emotionally. LGBTQIA+, systems",
      "impacted, cisgender women/girl, and Asian youth experience the most ",
      "significant differences.",
      sep = "\n"
    ),
    caption = "\nBold Vision, Youth Thriving Survey, 2024.") +
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
        plot.caption = element_text(hjust = 0.0, size = 11, colour = "black", family = font_caption, face = "plain"),
        plot.subtitle = 
          element_text(hjust = 0.0, size = 14, family = font_subtitle), 
        plot.title = 
          element_markdown(hjust = 0.0, size = 20, family = font_title, lineheight=1)
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
  geom_text(data=label_data, aes(x=id, y=avg_adjusted+.03, label=youth_label, hjust=hjust), color="black", family=font_axis_label,alpha=0.6, size=4, angle= label_data$angle, inherit.aes = FALSE ) 


showtext_opts(dpi=300)

ggsave(plot=p, 
       file="W:/Project/OSI/Bold Vision/Youth Thriving Survey/Deliverables/Strong Minds/Component_Summary.png",
       units = c("in"),  width = 8, height = 8)

ggsave(plot=p, 
       file="W:/Project/OSI/Bold Vision/Youth Thriving Survey/Deliverables/Strong Minds/Component_Summary.pdf",
       units = c("in"),  width = 8, height = 8)


# Step 5: Run circular bar plot for another component -------
# filter for the component
df <- df_all %>% filter(component_label=='Freedom From Microaggressions')

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
           # position = "dodge2",
           alpha=1, show.legend=TRUE) +  
  # scale_fill_gradient(low="#FDDFF3",high="#F75EC1") +
  scale_fill_gradientn("Freedom From Microaggressions",
                       colours=c("#D4D0F0","#AAA0E0","#7F71D1","#2A12B2")
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
  ylim(-.25,1) +
  ylab("")+
  xlab("")+
  # Add labels
  labs(
    title = "Average Predicted <span style ='color: #2A12B2;'>Freedom from Microaggressions </span>",
    subtitle = paste
    ("\nLA County youth vary in how they are thriving emotionally. LGBTQIA+, systems",
      "impacted, cisgender women/girl, and Asian youth experience the most ",
      "significant differences.",
      sep = "\n"
    ),
    caption = "\nBold Vision, Youth Thriving Survey, 2024.") +
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
        plot.caption = element_text(hjust = 0.0, size = 11, colour = "black", family = font_caption, face = "plain"),
        plot.subtitle = 
          element_text(hjust = 0.0, size = 14, family = font_subtitle), 
        plot.title = 
          element_markdown(hjust = 0.0, size = 20, family = font_title, lineheight=1)
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
  geom_text(data=label_data, aes(x=id, y=avg_adjusted+.03, label=youth_label, hjust=hjust), color="black", family=font_axis_label,alpha=0.6, size=4, angle= label_data$angle, inherit.aes = FALSE ) 


showtext_opts(dpi=300)

ggsave(plot=p, 
       file="W:/Project/OSI/Bold Vision/Youth Thriving Survey/Deliverables/Racial Justice, Equity, And Inclusion/Component_Summary_Microaggressions.png",
       units = c("in"),  width = 8, height = 8)

ggsave(plot=p, 
       file="W:/Project/OSI/Bold Vision/Youth Thriving Survey/Deliverables/Racial Justice, Equity, And Inclusion/Component_Summary_Microaggressions.pdf",
       units = c("in"),  width = 8, height = 8)

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