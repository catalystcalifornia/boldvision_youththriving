# Create visuals of average component scores/subcomponent scores for all youth and by demographics

# Step 0: Setting up work space ------
library(dplyr)
library(ggplot2)
library(tidyverse)
library(stringr)
library(extrafont)
library(showtext)
# library(geofacet)
# # install.packages("geofacet")

# connect to postgres and source functions
source("W:\\RDA Team\\R\\credentials_source.R")

con <- connect_to_db("bold_vision")

# pulling in data
df_total <- dbGetQuery(con, "SELECT * 
                       FROM youth_thriving.factor_analysis_avg_scores_total
                       where component_model!='component_positive_identity_and_self_worth'")


df_race <- dbGetQuery(con, "SELECT * 
                       FROM youth_thriving.factor_analysis_avg_scores_race
                       where component_model!='component_positive_identity_and_self_worth'")

df_sogi <- dbGetQuery(con, "SELECT * 
                       FROM youth_thriving.factor_analysis_avg_scores_sogi
                       where component_model!='component_positive_identity_and_self_worth'")

df_age <- dbGetQuery(con, "SELECT * 
                       FROM youth_thriving.factor_analysis_avg_scores_age
                       where component_model!='component_positive_identity_and_self_worth'")

df_spa <- dbGetQuery(con, "SELECT * 
                       FROM youth_thriving.factor_analysis_avg_scores_spa
                       where component_model!='component_positive_identity_and_self_worth'")

df_systems <- dbGetQuery(con, "SELECT * 
                       FROM youth_thriving.factor_analysis_avg_scores_systems_involved
                       where component_model!='component_positive_identity_and_self_worth'")

df_all <- rbind(df_total,
                df_age,
                df_race,
                df_systems,
                df_sogi) %>%
  filter(!str_detect(subgroup,"not "))

table(df_all$subgroup, useNA='always')

min(df_all$avg)
max(df_all$avg)

# adjust minimum across data frames
df_all <- df_all %>%
  group_by(component_model) %>%
  mutate(min=min(avg),
    avg_adjusted = avg+abs(min(avg))+.002)

min(df_all$avg_adjusted)
max(df_all$avg_adjusted)

# data dictionary for component labels
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

component_labels$component_label

df_all <- df_all %>%
  left_join(component_labels)

df_all$component_label <- factor(df_all$component_label, levels = c("Freedom From Psychological Distress",
                                                                    "Self-Efficacy And Hope",
                                                                        "Cultural Identity",
                                                                    "Caring Families And Relationships",
                                                                        "Vibrant Communities", 
                                                                    "Freedom From Microaggressions",
                                                                        "Freedom From Discrimination",
                                                                        "Freedom From Structural Racism"))
                                                                        

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
light_blue <- "#465adc"
dark_blue <-'#220f8c'


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

# Make the plot
df_test <- df_all %>% filter(subgroup=='all youth')

p <- ggplot(df_test, aes(x=component_label, y=avg_adjusted, fill=component_label)) +
  geom_bar(stat = "identity", position = "dodge") +   
  scale_fill_manual(values = c(
    "Caring Families And Relationships" = light_green,
    "Freedom From Microaggressions" = light_blue,
    "Self-Efficacy And Hope" = dark_pink,
    "Freedom From Structural Racism" = dark_blue,
    "Freedom From Discrimination"=blue,
    "Cultural Identity" = yellow,
    "Freedom From Psychological Distress" = pink,
    "Vibrant Communities" = dark_green
  )) +    
  ylim(-.25,1) +
  ylab("")+
  labs(
    subtitle="All Youth"
  ) +
  theme_minimal() +
  theme(legend.title = element_blank(),
        # legend.position = "bottom", # no legend title 
        # define style for axis text
        axis.text.y=element_blank(),
        # axis.text.y = element_text(size = 9, colour = "black", family= font_axis_label, face = "bold"),
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        # axis.title.x = element_text(size = 12, colour = "black", family = font_axis_label, face = "bold"),
        # define style for title and caption
        plot.caption = element_text(hjust = 0.0, size = 8, colour = "black", family = font_caption, face = "plain"),
        # plot.title = element_text(hjust = 0.0, size = 18, colour = "black", family = font_title, face = "bold"),
        plot.subtitle = 
          element_text(hjust = 0.0, size = 12, colour = "black", family = font_title), 
        # grid line style
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) + 
  coord_polar()

p

ggsave(plot=p, 
       file="W:/Project/OSI/Bold Vision/Youth Thriving Survey/Deliverables/Component_Summary_test.png",
       units = c("in"),  width = 8, height = 8)

# Set a number of 'empty bar' to add at the end of each group
df <- df_all %>% filter(!subgroup %in% c("all youth", "nh_aian","nh_nhpi"))

df$component_label <- factor(df$component_label, levels = c("Freedom From Psychological Distress",
                                                                    "Self-Efficacy And Hope",
                                                                    "Cultural Identity",
                                                                    "Caring Families And Relationships",
                                                                    "Vibrant Communities", 
                                                                    "Freedom From Microaggressions",
                                                                    "Freedom From Discrimination",
                                                                    "Freedom From Structural Racism"))


empty_bar <- 4
to_add <- data.frame( matrix(NA, empty_bar*nlevels(df$component_label), ncol(df)) )
colnames(to_add) <- colnames(df)
to_add$component_label <- rep(levels(df$component_label), each=empty_bar)
df <- rbind(df, to_add)
df <- df %>% arrange(component_label)
df$id<- seq(1, nrow(df))

# Get the name and the y position of each label
label_data <- df
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# prepare a data frame for base lines
base_data <- df %>% 
  group_by(component_label) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))


p<-ggplot(df, aes(x=as.factor(id), y=avg_adjusted, fill=component_label, group=as.factor(component_label))) +
  geom_bar(stat = "identity", position = "dodge") +   
  scale_fill_manual(values = c(
    "Caring Families And Relationships" = light_green,
    "Freedom From Microaggressions" = light_blue,
    "Self-Efficacy And Hope" = dark_pink,
    "Freedom From Structural Racism" = dark_blue,
    "Freedom From Discrimination"=blue,
    "Cultural Identity" = yellow,
    "Freedom From Psychological Distress" = pink,
    "Vibrant Communities" = dark_green
  )) +    
  ylim(-.25,1) +
  ylab("")+
  xlab("")+
  theme_minimal() +
  theme(legend.title = element_blank(),
        # legend.position = "none", # no legend title
        # define style for axis text
        axis.text.y=element_blank(),
        # axis.text.y = element_text(size = 9, colour = "black", family= font_axis_label, face = "bold"),
        axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        # axis.title.x = element_text(size = 12, colour = "black", family = font_axis_label, face = "bold"),
        # define style for title and caption
        plot.caption = element_text(hjust = 0.0, size = 8, colour = "black", family = font_caption, face = "plain"),
        # plot.title = element_text(hjust = 0.0, size = 18, colour = "black", family = font_title, face = "bold"),
        plot.subtitle = 
          element_text(hjust = 0.0, size = 12, colour = "black", family = font_title), 
        # grid line style
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) + 
  coord_polar() +
  geom_text(data=label_data, aes(x=id, y=avg_adjusted+.025, label=subgroup, hjust=hjust), color="black", fontface="plain", family=font_axis_label, alpha=0.6, size=5, angle= label_data$angle, inherit.aes = FALSE ) 

p
ggsave(plot=p, 
       file="W:/Project/OSI/Bold Vision/Youth Thriving Survey/Deliverables/Component_Summary.png",
       units = c("in"),  width = 8, height = 8)


# Set a number of 'empty bar' to add at the end of each group
df <- df_all %>% filter(!subgroup %in% c("all youth", "nh_aian","nh_nhpi"))

# empty_bar <- 4
# to_add <- data.frame( matrix(NA, empty_bar*nlevels(df$subgroup), ncol(df)) )
# colnames(to_add) <- colnames(df)
# to_add$subgroup <- rep(levels(df$subgroup), each=empty_bar)
# df <- rbind(df, to_add)
# df <- df %>% arrange(subgroup)
# df$id<- seq(1, nrow(df))

# # Get the name and the y position of each label
# label_data <- df
# number_of_bar <- nrow(label_data)
# angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
# label_data$hjust <- ifelse( angle < -90, 1, 0)
# label_data$angle <- ifelse(angle < -90, angle+180, angle)

# # prepare a data frame for base lines
# base_data <- df %>% 
#   group_by(component_label) %>% 
#   summarize(start=min(id), end=max(id) - empty_bar) %>% 
#   rowwise() %>% 
#   mutate(title=mean(c(start, end)))


p<- ggplot(df, aes(x=subgroup, y=avg_adjusted, fill=component_label, group=component_label)) +
  geom_bar(stat = "identity", position = "dodge") +   
  scale_fill_manual(values = c(
    "Caring Families And Relationships" = light_green,
    "Freedom From Microaggressions" = light_blue,
    "Self-Efficacy And Hope" = dark_pink,
    "Freedom From Structural Racism" = dark_blue,
    "Freedom From Discrimination"=blue,
    "Cultural Identity" = yellow,
    "Freedom From Psychological Distress" = pink,
    "Vibrant Communities" = dark_green
  )) +    
  ylim(-.25,1) +
  ylab("")+
  xlab("")+
  theme_minimal() +
  theme(legend.title = element_blank(),
        # legend.position = "none", # no legend title
        # define style for axis text
        axis.text.y=element_blank(),
        # axis.text.y = element_text(size = 9, colour = "black", family= font_axis_label, face = "bold"),
        # axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        # axis.title.x = element_text(size = 12, colour = "black", family = font_axis_label, face = "bold"),
        # define style for title and caption
        plot.caption = element_text(hjust = 0.0, size = 8, colour = "black", family = font_caption, face = "plain"),
        # plot.title = element_text(hjust = 0.0, size = 18, colour = "black", family = font_title, face = "bold"),
        plot.subtitle = 
          element_text(hjust = 0.0, size = 12, colour = "black", family = font_title), 
        # grid line style
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) + 
  coord_polar() 


p
ggsave(plot=p, 
       file="W:/Project/OSI/Bold Vision/Youth Thriving Survey/Deliverables/Component_Summary_subgroup.png",
       units = c("in"),  width = 8, height = 8)


# just by component
df <- df_all %>% filter(component_label=='Freedom From Psychological Distress')

p<- ggplot(df, aes(x=subgroup, y=avg_adjusted, fill=component_label, group=component_label)) +
  geom_bar(stat = "identity", position = "dodge2",alpha=.9) +   
  scale_fill_manual(values = c(
    "Caring Families And Relationships" = light_green,
    "Freedom From Microaggressions" = light_blue,
    "Self-Efficacy And Hope" = dark_pink,
    "Freedom From Structural Racism" = dark_blue,
    "Freedom From Discrimination"=blue,
    "Cultural Identity" = yellow,
    "Freedom From Psychological Distress" = pink,
    "Vibrant Communities" = dark_green
  )) +    
  ylim(-.25,1) +
  ylab("")+
  xlab("")+
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.position = "none", # no legend title
        # define style for axis text
        axis.text.y=element_blank(),
        # axis.text.y = element_text(size = 9, colour = "black", family= font_axis_label, face = "bold"),
        # axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        # axis.title.x = element_text(size = 12, colour = "black", family = font_axis_label, face = "bold"),
        # define style for title and caption
        plot.caption = element_text(hjust = 0.0, size = 8, colour = "black", family = font_caption, face = "plain"),
        # plot.title = element_text(hjust = 0.0, size = 18, colour = "black", family = font_title, face = "bold"),
        plot.subtitle = 
          element_text(hjust = 0.0, size = 12, colour = "black", family = font_title), 
        # grid line style
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) + 
  coord_polar() 


p
ggsave(plot=p, 
       file="W:/Project/OSI/Bold Vision/Youth Thriving Survey/Deliverables/Component_Summary_component.png",
       units = c("in"),  width = 8, height = 8)