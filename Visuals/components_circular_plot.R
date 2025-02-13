# Create visuals of average component scores/subcomponent scores for all youth and by demographics

# Step 0: Setting up work space ------
library(dplyr)
library(ggplot2)
library(tidyverse)
library(stringr)
library(extrafont)
library(showtext)

# connect to postgres and source functions
source("W:\\RDA Team\\R\\credentials_source.R")

con <- connect_to_db("bold_vision")

# pulling in data
df_total <- dbGetQuery(con, "SELECT * 
                       FROM youth_thriving.factor_analysis_avg_scores_total
                       where component_model!='component_positive_identity_and_self_worth'")

# adjust minimum across data frames
df_total <- df_total %>%
  mutate(avg_adjusted = avg+abs(min(avg))+.002)

# data dictionary for component labels
component_labels <- select(df_total, component_model) %>%
  mutate(component_label=gsub(component_model,pattern="component_",replacement=""),
         component_label=gsub(component_label,pattern="sub",replacement=""),
         component_label=gsub(component_label,pattern="_", replacement=" "),
         component_label=str_to_title(component_label))%>%
  mutate(component_label=ifelse(component_label=="Self Efficacy Hope",
                                "Self-Efficacy And Hope",component_label))

df_total <- df_total %>%
  left_join(component_labels)

df_total$component_label <- factor(df_total$component_label, levels = c("Psychological Distress","Self-Efficacy And Hope",
                                                                        "Cultural Identity","Caring Families And Relationships",
                                                                        "Vibrant Communities", "Microaggressions",
                                                                        "Experiences Of Racism And Discrimination",
                                                                        "Structural Racism"))
                                                                        

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
p <- ggplot(df_total, aes(x=component_label, y=avg_adjusted, fill=component_label)) +
  geom_bar(stat = "identity", position = "dodge") +   
  scale_fill_manual(values = c(
    "Caring Families And Relationships" = light_green,
    "Microaggressions" = light_blue,
    "Self-Efficacy And Hope" = dark_pink,
    "Structural Racism" = dark_blue,
    "Experiences Of Racism And Discrimination"=blue,
    "Cultural Identity" = yellow,
    "Psychological Distress" = pink,
    "Vibrant Communities" = dark_green
  )) +    
  ylim(-.04,.2) +
  # ylab("Average Underlying Outcome")+
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


p <- p + 
  # Add labels
  labs(
    title = "\nHiking Locations in Washington",
    subtitle = paste(
      "\nThis Visualisation shows the cummulative length of tracks,",
      "the amount of tracks and the mean gain in elevation per location.\n",
      "If you are an experienced hiker, you might want to go",
      "to the North Cascades since there are a lot of tracks,",
      "higher elevations and total length to overcome.",
      sep = "\n"
    ),
    caption = "\n\nData Visualisation by Tobias Stalder\ntobias-stalder.netlify.app\nSource: TidyX Crew (Ellis Hughes, Patrick Ward)\nLink to Data: github.com/rfordatascience/tidytuesday/blob/master/data/2020/2020-11-24/readme.md") +
  # Customize general theme
  theme(
    
    # Set default color and font family for the text
    text = element_text(color = "gray12", family = "Bell MT"),
    
    # Customize the text in the title, subtitle, and caption
    plot.title = element_text(face = "bold", size = 25, hjust = 0.05),
    plot.subtitle = element_text(size = 14, hjust = 0.05),
    plot.caption = element_text(size = 10, hjust = .5),
    
    # Make the background white and remove extra grid lines
    panel.background = element_rect(fill = "white", color = "white"),
    panel.grid = element_blank(),
    panel.grid.major.x = element_blank()
  )
# Use `ggsave("plot.png", plt,width=9, height=12.6)` to save it as in the output
plt