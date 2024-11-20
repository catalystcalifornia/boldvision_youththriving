source('W:\\Project\\OSI\\Bold Vision\\Youth Thriving Survey\\GitHub\\MK\\boldvision_youththriving\\Visuals\\visual_functions.R')

true_factors<-c("Don't wish to answer", "Never true","Sometimes true","Often true","Always true") 
time_factors_reverse<-c("All of the time", "Most of the time", "Some of the time", "A little of the time", "None of the time", "Don't wish to answer") #reverse so a greater number means a good outcome and a smaller number means a bad outcome
yes_factors<-c("Don't wish to answer", "Don't know", "No", "Yes")
yes_factors_reverse <- c("Yes", "No", "Don't know", "Don't wish to answer") #reverse so a greater number means a good outcome and a smaller number means a bad outcome
count_factors<-c("Don't wish to answer", "None","One","Two","Three or more")
freq_factors_reverse<-c("All of the time", "Most of the time","Sometimes", "Rarely","Never", "Does not apply to me") #reverse so a greater number means a good outcome and a smaller number means a bad outcome

data_freq_ques <- dbGetQuery(con, paste0("SELECT * FROM youth_thriving.tot_freq_safety"))

df_filter <- data_freq_ques %>% filter(variable == 'q19')  %>%
  mutate(response = factor(response, levels = freq_factors_reverse))  # Set the order of levels

df_visual <- ggplot(df_filter, aes(x = weighted_percent, y = response,
                                   #fill based on domain/component
                                   fill = ifelse(domain == 'Caring Families And Relationships', pink, 
                                                                                     ifelse(domain == 'Cultural Identity', dark_pink,
                                                                                            ifelse(domain == 'Demographics', gray,
                                                                                                   ifelse(domain == 'Positive Identity And Self-Worth', yellow, 
                                                                                                          ifelse(domain == 'Racial Justice, Equity, And Inclusion', light_green,
                                                                                                                 ifelse(domain == 'Safety', dark_green,
                                                                                                                        ifelse(domain == 'Strong Minds', blue,
                                                                                                                               ifelse(domain == 'Vibrant Communities', orange,
                                                                                                                                      gray)))))))))) +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_y_discrete(labels = function(response) str_wrap(response, width = 18)) +
  # bar labels
  geom_text(data = df_filter,
            aes(label = paste0(round(weighted_percent, digits = 1), "%")),
            size = 4,
            stat="identity", colour = "white",
            position = position_dodge(width = 1), 
            vjust = 1.25 ,
            # hjust= 1.15,
            fontface = "bold", family=font_bar_label) +  
  labs(title = paste(str_wrap('title_text', whitespace_only = TRUE, width = 57), collapse = "\n"),
       x = paste(str_wrap('x_axis_text', whitespace_only = TRUE, width = 65), collapse = "\n"),
       y = "",
       caption= paste(str_wrap(paste0("Question: ", unique(df_filter$question), "\n",
                                      " Sub Question: ", unique(df_filter$sub_question), "\n",
                                      " Category: ", unique(df_filter$domain), "\n",
                                      " Data Source: Catalyst California, Bold Vision Youth Thriving Survey, 2024."),
                               whitespace_only = TRUE, width = 120), collapse = "\n")) +
  #theme/aesthetics
  theme_minimal() +
  theme(legend.position = "none",
        # define style for axis text
        axis.text.y = element_text(size = 9, colour = "black", family= font_axis_label, face = "bold"),
        axis.title.x = element_text(size = 10, colour = "black", family = font_axis_label, face = "bold"),
        # define style for title and caption
        plot.caption = element_text(hjust = 0.0, size = 8, colour = "black", family = font_caption),
        plot.title = element_text(hjust = 0.0, size = 18, colour = "black", family = font_title, face = "bold"),
        # grid line style
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank())  +
  coord_flip()
