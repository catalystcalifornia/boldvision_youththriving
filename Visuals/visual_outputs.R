# Code for running visuals for rates by question and rates by question and race

# source script
source('Visuals\\visual_functions.R')
source('Visuals\\visual_smallmultiples.R')
# source('W:/Project/OSI/Bold Vision/Youth Thriving Survey/GitHub/EMG/boldvision_youththriving/Visuals/visual_smallmultiples.R') # only use this line with local folder when testing changes

#### Visualizing for Youth Council Presentation 03/08/25 ####
#dl
df_dl <- fx_create_df(con, tables, "Caring Families And Relationships", "dl", "tot_freq_caring_families") 
View(df_dl)
fx_vis_smallmultiples(df = df_dl, title_text = 'Caring individuals youth can rely on are essential to youth wellbeing, but fewer NHPI, undocumented, and SWANA youth feel they always have someone to go to with a problem',
                      subtitle_text = 'When I have a problem, I have someone who will be there for me', 
                      likert_factors = true_factors, graph_orderby = "Always true")

#q10
df_q10 <- fx_create_df(con, tables, "Positive Identity And Self-Worth", "q10", "tot_freq_positive_identity") 
View(df_q10)
fx_vis_smallmultiples(df = df_q10, title_text = 'Having a spark in life supports emotional health and hope for the future, but LGBTQIA and NHPI youth are least likely to believe they have a spark',
                      subtitle_text = "When people are really happy, energized, and passionate about their talents, interests, or hobbies, we say they have a “spark” in their life.... Do you have this kind of spark in your life?",
                     likert_factors = yes_factors_reverse, graph_orderby = "Yes")

#eo
df_eo <- fx_create_df(con, tables, "Racial Justice, Equity, And Inclusion", "eo", "tot_freq_racial_justice") 
View(df_eo)
fx_vis_smallmultiples(df = df_eo, title_text = 'Undocumented, SWANA, and Black youth are most likely to experience distressing and racialized interactions'
                      ,subtitle_text = "In the past 12 months, how often have you dealt with being told hurtful or offensive jokes/comments about your race?",
                      likert_factors = freq_factors_reverse, graph_orderby = "Never")
                      
#et
df_et <- fx_create_df(con, tables, "Racial Justice, Equity, And Inclusion", "et", "tot_freq_racial_justice") 
View(df_et)
fx_vis_smallmultiples(df = df_et, title_text = 'Over half of undocumented, unhoused, and LGBTQIA youth report poor quality health services get in the way of their best life at least some of the time'
                      , subtitle_text = "How often does poor quality health services get in the way of your living your best life?",
                      likert_factors = freq_factors_reverse, graph_orderby = "Never")

#co
df_co <- fx_create_df(con, tables, "Positive Identity And Self-Worth", "co", "tot_freq_positive_identity") 
View(df_co)
fx_vis_smallmultiples(df = df_co, title_text = 'All youth should feel hopeful about their future, but Asian and LGTBQIA+ youth are least likely to feel hopeful all the time'
                      , subtitle_text = "I feel hopeful when I think about my future",
                      likert_factors = true_factors, graph_orderby = "Always true")

#dz
df_dz <- fx_create_df(con, tables, "Cultural Identity", "dz", "tot_freq_cultural_identity") 
View(df_dz)
fx_vis_smallmultiples(df = df_dz, title_text = 'More than half of SWANA, AIAN, and female youth most often reported that their culture helps them feel good about who they are'
                      , subtitle_text = "My culture helps me feel good about who I am",
                      likert_factors = true_factors, graph_orderby = "Always true")

#cy
df_cy <- fx_create_df(con, tables, "Strong Minds", "cy", "tot_freq_strong_minds") 
View(df_cy)
fx_vis_smallmultiples(df = df_cy, title_text = 'LGBTQIA youth are least likely to feel valuable and report feeling worthless more than other groups'
                      ,subtitle_text = "About how often in the past 30 days, did you feel worthless?",
                      likert_factors = time_factors, graph_orderby = "None of the time")

#dm
df_dm <- fx_create_df(con, tables, "Positive Identity And Self-Worth", "dm", "tot_freq_positive_identity") 
View(df_dm)
fx_vis_smallmultiples(df = df_dm, title_text = 'More than two thirds of undocumented, unhoused, and systems impacted youth are least likely to find opportunities to connect and engage with their community'
                      , subtitle_text = "How true is it that there are ways for you to get involved with your community?",
                      likert_factors = true_factors, graph_orderby = "Always true")


# ####visualizing key findings for Youth Council presentation 10/19/24 (NOTE: Charts are archived since data has been updated since then)#### 
# 
# fx_single_barchart(question_number_i = '18', sub_question_i = 'Contact with the police', #these are the inputs, i stands for insert/input of the variable of interest we want to look at
#                    response_label_1 = 'Sometimes', response_label_2 = 'Most of the time', response_label_3 = 'All of the time', #insert the responses we want to capture data for, up to 3
#                    title_text = 'Contact with police is interfering with Black youth at a rate higher than most groups', x_axis_text = 'Rate of youth reporting contact with police is getting in the way of living their best life' #insert text on graph that is customized
# )  
# 
# fx_single_barchart(question_number_i = '16', sub_question_i = 'At school', #these are the inputs, i stands for insert/input of the variable of interest we want to look at
#                    response_label_1 = 'Sometimes', response_label_2 = 'Most of the time', response_label_3 = 'All of the time', #insert the responses we want to capture data for, up to 3
#                    title_text = 'Black youth experience racial discrimination at schools at a rate higher than any other group', x_axis_text = 'Rate of youth reported being treated unfairly due to their race at school' #insert text on graph that is customized
# )  
# 
# fx_single_barchart_freq(domain_pgname = 'safety', #name of domain as it is in the pgadmine table// for example, "strong_minds"
#                         variable_i = 'q19', #these are the inputs, i stands for insert/input of the variable of interest we want to look at
#                         title_text = 'Almost half (49.7%) of youth reported worrying about their own safety', x_axis_text = 'Rate of youth worried about their safety in their own neighborhood' #insert text on graph that is customized
# )
# 
# fx_single_barchart_freq(domain_pgname = 'caring_families', #name of domain as it is in the pgadmine table// for example, "strong_minds"
#                         variable_i = 'dk', #these are the inputs, i stands for insert/input of the variable of interest we want to look at
#                         title_text = 'Majority of youth have someone to share good news with', x_axis_text = '' #insert text on graph that is customized
# )
# 
# fx_single_barchart_freq(domain_pgname = 'caring_families', #name of domain as it is in the pgadmine table// for example, "strong_minds"
#                         variable_i = 'dl', #these are the inputs, i stands for insert/input of the variable of interest we want to look at
#                         title_text = 'Majority of youth have someone who will be there for them', x_axis_text = '' #insert text on graph that is customized
# )
# 
# fx_single_barchart(question_number_i = '9', sub_question_i = 'Nervous', #these are the inputs, i stands for insert/input of the variable of interest we want to look at
#                    response_label_1 = 'Some of the time', response_label_2 = 'Most of the time', response_label_3 = 'All of the time', #insert the responses we want to capture data for, up to 3
#                    title_text = 'Over 60% of Multiracial, Asian, and Latine youth feel nervous some of the time', x_axis_text = 'Rate of youth reporting feelings of nervousness in the past year' #insert text on graph that is customized
# )  
# 
# fx_single_barchart(question_number_i = '11', sub_question_i = 'I feel I have time to figure out what I like', #these are the inputs, i stands for insert/input of the variable of interest we want to look at
#                    response_label_1 = '', response_label_2 = '', response_label_3 = 'Always true', #insert the responses we want to capture data for, up to 3
#                    title_text = 'Over 30% of White and SWANA youth rated this statement as always true while less than 20% of Latine and AIAN youth rated this statement as always true', x_axis_text = 'Rate of youth reporting they have time to figure out what they like as always true' #insert text on graph that is customized
# )  
# 
# fx_single_barchart(question_number_i = '8', sub_question_i = 'I believe that I am capable in most things', #these are the inputs, i stands for insert/input of the variable of interest we want to look at
#                    response_label_1 = '', response_label_2 = '', response_label_3 = 'Always true', #insert the responses we want to capture data for, up to 3
#                    title_text = 'Over 40% of SWANA, NHPI, Black, and AIAN youth rated this statement as always true', x_axis_text = 'Rate of youth reporting they feel they are capable of most things as always true' #insert text on graph that is customized
# )  
# 
# 
# fx_single_barchart_freq(domain_pgname = 'cultural_identity', #name of domain as it is in the pgadmine table// for example, "strong_minds"
#                         variable_i = 'dz', #these are the inputs, i stands for insert/input of the variable of interest we want to look at
#                         title_text = 'Over half of youth report the importance of their cultural identity to feeling good about who they are', x_axis_text = '' #insert text on graph that is customized
# )
# 
# 
# fx_single_barchart_freq(domain_pgname = 'positive_identity', #name of domain as it is in the pgadmine table// for example, "strong_minds"
#                         variable_i = 'cn', #these are the inputs, i stands for insert/input of the variable of interest we want to look at
#                         title_text = 'About 75% of youth believe they are capable of most things often or always', x_axis_text = '' #insert text on graph that is customized
# )
# 
# 
####visualizing key findings for Community Council presentation 11/21/24 #### 

# #RAN
# fx_single_barchart(question_number_i = '18', sub_question_i = 'Contact with the police', #these are the inputs, i stands for insert/input of the variable of interest we want to look at
#                    response_label_1 = 'Sometimes', response_label_2 = 'Most of the time', response_label_3 = 'All of the time', #insert the responses we want to capture data for, up to 3
#                    title_text = 'Contact with police is interfering with NHPI and Black youth at a rate higher than most groups', x_axis_text = 'Rate of youth reporting contact with police is getting in the way of living their best life at least sometimes' #insert text on graph that is customized
# )  
# 
# #RAN
# fx_single_barchart_freq(domain_pgname = 'safety', #name of domain as it is in the pgadmine table// for example, "strong_minds"
#                         variable_i = 'q19', #these are the inputs, i stands for insert/input of the variable of interest we want to look at
#                         likert_type_i = freq_scale_rev ,
#                         title_text = 'Almost half (49.7%) of youth reported worrying about their own safety', x_axis_text = '' #insert text on graph that is customized
# )
# 
# #RAN
# fx_single_barchart_freq(domain_pgname = 'caring_families', #name of domain as it is in the pgadmine table// for example, "strong_minds"
#                         variable_i = 'dl', #these are the inputs, i stands for insert/input of the variable of interest we want to look at
#                         likert_type_i = true_scale ,
#                         title_text = 'Majority of youth have someone who will be there for them', x_axis_text = '' #insert text on graph that is customized
# )
# 
# #RAN
# fx_single_barchart(question_number_i = '9', sub_question_i = 'Nervous', #these are the inputs, i stands for insert/input of the variable of interest we want to look at
#                    response_label_1 = 'Some of the time', response_label_2 = 'Most of the time', response_label_3 = 'All of the time', #insert the responses we want to capture data for, up to 3
#                    title_text = 'Over 60% of Multiracial, Asian, and Latine youth feel nervous at least some of the time', x_axis_text = 'Rate of youth reporting feelings of nervousness in the past year' #insert text on graph that is customized
# )  
# 
# #RAN
# fx_single_barchart(question_number_i = '11', sub_question_i = 'I feel I have time to figure out what I like', #these are the inputs, i stands for insert/input of the variable of interest we want to look at
#                    response_label_1 = '', response_label_2 = '', response_label_3 = 'Always true', #insert the responses we want to capture data for, up to 3
#                    title_text = 'Over 30% of White and SWANA youth feel they have time to figure out what they like while less than 20% of Latine and AIAN youth rated this statement as always true', x_axis_text = 'Rate of youth reporting they have time to figure out what they like as always true' #insert text on graph that is customized
# )  
# 
# #RAN
# fx_single_barchart(question_number_i = '8', sub_question_i = 'I believe that I am capable in most things', #these are the inputs, i stands for insert/input of the variable of interest we want to look at
#                    response_label_1 = '', response_label_2 = '', response_label_3 = 'Always true', #insert the responses we want to capture data for, up to 3
#                    title_text = 'Over 40% of SWANA, NHPI, Black, and AIAN youth believe they are capable in most things', x_axis_text = 'Rate of youth reporting they feel they are capable of most things as always true' #insert text on graph that is customized
# )  
# 
# #RAN
# fx_single_barchart_freq(domain_pgname = 'cultural_identity', #name of domain as it is in the pgadmine table// for example, "strong_minds"
#                         variable_i = 'dz', #these are the inputs, i stands for insert/input of the variable of interest we want to look at
#                         likert_type_i = true_scale ,
#                         title_text = 'Over half of youth report the importance of their cultural identity to feeling good about who they are', x_axis_text = '' #insert text on graph that is customized
# )
# 
# #RAN
# fx_single_barchart_freq(domain_pgname = 'positive_identity', #name of domain as it is in the pgadmine table// for example, "strong_minds"
#                         variable_i = 'cn', #these are the inputs, i stands for insert/input of the variable of interest we want to look at
#                         likert_type_i = true_scale ,
#                         title_text = 'About 75% of youth believe they are capable of most things often or always', x_axis_text = '' #insert text on graph that is customized
# )
# 
# #RAN
# fx_single_barchart(question_number_i = '14', sub_question_i = 'paid and unpaid work experiences for me', #these are the inputs, i stands for insert/input of the variable of interest we want to look at
#                    response_label_1 = '', response_label_2 = '', response_label_3 = 'Yes', #insert the responses we want to capture data for, up to 3
#                    title_text = 'NHPI youth are least likely to report having access to work experiences than any other subgroup', x_axis_text = 'Rate of youth reporting they have access to paid and unpaid work experiences' #insert text on graph that is customized
# )
