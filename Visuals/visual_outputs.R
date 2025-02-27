# Code for running visuals for rates by question and rates by question and race

# source script
source('Visuals\\visual_functions.R')
source('W:\\Project\\OSI\\Bold Vision\\Youth Thriving Survey\\GitHub\\MK\\boldvision_youththriving\\Visuals\\visual_smallmultiples.R')

#### Visualizing for Youth Council Presentation 03/08/25 ####

df_dl <- fx_create_df(con, tables, "Caring Families And Relationships", "dl", "tot_freq_caring_families") 

View(df_dl)

fx_vis_smallmultiples(df = df_dl, title_text = 'Most LA County youth often or always feel they can go to someone with a problem'
                      ,likert_factors = true_factors, graph_orderby = "Always true"
                      )


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
