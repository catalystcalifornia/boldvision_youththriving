#Importing Data to PGAMIN Database 


####Step 1: Set Up ####


packages <- c("dplyr", "RPostgreSQL", "usethis", "readxl", "janitor") 


for(pkg in packages){
  library(pkg, character.only = TRUE)
}

options(scipen = 100) # disable scientific notation

# create connection for bold_vision
source("W:\\RDA Team\\R\\credentials_source.R")
con_bv <- connect_to_db("bold_vision") 


####Step 2: Read in dataset and sent to database with comments ####
ys_data <- read_excel("W:\\Project\\OSI\\Bold Vision\\Youth Thriving Survey\\Data\\Survey responses\\Final Data\\BVYTSWeightSummary_Database.xlsx",sheet="Database")

ys_data <- clean_names(ys_data)

# check for NA columns
columns_na_counts <- lapply(ys_data, function(x) sum(is.na(x)))%>%
  as.data.frame()%>%
  pivot_longer(everything(), names_to="variable",values_to="na_count")

# exclude extra blank columns
ys_data <- ys_data %>%
  select(-c(x155,x159))

colnames(ys_data)[grepl('q12a_how_true_is_this_about_you',colnames(ys_data))] <- 'q12a'
colnames(ys_data)[grepl('q10b_which_of_the_following_',colnames(ys_data))] <- 'q10b'
colnames(ys_data)[grepl('q10a_how_many_adults_really_',colnames(ys_data))] <- 'q10a'


# dbWriteTable(con_bv, c('youth_thriving', 'raw_survey_data'), ys_data,
#                           overwrite = FALSE, row.names = FALSE)

# dbSendQuery(con_bv, "COMMENT ON TABLE youth_thriving.raw_survey_data IS 'The following dataset are responses from the Youth Thriving Survey conducted by Bold Vision in 2024. The data dictionary explaining each variable is here: youth_thriving.bvys_datadictionary_2024 . Steps explaining data cleaning can be found here: W:\\Project\\OSI\\Bold Vision\\Youth Thriving Survey\\Data\\Survey responses\\Final Data\\BVYTSPopulationWeighting_DataCleaning.pdf Original Dataset is here: a)	W:\\Project\\OSI\\Bold Vision\\Youth Thriving Survey\\Data\\Survey responses\\Final Data\\BVYTSWeightSummary_Database.xlsx The process for cleaning and uploading the data is explained in the QA Documentation here: W:\\Project\\OSI\\Bold Vision\\Youth Thriving Survey\\Documentation\\QA_dataimport_datadictionary.docx'")


####Step 3: Read in data dictionary and send to database comments ####

data_dictionary <- read_excel("W:\\Project\\OSI\\Bold Vision\\Youth Thriving Survey\\Data\\Survey responses\\bvys_datadictionary_2024.xlsx")

data_dictionary <- data_dictionary %>% 
  mutate(
    variable_name = str_to_title(variable_name),
    response_domain = str_to_title(response_domain)) %>%
  filter_all(any_vars(!is.na(.))) %>%
  mutate(
    primary_key = row_number()) %>%
  select(primary_key, everything())

# generalize variables in Caring Families and Relationships to align with conceptboard
data_dictionary$variable_name[data_dictionary$variable_name %in% c("Support: Adults, Family, Peers (Aligns With Youth Voice)","School Relationships")] <- "Relationships/Support"

# match Demographic to Demographics in response domains
data_dictionary$response_domain[data_dictionary$response_domain=="Demographic"] <- "Demographics"

# specify Info domain for org
data_dictionary$response_domain[data_dictionary$variable=="org"] <- "Info"

# do some other clean up for consistent labels
data_dictionary <- data_dictionary %>%
  mutate(
    variable_name=str_replace(variable_name," Nhpi "," NHPI "),
    variable_name=str_replace(variable_name,"Spa ","SPA "))


data_dictionary[data_dictionary=="Don't Wish to Answer"]<-"Don't wish to answer"
data_dictionary[data_dictionary=="Don't Wish to Answer"]<-"Don't wish to answer"
data_dictionary[data_dictionary=="Dont wish to answer"]<-"Don't wish to answer"
data_dictionary[data_dictionary=="Dont Wish to Answer"]<-"Don't wish to answer"
data_dictionary[data_dictionary=="I am not sure or Questioning"]<-"I am not sure or questioning"
data_dictionary[data_dictionary=="Dont know"]<-"Don't know"
data_dictionary[data_dictionary=="Don't Know"]<-"Don't know"
data_dictionary[data_dictionary=="Dont Know"]<-"Don't know"
data_dictionary[data_dictionary=="South West Asian and North African"]<-"Southwest Asian and North African"
data_dictionary[data_dictionary=="some of the time"]<-"Some of the time"
data_dictionary[data_dictionary=="most of the time"]<-"Most of the time"
data_dictionary[data_dictionary=="all of the time"]<-"All of the time"
data_dictionary[data_dictionary=="Never True"]<-"Never true"
data_dictionary[data_dictionary=="Sometimes True"]<-"Sometimes true"
data_dictionary[data_dictionary=="Often True"]<-"Often true"
data_dictionary[data_dictionary=="Always True"]<-"Always true"


# dbWriteTable(con_bv, c('youth_thriving', 'bvys_datadictionary_2024'), data_dictionary,
#              overwrite = FALSE, row.names = FALSE)

dbSendQuery(con_bv, "COMMENT ON TABLE youth_thriving.bvys_datadictionary_2024 IS 'The following data dictionary aims to decode the data for the Bold Visin Youth Thriving Survey Data for 2024. QA Documentation here: W:\\Project\\OSI\\Bold Vision\\Youth Thriving Survey\\Documentation\\QA_dataimport_datadictionary.docx'")


dbSendQuery(con_bv, "COMMENT ON COLUMN youth_thriving.bvys_datadictionary_2024.variable IS 'refers to the column label or variable in the survey data';
                     COMMENT ON COLUMN youth_thriving.bvys_datadictionary_2024.question IS 'the question that this variable refers to';
                     COMMENT ON COLUMN youth_thriving.bvys_datadictionary_2024.sub_question IS 'the subquestion that this variable refers to';
                     COMMENT ON COLUMN youth_thriving.bvys_datadictionary_2024.variable_category IS 'the categories the data collector identified in their codebook';
                     COMMENT ON COLUMN youth_thriving.bvys_datadictionary_2024.variable_name IS 'a more explanatory name of what the variable aims to measure';
                     COMMENT ON COLUMN youth_thriving.bvys_datadictionary_2024.response_domain IS 'the survey domain this variable refers to';
                     COMMENT ON COLUMN youth_thriving.bvys_datadictionary_2024.response_type IS 'the type of question this is so oe: open ended, mc: multiple choice, tf: true or false, fun: fun questions used to break up survey';
                     COMMENT ON COLUMN youth_thriving.bvys_datadictionary_2024.response_1 IS 'what the response is if the variable has a 1 coded for the data. True and False questions will also show an actual response too. So there is no 0 which is just not true but for 1, we coded what is true for the respondent';
                     COMMENT ON COLUMN youth_thriving.bvys_datadictionary_2024.response_2 IS 'what the response is if the variable has a 2 coded for the data.';
                     COMMENT ON COLUMN youth_thriving.bvys_datadictionary_2024.response_3 IS 'what the response is if the variable has a 3 coded for the data.';
                     COMMENT ON COLUMN youth_thriving.bvys_datadictionary_2024.response_4 IS 'what the response is if the variable has a 4 coded for the data.';
                     COMMENT ON COLUMN youth_thriving.bvys_datadictionary_2024.response_5 IS 'what the response is if the variable has a 5 coded for the data.';
                     COMMENT ON COLUMN youth_thriving.bvys_datadictionary_2024.response_6 IS 'what the response is if the variable has a 6 coded for the data.';
                     COMMENT ON COLUMN youth_thriving.bvys_datadictionary_2024.response_7 IS 'what the response is if the variable has a 7 coded for the data.';
                     COMMENT ON COLUMN youth_thriving.bvys_datadictionary_2024.response_8 IS 'what the response is if the variable has a 8 coded for the data.';
                     COMMENT ON COLUMN youth_thriving.bvys_datadictionary_2024.response_9 IS 'what the response is if the variable has a 9 coded for the data.';
                     COMMENT ON COLUMN youth_thriving.bvys_datadictionary_2024.response_10 IS 'what the response is if the variable has a 10 coded for the data.';
                     COMMENT ON COLUMN youth_thriving.bvys_datadictionary_2024.response_11 IS 'what the response is if the variable has a 11 coded for the data.';
                     COMMENT ON COLUMN youth_thriving.bvys_datadictionary_2024.response_12 IS 'what the response is if the variable has a 12 coded for the data.';
                     COMMENT ON COLUMN youth_thriving.bvys_datadictionary_2024.notes IS 'any important notes like what variables were just for the weights and whether skip logic was applied';
                     COMMENT ON COLUMN youth_thriving.bvys_datadictionary_2024.likert IS 'notes if a Likert scale was used and how many points/options given (e.g., 5pt_scale, 4pt_scale, etc.)';
            ")

# add primary key for editing in database
dbSendQuery(con_bv, "ALTER TABLE youth_thriving.bvys_datadictionary_2024 ADD PRIMARY KEY (primary_key)");

dbDisconnect(con_bv)
