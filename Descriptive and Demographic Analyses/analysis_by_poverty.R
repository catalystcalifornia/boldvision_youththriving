# Calculate frequency tables for each item based on ZIP Code area poverty

# Step 1: Set up ------------

# Pull frequency table function from github script ####
source("Descriptive and Demographic Analyses/freq_table_function.R")

pov_data <- dbGetQuery(con, "SELECT * FROM youth_thriving.poverty_rate_data")

svy_data <- svy_data %>%
  left_join(pov_data %>% select(response_id,perc_below_200_fpl))

# Step 2: Recode poverty variable to be 0/1  ------------
# we're using the median cutoff determined in Descriptive and Demographic Analyses/getting_demographics.Rmd
# function needs a 0/1 variable to work
svy_data <- svy_data %>%
  mutate(area_poverty_above_county_median=
           case_when(
             perc_below_200_fpl<26 ~ 0, # below county median area poverty for 200% FPL
             perc_below_200_fpl>=26 ~ 1, # above county median area poverty for 200% FPL
             TRUE ~ NA))

# check results
table(svy_data$area_poverty_above_county_median,useNA='always')

sum(is.na(svy_data$perc_below_200_fpl)) # same number of NA

nrow(svy_data%>%filter(perc_below_200_fpl>=26)) # same number above median

# Step 3: Run frequency function ------
df_merged_per_area_poverty_above_county_median <- fx_freq_table("area_poverty_above_county_median")

# rename values in column for easier reading
df_merged_per_area_poverty_above_county_median <- df_merged_per_area_poverty_above_county_median %>%
  mutate(area_poverty_above_county_median=
           case_when(
             area_poverty_above_county_median=='area_poverty_above_county_median' ~ 'area poverty above county median',
             area_poverty_above_county_median=='not area_poverty_above_county_median' ~ 'area poverty not above county median',
             TRUE ~ NA))


# Step 4: Push table to postgres -----
table_name <- "response_analysis_per_area_poverty"
df <- df_merged_per_area_poverty_above_county_median

# Write data to database
dbWriteTable(con, c('youth_thriving', table_name), df,
             overwrite = FALSE, row.names = FALSE)

# Create table comment
table_comment <- paste0(
  "The following is a table of response frequencies and rates for non-demographic questions grouped by area poverty based on 200% FPL rate in ZIP Code above/below county median of about 26%. 
  The denominator for each stat is the total number of youth who 
    answered the question grouped by area poverty above or below county median. For example, looking at 
    youth in high poverty areas, count represents how many youth in high poverty areas selected response X to a given question. 
    Rate represents what % of youth in high poverty areas selected response X out of the total number of youth in high poverty areas 
    who answered the question.",
  "W:\\Project\\OSI\\Bold Vision\\Youth Thriving Survey\\Documentation\\QA_poverty_frequencies.docx",
  " Created on ", Sys.Date()
)

dbSendQuery(con, paste0(
  "COMMENT ON TABLE youth_thriving.", table_name, " IS '", table_comment, "';"
))

# Add comments to columns
demographic_variable <- ' high area poverty '

dbSendQuery(con, paste0("
    COMMENT ON COLUMN youth_thriving.", table_name, ".variable IS 
      'refers to the column label or variable in the survey data';
    COMMENT ON COLUMN youth_thriving.", table_name, ".question IS 
      'the question that this variable refers to';
    COMMENT ON COLUMN youth_thriving.", table_name, ".sub_question IS 
      'the subquestion that this variable refers to';
    COMMENT ON COLUMN youth_thriving.", table_name, ".variable_name IS 
      'the survey SUBcomponent this variable falls under';
    COMMENT ON COLUMN youth_thriving.", table_name, ".response_domain IS 
      'the survey component this variable falls under';
    COMMENT ON COLUMN youth_thriving.", table_name, ".area_poverty_above_county_median IS 
      '", demographic_variable, " youth';
    COMMENT ON COLUMN youth_thriving.", table_name, ".response IS 
      'the response that the data is about';
    COMMENT ON COLUMN youth_thriving.", table_name, ".count IS 
      'the count of ", demographic_variable, " youth that selected this response';
    COMMENT ON COLUMN youth_thriving.", table_name, ".rate IS 
      'the weighted % of", demographic_variable, " youth who selected this response out of the total number of ", demographic_variable, " youth who answered this question';
    COMMENT ON COLUMN youth_thriving.", table_name, ".rate_cv IS 
      'a weighted coefficient of variation for this rate';
    COMMENT ON COLUMN youth_thriving.", table_name, ".moe IS 
      'a weighted margin of error for this rate';
  "))

dbDisconnect(con)