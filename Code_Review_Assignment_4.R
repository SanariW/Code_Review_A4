#Assignment #4
#Shayli Babazadeh
library(tidyverse)

#' CODE REVIEW BY: SANARI WICKRAMARATNE

#First I am going to input the file from directory
input_file <- read.csv("ufo_subset.csv", header = TRUE)
#Then I am going to identify the rows that have a missing value for the shape column 
#I will put "unknown" instead
missing_shape <- is.na(input_file$shape)| input_file$shape == ""
input_file$shape[missing_shape] <- "unknown"

#Then I want to identify the rows that have a missing value for country and remove them
#I will create a new dataset 
if2 <- input_file[!is.na(input_file$country) & input_file$country != "", ]

#I will then convert Datetime and Date_posted columns into appropriate formats
if2$datetime <- lubridate::ymd_hm(if2$datetime)
if2$date_posted <- lubridate::ymd(gsub("(\\d{2})-(\\d{2})-(\\d{4})", "\\3-\\2-\\1", if2$date_posted))

#Then I am  going to classify the hoax comments 
#I have decided that if a comment line has # then it is a hoax comment 
#I then went through using an ifelse() statement and if a line had # in it then
#TRUE was stated in the new column named is_hoax, otherwise that column says FALSE

if2$is_hoax <- FALSE

for (i in 1:nrow(if2)) {
  if (grepl("HOAX", if2$comments[i]) | grepl("#", if2$comments[i])) {
    if2$is_hoax[i] <- TRUE
  }
}


#Then I am going to create a table reporting the percentage of hoax sightings per country
hoax_percentage <- round(prop.table(table(if2$country, if2$is_hoax), margin = 1) * 100, 0)
truedf <- as.data.frame(hoax_percentage)
subset_table <- truedf[truedf$Var2 == 'TRUE',c(1,3)]
colnames(subset_table) <- c("Country", "Percentage of hoax sightings")
subset_table

#Next I am going to add another column to the dataset (report_delay) and populate with the time difference in days, 
#between the date of the sighting and the date it was reported.
if2$report_delay <- round(difftime(if2$date_posted, if2$datetime, units = "days"),0)

#Next I am going to remove the rows where the sighting was reported before it happened.
#This will be done by picking only the rows that have a negative number for report_delay and exluding them from 
#the new dataset 
if3<- if2[!grepl("-", if2$report_delay),]

#Next I am going to create a table reporting the average report_delay per country.
# I used the aggregate() function will group the report delay values per country 
#and the function is specified to take the mean of the values for each country.
avg_delay <- aggregate(report_delay ~ country, data = if3, FUN = mean)
avg_delay

#Next I am going to check the data quality (missingness, format, range etc) of the "duration seconds" column. 
#Explain what kinds of problems you have identified and how you chose to deal with them, in your comments.
#I am going to check if the sum of the missing values for any of the rows is >0 and I will look at the data to asses
#if those entries should be omitted 
sum(is.na(if3$city)| if3$city == "")
sum(is.na(if3$state)| if3$state == "") 
#there are 419 entries that are missing the state, however since none of the city names are missing then it is acceptable
sum(is.na(if3$shape)| if3$shape == "") 
sum(is.na(if3$duration.hours.min)| if3$duration.hours.min == "") 
sum(is.na(if3$duration.seconds)| if3$duration.seconds == "") 


#For the duration.hours.min, the format is very confusing but I will change that according to the seconds row
#I don't think for this dataset it is necessary to have an hours column because most of the sightings are in shorter periods of time 
if3$duration.min <- round(if3$duration.seconds/60, 1)

#Here, I have reformatted the previous data into a dataset and removed the column duration.hours.min due to ambiguity 
if4 <-  if3[,c(1,2,3,4,5,6,14,8,9,10,11,12,13)]

#Next,I will create a histogram using the "duration seconds" column.
hist(log(if4$duration.seconds), xlab = "Duration (s)", main = "Duration of UFO Sightings", xlim = c(-2, 12))
hist


#' CODE REVIEW COMMENTS: 
#' 



