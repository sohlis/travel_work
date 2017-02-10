#practice, prepping data for sql
library(tidyverse)
setwd("~/Downloads")

#read in csv file
df <- read_csv("merged.csv")

#parse out relevant info for analysis
df_reviews <- df[, -(4)]
df_reviews <- df_reviews[, 1:3]

#rename columns
colnames(df_reviews)[colnames(df_reviews) == "scullys-tavern-miami"] <- "Restaurant"
colnames(df_reviews)[colnames(df_reviews) == "10/14/16"] <- "Date"
colnames(df_reviews)[colnames(df_reviews) == "5.0 star rating"] <- "Rating"

#clean rating strings
df_reviews$Rating <- gsub(" star rating", "", df_reviews$Rating)
df_reviews$Rating <- sub("[[:space:]]+$", "", df_reviews$Rating) 
df_reviews$Rating <- as.numeric(df_reviews$Rating)

#clean dates
df_reviews$Date <- as.Date(df_reviews$Date, format = "%m/%d/%y") 

write_csv(df_reviews, "sql_practice_file.csv")
