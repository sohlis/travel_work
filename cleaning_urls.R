#Connect the data from the wiki scraper to the yelp urls. This is an intermediate step to eventually merging all the data together
#library(tidyverse)
#setwd()

#read in csv file with restaurant names and urls
names.url <- read_csv("names_with_url.csv")

colnames(names.url)[colnames(names.url) == "Scully's Tavern Miami Florida yelp"] <- "google_string"
colnames(names.url)[colnames(names.url) == "scullys-tavern-miami"] <- "yelp_string"

flavor.test <- left_join(flavor.town, names.url, by = "google_string")
?left_join()
View(flavor.test)
