#Analyzing the Fieri Effect
library(tidyverse)
library(data.table)
library(plyr)

#setwd()

#read in csv file containing all Fieri flavor data
fieri <- read_csv("fieri_data_complete.csv")

#create columns to be used in for loop
fieri$after <- 0
fieri$before <- 0
fieri$effect <- 0

#split data by yelp_string
fieri_split <- split(fieri, fieri$yelp_string)

#ddply(x, ~group, summarise, mean = mean(rating))
d <- filter(fieri, index == 340)
View(d)

counter <- 1

for(df in 1:length(fieri_split)){
  for (i in df){
    
    df$after[counter] <- NA
    df$before[counter] <- NA
    if (i == "AFTER")
    {
      df$after[counter] <- df$rating[counter]
    }
  
    else if (i == "BEFORE")
    {
      df$before[counter] <- df$rating[counter]
    }
    
  counter <- counter + 1

  }
  df$effect <- (mean(df$after, na.rm = TRUE) - mean(df$before, na.rm = TRUE)) / mean(df$before, na.rm = TRUE)
  
}

length(fieri_split[[1]])
str(fieri_split[1])

fieri$Restaurant <- as.factor(fieri$Restaurant)
q <- fieri %>% 
  group_by(Restaurant) %>% 
  group_by(group) %>% 
  summarise(after = mean(group))

class(fieri$rating)
