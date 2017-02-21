#Analyzing the Fieri Effect
library(tidyverse)
library(data.table)
library(plyr)

#setwd()

#read in csv file containing all Fieri flavor data
fieri <- read_csv("fieri_data_complete.csv")

#create before and after group
fieri$group <- ifelse(fieri$review_date <= fieri$ep_air_date, "BEFORE", "AFTER")

#group by reviews before air date and take mean
before_test <- fieri %>% 
  filter(group == "BEFORE") %>% 
  group_by(index) %>%
  summarise_each(funs(mean), rating)

colnames(before_test)[colnames(before_test) == "rating"] <- "avg_rating_before"

#group by reviews after air date and take mean
after_test <- fieri %>% 
  filter(group == "AFTER") %>% 
  group_by(index) %>%
  summarise_each(funs(mean), rating)  
  
colnames(after_test)[colnames(after_test) == "rating"] <- "avg_rating_after"

#join before and after sets together
effect_test <- left_join(before_test, after_test, by = "index")
View(effect_test)
