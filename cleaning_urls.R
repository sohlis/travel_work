#Connect the data from the wiki scraper to the yelp urls. This is an intermediate step to eventually merging all the data together
library(tidyverse)
library(data.table)
#setwd()

#read in csv file with restaurant names and urls
names.url <- read_csv("names_with_url.csv")

colnames(names.url)[colnames(names.url) == "Scully's Tavern Miami Florida yelp"] <- "google_string"
colnames(names.url)[colnames(names.url) == "scullys-tavern-miami"] <- "yelp_string"

#needing cleaning 
names.url[585, 2] <- "worth-our-weight-santa-rosa"
names.url[691, 2] <- "bun-n-barrel-san-antonio-2"
names.url[749, 2] <- "falconettis-east-side-grill-vancouver"
names.url[813, 2] <- "gold-n-silver-inn-reno"
names.url[831, 2] <- "the-thumb-scottsdale"
names.url[198, 2] <- NA 

#create new merged data set
ftown <- left_join(flavor.town, names.url, by = "google_string")

#write new data set to file, begin preparing final merge 
#write.csv(ftown, file = "all_data_sans_reviews.csv", row.names = FALSE)

#read in review data
reviews <- read_csv("merged.csv")

#get rid of empty columns from import
reviews <- reviews[, 1:4]

#add in another column of all the reviews so that after I merge the data frames, I still have a variable to split the data frame by
reviews$yelp_string <- reviews$`scullys-tavern-miami`

#rename variables
colnames(reviews)[colnames(reviews) == "scullys-tavern-miami"] <- "Restaurant"
colnames(reviews)[colnames(reviews) == "10/14/16"] <- "review_date"
colnames(reviews)[colnames(reviews) == "5.0 star rating"] <- "rating"
colnames(reviews)[colnames(reviews) == "Finally made it \\\\xa0in to Scully's last night. \\\\xa0They play live Jazz on Thursday nights. \\\\xa0The Band: \\\\xa0Great 6 or 7 member ensemble playing jazzy favorites all night long. \\\\xa0We caught 2 sets and managed to dance a bit before we had to go home. \\\\xa0Definitely worth the trip, even if for the music alone.The Food: \\\\xa0Ordered wings and smoked fish dip. \\\\xa0Both were very good. \\\\xa0Menu has a great sampling of items. \\\\xa0Our friends had a seared tuna salad and a burger that also looked very good.The Drinks: \\\\xa0I drank Glenlevit neat all night. \\\\xa0You can't do anything to screw that up. \\\\xa0My wife had wine, OK selection considering where you are.The Service: \\\\xa0Excellent all the way around. \\\\xa0Our waitress, forgot to get her name, was great!!!! \\\\xa0She was very accommodating and looked in on some frequently. \\\\xa0 Bar and kitchen staff wee quick with orders also.Overall, I really liked this place alot. \\\\xa0I'm just sorry it took me nearly 30 years to make it there."] <- "review_text"

#clean rating strings
reviews$rating <- gsub(" star rating", "", reviews$rating)
reviews$rating <- sub("[[:space:]]+$", "", reviews$rating) 
reviews$rating <- as.numeric(reviews$rating)

#clean dates
reviews$review_date <- as.Date(reviews$review_date, format = "%m/%d/%y")

#Merge ftown data set with reviews data set
fieri <- merge(reviews, ftown, by = "yelp_string")

#write csv file containing final fieri data set
<<<<<<< HEAD
#write.csv(fieri, "fieri_data_complete.csv", row.names = FALSE)
=======
#write.csv("fieri_data_complete.csv", row.names = FALSE)
>>>>>>> 4b9bcac9db2092af0c4f6af052a7fccf4fdd6f34
