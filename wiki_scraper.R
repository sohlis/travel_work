#Scraping list of restaurant names from DDD's wikipedia page
library(htmltab)
library(dplyr)
library(data.table)
library(tidyverse)

url <- "https://en.wikipedia.org/wiki/List_of_Diners,_Drive-Ins,_and_Dives_episodes"

#Need to find way to condense this code to one line
season1 <- htmltab(doc = url, which = 1)
season2 <- htmltab(doc = url, which = 2)
season3 <- htmltab(doc = url, which = 3)
season4 <- htmltab(doc = url, which = 4)
season5 <- htmltab(doc = url, which = 5)
season6 <- htmltab(doc = url, which = 6)
season7 <- htmltab(doc = url, which = 7)
season8 <- htmltab(doc = url, which = 8)
season9 <- htmltab(doc = url, which = 9)
season10 <- htmltab(doc = url, which = 10)
season11 <- htmltab(doc = url, which = 11)
season12 <- htmltab(doc = url, which = 12)
season13 <- htmltab(doc = url, which = 13)
season14 <- htmltab(doc = url, which = 14)

#correcting error with season 15
season15 <- htmltab(doc = url, which = 15)
s15.ep1 <- "August 5, 2012"
s15.ep2 <- "August 13, 2012"
s15.ep3 <-  "August 19, 2012"
s15.ep4 <- "August 27, 2012"
s15.ep5 <- "September 3, 2012"
s15.ep6 <- "September 24, 2012"
s15.ep7 <- "October 1, 2012"
s15.ep8 <- "October 8, 2012"
s15.ep9 <- "October 15, 2012"
s15.ep10 <- "October 22, 2012"
s15.ep11 <- "October 29, 2012"
s15.ep12 <- "November 5, 2012"
s15.ep13 <- "November 12, 2012"

season15$`Original Air Date` <- NA
season15$`Original Air Date`[1:3] <- s15.ep1
season15$`Original Air Date`[4:5] <- s15.ep2
season15$`Original Air Date`[6:7] <- s15.ep3
season15$`Original Air Date`[8:9] <- s15.ep4
season15$`Original Air Date`[10:11] <- s15.ep5
season15$`Original Air Date`[12:15] <- s15.ep6
season15$`Original Air Date`[16:18] <- s15.ep7
season15$`Original Air Date`[19:21] <- s15.ep8
season15$`Original Air Date`[22:23] <- s15.ep9
season15$`Original Air Date`[24:26] <- s15.ep10
season15$`Original Air Date`[27:29] <- s15.ep11
season15$`Original Air Date`[30:32] <- s15.ep12
season15$`Original Air Date`[33:35] <- s15.ep13


season16 <- htmltab(doc = url, which = 16)
season17 <- htmltab(doc = url, which = 17)
season18 <- htmltab(doc = url, which = 18)
season19 <- htmltab(doc = url, which = 19)
season20 <- htmltab(doc = url, which = 20)
season21 <- htmltab(doc = url, which = 21)
season22 <- htmltab(doc = url, which = 22)
season23 <- htmltab(doc = url, which = 23)
season24 <- htmltab(doc = url, which = 24)

#combine all seasons into single data frame
flavor.town <- rbind(season1, season2, season3, season4, season5, season6, season7, season8,
                     season9, season10, season11, season12, season13, season14, season15,
                     season16, season17, season18, season19, season20, season21, season22,
                     season23, season24)

flavor.town <- as.data.frame(flavor.town, header = TRUE)
flavor.town$Episode <- as.numeric(flavor.town$Episode)

#remove NAs in restaurant column
flavor.town <- flavor.town[!(is.na(flavor.town$Restaurant)), ]

##Create a dataframe that combines the strings for restaurants and location to make a list
#that will be passed to a scraper to find all the necessary URLs for Yelp Scraper

combined.info <- flavor.town %>% select(Restaurant, Location)
combined.info <- na.omit(combined.info)

combined.info$Location <- gsub(',', "", combined.info$Location)

combined.info$rest.loc <- paste(combined.info$Restaurant, combined.info$Location, sep = " ")
combined.info$rest.loc <- paste(combined.info$rest.loc, "yelp", sep = " ")

#create a new dataframe that only contains the information needed to pass to the scraper
data.for.url.scraper <- as.character(combined.info$rest.loc)
data.for.url.scraper <- as.data.frame(data.for.url.scraper)

#merge the two data sets to maintain all the data in one dataframe
flavor.town <- cbind(flavor.town, data.for.url.scraper)

#create an index variable for the data frame
#index <- c(1:880)
#flavor.town <- cbind(flavor.town, index)

#clean up the column names
colnames(flavor.town)[colnames(flavor.town) == "Episode"] <- "ep_number"
colnames(flavor.town)[colnames(flavor.town) == "Title"] <- "ep_title"
colnames(flavor.town)[colnames(flavor.town) == "Original Air Date"] <- "ep_air_date"
colnames(flavor.town)[colnames(flavor.town) == "data.for.url.scraper"] <- "google_string"
colnames(flavor.town)[colnames(flavor.town) == "Restaurant"] <- "restaurant"
colnames(flavor.town)[colnames(flavor.town) == "Location"] <- "location"

#convert episode air date to date class
flavor.town$ep_air_date <- as.Date(flavor.town$ep_air_date, format = "%B %d, %Y")

#remove NAs coerced due to difference in date format (, vs .)
flavor.town[is.na(flavor.town$ep_air_date), "ep_air_date"] <- "2009-02-09"

#convert yelp string to character variable
flavor.town$google_string <- as.character(flavor.town$google_string)

#Identify any restaurants Guy has hit multiple times
n.occur.string <- data.frame(table(flavor.town$google_string))
n.occur.restaurant <- data.frame(table(flavor.town$restaurant))

colnames(n.occur.string)[colnames(n.occur.string) == "Var1"] <- "google_string"
colnames(n.occur.string)[colnames(n.occur.string) == "Freq"] <- "freq_visited"

flavor.town <- left_join(flavor.town, n.occur.string, by = "google_string")

#Fix troublesome terms in google_string and restuarant columns
flavor.town$google_string <- gsub("Ã©", "e", flavor.town$google_string)
flavor.town$google_string <- gsub("â€™", "'", flavor.town$google_string)
flavor.town$google_string <- gsub("â€“", "", flavor.town$google_string)
flavor.town$google_string <- gsub("â€˜", "'", flavor.town$google_string)

flavor.town$restaurant <- gsub("Ã©", "e", flavor.town$restaurant)
flavor.town$restaurant <- gsub("â€™", "'", flavor.town$restaurant)
flavor.town$restaurant <- gsub("â€“", "", flavor.town$restaurant)
flavor.town$restaurant <- gsub("â€˜", "'", flavor.town$restaurant)


#Noting issue with multiple restaurants: There are some restaurants we can see that he has been to twice
#and one restaurant that he visited three times. However, some of restaurant names and yelp string names
#have slight misspellings or differences due to special characters. Thus, some restaurants he has visted
#potentially twice but we aren't capturing it in below commented out code. 

#remove duplicates to make sure the merge works correctly 
flavor.town <- flavor.town[!duplicated(flavor.town$google_string), ]

#create an index variable for the data frame
index <- c(1:879)
flavor.town <- cbind(flavor.town, index)

#write a csv file for all the data frame
#write.csv(flavor.town, file = "flavor_town_final.csv", row.names = FALSE)

#####Connect the data from the wiki scraper to the yelp urls. This is an intermediate step to eventually merging all the data together
#####Create final data frame that houses all data from DDD


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

#create before and after group
fieri$group <- ifelse(fieri$review_date <= fieri$ep_air_date, "BEFORE", "AFTER")

#write csv file containing final fieri data set
#write.csv(fieri, "fieri_data_complete.csv", row.names = FALSE)

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

#calculate change in means before and after episode air date
effect_test <- effect_test %>% 
  mutate(score_change = avg_rating_after - avg_rating_before) %>% 
  mutate(pct_score_change = (score_change / avg_rating_before) * 100)



