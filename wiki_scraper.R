#Scraping list of restaurant names from DDD's wikipedia page
library(htmltab)
library(dplyr)
library(data.table)

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
index <- c(1:969)
flavor.town <- cbind(flavor.town, index)

#clean up the column names
colnames(flavor.town)[colnames(flavor.town) == "Episode"] <- "ep_number"
colnames(flavor.town)[colnames(flavor.town) == "Title"] <- "ep_title"
colnames(flavor.town)[colnames(flavor.town) == "Original Air Date"] <- "ep_air_date"
colnames(flavor.town)[colnames(flavor.town) == "data.for.url.scraper"] <- "yelp_string"
colnames(flavor.town)[colnames(flavor.town) == "Restaurant"] <- "restaurant"
colnames(flavor.town)[colnames(flavor.town) == "Location"] <- "location"

#convert episode air date to date class
flavor.town$ep_air_date <- as.Date(flavor.town$ep_air_date, format = "%B %d, %Y")

#remove NAs coerced due to difference in date format (, vs .)
flavor.town[is.na(flavor.town$ep_air_date), "ep_air_date"] <- "2009-02-09"

#convert yelp string to character variable
flavor.town$yelp_string <- as.character(flavor.town$yelp_string)

#Identify any restaurants Guy has hit multiple times
n.occur.string <- data.frame(table(flavor.town$yelp_string))
n.occur.restaurant <- data.frame(table(flavor.town$restaurant))

colnames(n.occur.string)[colnames(n.occur.string) == "Var1"] <- "yelp_string"
colnames(n.occur.string)[colnames(n.occur.string) == "Freq"] <- "freq_visited"

flavor.town <- left_join(flavor.town, n.occur.string, by = "yelp_string")

#Noting issue with multiple restaurants: There are some restaurants we can see that he has been to twice
#and one restaurant that he visited three times. However, some of restaurant names and yelp string names
#have slight misspellings or differences due to special characters. Thus, some restaurants he has visted
#potentially twice but we aren't capturing it in below commented out code. 

#write a csv file for all the data frame
write.csv(flavor.town, file = "flavor_town_final.csv", row.names = FALSE)
