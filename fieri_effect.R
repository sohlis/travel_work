#Scraping list of restaurant names from DDD's wikipedia page
# library(htmltab)
# library(data.table)
# library(tidyverse) #can't figure out why these fail
library(dplyr)
library(readr)
library(ggplot2) #these work for continuing

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
index <- c(1:902) # for mac ?
#index <- c(1:879) # for windows ?
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

#break out data frame into groups of individual restaurants, before and after ep air date
grouped <- group_by(fieri, index, group)
fieri_grouped <- summarise(grouped, mean = mean(rating), sd = sd(rating), n = n())

#merge data grouped data with main data set to add group means and sd
fieri <- merge(fieri, fieri_grouped, by = c("index", "group"))

#Do some feature engineering to break out city and state variables
fieri <- separate(fieri, location, into = c("city", "state"), sep = ",", remove = FALSE)

#use state variables to create new variables for U.S. regions
fieri$region <- "Other"
fieri[fieri$state %in% c("Connecticut", "Maine", "Massachusetts", "New Hampshire", "Rhode Island", "Vermont"), "region"] <- "Northeast"
fieri[fieri$state %in% c("New Jersey", "New York", "Pennsylvania"), "region"] <- "Mid-Atlantic"
fieri[fieri$state %in% c("Illinois", "Indiana", "Michigan", "Ohio", "region"), "region"] <- "East North Central" 
fieri[fieri$state %in% c("Iowa", "Kansas", "Minnesota", "Missouri", "Nebraska", "North Dakota", "South Dakota"), "region"] <- "West North Central"
fieri[fieri$state %in% c("Delaware", "Florida", "Georgia", "Maryland", "North Carolina", "South Carolina", "Virginia", "DC", "West Virginia"), "region"] <- "South Atlantic"
fieri[fieri$state %in% c("Alabama", "Kentucky", "Mississippi", "Tennessee"), "region"] <- "East South Central"
fieri[fieri$state %in% c("Arkansas", "Louisiana", "Oklahoma", "Texas"), "region"] <- "West South Central"
fieri[fieri$state %in% c("Arizona", "Colorado", "Idaho", "Montana", "Nevada", "New Mexico", "Utah", "Wyoming"), "region"] <- "Mountain"
fieri[fieri$state %in% c("Alaska", "California", "Hawaii", "Oregon", "Washington"), "region"] <- "Pacific"

#note that the states below are missing from Data Set:
#Arkansas, Delaware, Montana, North Dakota, South Dakota, Vermont
#Foreign locations included: Ontario, Mexico, Italy, London, British Columbia, UK

#need to create new variable for each year to further breakout trend of the effect
fieri$ep_year <- format(fieri$ep_air_date, '%Y')
fieri$review_year <- format(fieri$review_date, '%Y')


#write_csv(fieri, "fieri_mac_march.csv")
#fieri <- read_csv("fieri_mac_march.csv")

#write_csv(fieri, "fieri_march.csv")
#fieri <- read_csv("fieri_march.csv")

#create a new data frame of summary statistics
fieri$group <- as.factor(fieri$group)

fieri_summary <- fieri %>% 
        group_by(index, group) %>% 
        summarise(rating_mean = mean(rating), rating_sd = sd(rating), rating_n = n())
        
#add episode air date data to summary df
to_be_merged <- fieri %>% select(index, ep_air_date)
to_be_merged <- to_be_merged[!duplicated(to_be_merged), ]

fieri_summary <- merge(fieri_summary, to_be_merged, by = "index")

#create a subset of the summary data that only includes restaurants which have reviews before AND after episode air date
fieri_summary_duplicate <- fieri_summary[duplicated(fieri_summary$index), ]
fieri_summary_duplicate <- fieri_summary_duplicate$index

fieri_before_after <- fieri_summary[fieri_summary$index %in% fieri_summary_duplicate, ]

#create separate data sets for before observations and after observations, then merge later for simplicity
fieri_before_obs <- filter(fieri_before_after, group == "BEFORE")
fieri_after_obs <- filter(fieri_before_after, group == "AFTER")

fieri_before_obs <- fieri_before_obs %>% 
        rename(avg_before_obs_rating = rating_mean,
               sd_before_obs = rating_sd,
                n_before_obs = rating_n,
               before_group = group)

fieri_after_obs <- fieri_after_obs %>% 
        rename(avg_after_obs_rating = rating_mean,
               sd_after_obs = rating_sd,
               n_after_obs = rating_n,
               after_group = group)

fieri_paired <- left_join(fieri_before_obs, fieri_after_obs, by = "index")

#create values for differences betwen average values
fieri_paired <- fieri_paired %>% 
        mutate(avg_rating_change = avg_after_obs_rating - avg_before_obs_rating,
               avg_rating_change_pct = ((avg_after_obs_rating - avg_before_obs_rating) / avg_before_obs_rating) * 100,
               nafter_sub_nbefore = n_before_obs - n_after_obs,
               nafter_sub_nbefore_pct = ((n_before_obs - n_after_obs) / n_before_obs) * 100)

##Now that all the information needed for begining visual analysis is in one data frame, start visualizing data
#and finding relevant thresholds to use for sample size

#for convenience create new data frame with shorter name to work with "df"
df <- fieri_paired

#run a t-test to see if there is statistically significant differences between means
t.test(df$avg_before_obs_rating, df$avg_after_obs_rating, mu = 0)

#Histograms of before and after group observation frequencies 
hist(df$n_before_obs)
hist(df$n_after_obs)

qplot(index, n_before_obs, data = df)
qplot(index, n_after_obs, data = df)
qplot(n_before_obs, n_after_obs, data = df)

hist(df$nafter_sub_nbefore)
hist(df$nafter_sub_nbefore_pct)

summary(df$n_before_obs)
summary(df$n_after_obs)
summary(df$nafter_sub_nbefore)

#create a new data frame that only contains restaurants that have at least 30 reviews before and after episode air date
df30 <- filter(df, n_before_obs > 30 & n_after_obs > 30)

qplot(n_before_obs, n_after_obs, data = df30)
qplot(index, nafter_sub_nbefore, data = df30)

summary(df30$nafter_sub_nbefore)

#the restaurants featured earliest have the largest discrepency in sample size between groups so I 
#will make an adjustment to keep the data set more balanced. I will remove restaurants where the difference
#between observations after the episode air date and before epsiode air date is less than -2000. This
#will remove some of the restaurants that were reviewed earliest (potential introducing recency bias in results)

df_cleaned <- filter(df30, nafter_sub_nbefore >= -2000)
qplot(index, nafter_sub_nbefore, data = df_cleaned)

#t test for sigfnificant difference in means with this trimmed data set
t.test(df_cleaned$avg_before_obs_rating, df_cleaned$avg_after_obs_rating, mu = 0)

#merge some additional features/variables for visualization
location_restaurant <- select(fieri, index, restaurant, city, state, region, ep_year)
indexes_to_merge <- select(df_cleaned, index)
location_restaurant <- unique(location_restaurant)
location_restaurant <- location_restaurant[location_restaurant$index %in% indexes_to_merge$index, ]

df_cleaned <- merge(df_cleaned, location_restaurant, by = "index")

#initial scatter plot looking at overall trend, restaurant vs. fieri effect
gg_fieri <- ggplot(data = df_cleaned, aes(x = ep_air_date.x, y = avg_rating_change)) + 
        geom_point() +
        stat_smooth(method = "lm") +
        geom_hline(aes(yintercept = 0)) +
        geom_hline(aes(yintercept = mean(avg_rating_change)))
gg_fieri

#same visualization but in percentages terms
gg_fieri_pct <- ggplot(data = df_cleaned, aes(x = ep_air_date.x, y = avg_rating_change_pct)) + 
        geom_point() +
        stat_smooth(method = "lm") +
        geom_hline(aes(yintercept = 0)) +
        geom_hline(aes(yintercept = mean(avg_rating_change_pct)))
gg_fieri_pct

#decompose effect by region and state
gg_fieri_state <- ggplot(data = df_cleaned, aes(x = ep_air_date.x, y = avg_rating_change_pct)) + 
        geom_point() +
        facet_grid(. ~ region)
gg_fieri_state        

#breakout effect by year
gg_fieri_year <- ggplot(data = df_cleaned, aes(x = ep_air_date.x, y = avg_rating_change_pct)) + 
  geom_point() +
  facet_grid(. ~ ep_year)
gg_fieri_year   

