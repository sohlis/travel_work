##Text Mining/Sentiment Analysis
#Load necessary packages
library(tidyverse)
library(tidytext)

#Read in data set generated from fieri_effect.R
fieri_text <- read_csv("fieri_mac_march.csv")

#Group data by variable splitting on so it's easier to read
fieri_text <- group_by(fieri_text, restaurant)

#Split the data by restaurants
f_text <- split(fieri_text, fieri_text$restaurant)

#Begin exploring first restaurant reviews
text_single_rest <- f_text[["10th Ave Burrito Co"]]

#Start with a single review
text_single_review <- text_single_rest$review_text[1]

#Tidy the review text
text_single_review <- data_frame(text = text_single_review)
tidy_review <- text_single_review %>% unnest_tokens(word, text)

#Remove stop words
data(stop_words)
tidy_review <- tidy_review %>% anti_join(stop_words)

#Look at most used words in the review
tidy_review %>% count(word, sort = TRUE)

#Visualize the word count
tidy_review %>% 
        count(word, sort = TRUE) %>% 
        filter(n > 1) %>% 
        mutate(word = reorder(word, n)) %>% 
        ggplot(aes(word, n)) + 
        geom_bar(stat = 'identity') +
        xlab(NULL) +
        coord_flip()

#Repeat word count visualization after aggregating all reviews for one restaurant
agg_text_single_rest <- select(text_single_rest, review_text)
agg_text_single_rest <- paste(agg_text_single_rest[, 1], collapse = "|")

tidy_agg_text_rest <- data_frame(text = agg_text_single_rest)
tidy_agg_review <- tidy_agg_text_rest %>% 
        unnest_tokens(word, text) %>% 
        anti_join(stop_words)

tidy_agg_review %>% 
        count(word, sort = TRUE) %>% 
        filter(n > 170) %>% 
        mutate(word = reorder(word, n)) %>% 
        ggplot(aes(word, n)) +
        geom_bar(stat = 'identity') +
        xlab(NULL) +
        coord_flip()



