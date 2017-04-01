##Text Mining/Sentiment Analysis
#Load necessary packages
library(tidyverse)
library(tidytext)
library(stringr)

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

#Repeat for all the reviews for this particular restaurant 
tidy_reviews <- text_single_rest %>% 
        select(restaurant, review_text, rating, review_date) %>% 
        unnest_tokens(word, review_text) %>% 
        filter(!word %in% stop_words$word, str_detect(word, "^[a-z']+$"))
tidy_reviews

#Visualize
tidy_reviews %>% 
        count(word, sort = TRUE) %>% 
        filter(n > 250) %>% 
        mutate(word = reorder(word, n)) %>% 
        ggplot(aes(word, n)) +
        geom_bar(stat = 'identity') +
        xlab(NULL) +
        coord_flip()

#Sentiment Analysis
AFINN <- sentiments %>% 
        filter(lexicon == "AFINN") %>% 
        select(word, afinn_score = score)

tidy_reviews_sentiment <- tidy_reviews %>% 
        inner_join(AFINN, by = "word") %>% 
        group_by(review_date, rating) %>% 
        summarize(sentiment = mean(afinn_score))
tidy_reviews_sentiment

#Visualize Sentiment Analysis
ggplot(tidy_reviews_sentiment, aes(rating, sentiment, group = rating)) + 
        geom_boxplot() +
        ylab("Average Sentiment Score")
