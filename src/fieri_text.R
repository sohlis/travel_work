##Text Mining/Sentiment Analysis
#Load necessary packages
library(tidyverse)
library(tidytext)
library(stringr)
data(stop_words)

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

#Create a per word summary
tidy_reviews_counted <- tidy_reviews %>% 
        count(review_date, restaurant, rating, word) %>% 
        ungroup()
tidy_reviews_counted

word_summaries <- tidy_reviews_counted %>%
        group_by(word) %>% 
        summarize(reviews = n(), uses = sum(n), average_stars = mean(rating)) %>% 
        ungroup()

word_summaries_filtered <- word_summaries %>% 
        filter(reviews >= 5)

#Look at most positive words and negative words
word_summaries_filtered %>% arrange((average_stars))
word_summaries_filtered %>% arrange(desc(average_stars))

#Visualize word summary
ggplot(word_summaries_filtered, aes(reviews, average_stars)) +
        geom_point() +
        geom_text(aes(label = word), check_overlap = TRUE, vjust = 1, hjust = 1) +
        scale_x_log10() +
        geom_hline(yintercept = mean(tidy_reviews$rating), color = "red", linetype = 2) +
        xlab("Number of Reviews") +
        ylab("Average Rating")

#Sentiment Analysis of Word Summary
words_afinn <- word_summaries_filtered %>% 
        inner_join(AFINN)

ggplot(words_afinn, aes(afinn_score, average_stars, group = afinn_score)) +
        geom_boxplot() +
        xlab("AFINN score of word") +
        ylab("Average rating of reviews with this word")

ggplot(words_afinn, aes(afinn_score, average_stars, size = reviews)) + 
        geom_smooth(method="lm", se=FALSE, show.legend=FALSE) +
        geom_text(aes(label = word, size = NULL), check_overlap = TRUE, vjust=1, hjust=1) +
        geom_point() +
        scale_x_continuous(limits = c(-6,6)) +
        xlab("AFINN sentiment score") +
        ylab("Average Yelp rating")

ggplot(words_afinn, aes(reviews, average_stars, color = afinn_score)) +
        geom_point() +
        geom_text(aes(label = word), check_overlap = TRUE, vjust = 1, hjust = 1) +
        scale_x_log10() +
        geom_hline(yintercept = mean(tidy_reviews$rating), color = "red", linetype = 2) +
        scale_colour_gradient2("AFINN", low = "red", mid = "white", high = "blue", limits = c(-5,5)) +
        xlab("# of reviews") +
        ylab("Average Rating")

##Repeat above process for all reviews in data set
tidy_fieri <- fieri_text %>% 
        select(restaurant, review_text, rating, review_date, ep_air_date, group, zip_code) %>% 
        unnest_tokens(word, review_text) %>% 
        filter(!word %in% stop_words$word, str_detect(word, "^[a-z']+$"))
tidy_fieri

tidy_fieri %>% count(word, sort = TRUE)

tidy_fieri %>% 
        count(word, sort = TRUE) %>% 
        filter(n > 40000) %>% 
        mutate(word = reorder(word, n)) %>% 
        ggplot(aes(word, n)) +
        geom_bar(stat = 'identity') +
        xlab(NULL) +
        coord_flip()

AFINN <- sentiments %>% 
        filter(lexicon == "AFINN") %>% 
        select(word, afinn_score = score)

tidy_fieri_sentiment <- tidy_fieri %>% 
        inner_join(AFINN, by = "word") %>% 
        group_by(restaurant, ep_air_date, review_date, rating) %>% 
        summarize(sentiment = mean(afinn_score))
tidy_fieri_sentiment

ggplot(tidy_fieri_sentiment, aes(rating, sentiment, group = rating)) + 
        geom_boxplot() +
        ylab("Average Sentiment Score")

tidy_fieri_counted <- tidy_fieri %>% 
        count(review_date, restaurant, rating, word) %>% 
        ungroup()
tidy_fieri_counted

tidy_fieri_summaries <- tidy_fieri_counted %>%
        group_by(word) %>% 
        summarize(reviews = n(), uses = sum(n), average_rating = mean(rating)) %>% 
        ungroup()

tidy_summaries_filtered <- tidy_fieri_summaries %>% 
        filter(reviews >= 50)

#Look at most positive words and negative words
tidy_summaries_filtered %>% arrange((average_rating))
tidy_summaries_filtered %>% arrange(desc(average_rating))

#Visualize word summary
ggplot(tidy_summaries_filtered, aes(reviews, average_rating)) +
        geom_point() +
        geom_text(aes(label = word), check_overlap = TRUE, vjust = 1, hjust = 1) +
        scale_x_log10() +
        geom_hline(yintercept = mean(fieri_text$rating), color = "red", linetype = 2) +
        xlab("Number of Reviews") +
        ylab("Average Rating")

#Sentiment Analysis of Word Summary
fieri_words_afinn <- tidy_summaries_filtered %>% 
        inner_join(AFINN)

ggplot(fieri_words_afinn, aes(afinn_score, average_rating, group = afinn_score)) +
        geom_boxplot() +
        xlab("AFINN score of word") +
        ylab("Average rating of reviews with this word")

ggplot(fieri_words_afinn, aes(afinn_score, average_rating, size = reviews)) + 
        geom_smooth(method="lm", se=FALSE, show.legend=FALSE) +
        geom_text(aes(label = word, size = NULL), check_overlap = TRUE, vjust=1, hjust=1) +
        geom_point() +
        scale_x_continuous(limits = c(-6,6)) +
        xlab("AFINN sentiment score") +
        ylab("Average Yelp rating")

ggplot(fieri_words_afinn, aes(reviews, average_rating, color = afinn_score)) +
        geom_point() +
        geom_text(aes(label = word), check_overlap = TRUE, vjust = 1, hjust = 1) +
        scale_x_log10() +
        geom_hline(yintercept = mean(fieri_text$rating), color = "red", linetype = 2) +
        scale_colour_gradient2("AFINN", low = "red", mid = "white", high = "blue", limits = c(-5,5)) +
        xlab("# of reviews") +
        ylab("Average Rating")
