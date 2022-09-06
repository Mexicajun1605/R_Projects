#This project was done to illustrate what I have learned from 
#David Robinson and Julia Silge's book on text mining
#The text that I have chosen for this project is Alice in Wonderland

library(gutenbergr)
library(tidyverse)
library(tidytext)
library(readr)
library(wordcloud)
library(reshape2)
library(scales)

#Downloading Alice in Wonderland and Through the Looking Glass
#We will also be making a vector of the two works
alice <- gutenberg_download(c(11))
looking_glass <- gutenberg_download(c(12))
lewis_carroll <- gutenberg_download(c(11,12))

#Defining the stop words
data("stop_words")

#Tidying Alice, Looking Glass, and Lewis Carroll
#into a 2xn data frame by extracting all tokens
tidy_alice <- alice %>% 
  mutate(line_number = row_number()) %>%
  ungroup() %>%
  unnest_tokens(word, text) %>% 
  anti_join(stop_words)

tidy_glass <- looking_glass %>% 
  mutate(line_number = row_number()) %>% 
  ungroup() %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words)

tidy_carroll <- lewis_carroll %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words)
#Plotting out and filtering words that show up more than 50 times
alice_frequency <- tidy_alice %>% 
  count(word, sort = TRUE) %>% 
  filter(n > 50) %>% 
  ggplot(aes(y = word, x = n)) +
  geom_col(fill = "blue")

alice_frequency

tidy_glass %>% 
  count(word, sort = TRUE) %>% 
  filter(n >50) %>% 
  ggplot(aes(y = word, x = n))+
  geom_col(fill = "green")

tidy_carroll %>% 
  count(word, sort = TRUE) %>% 
  filter(n>100) %>% 
  ggplot(aes(y=word, x = n))+
  geom_col(fill = "red")

#Defining the sentiment we want to analyze
nrcjoy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")

#Searching for that sentiment
tidy_alice %>% 
  inner_join(nrcjoy) %>% 
  count(word, sort = TRUE)

tidy_glass %>% 
  inner_join(nrcjoy) %>% 
  count(word, sort = TRUE)
#Calculating the net sentiment using the Bing Lexicon
#This quantifies the sentiment of the book using a score of
#-5 to 5
alice_sentiment <- tidy_alice %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(index = line_number, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative)

alice_sentiment

ggplot(alice_sentiment, aes(index, sentiment)) + 
    geom_col(show.legend = FALSE, fill = "blue")

glass_sentiment <- tidy_glass %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(index = line_number, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative)

glass_sentiment

ggplot(glass_sentiment, aes(index, sentiment)) +
  geom_col(show.legend = FALSE, fill = "green")
#Binging Alice, we can arrive at scores of how positive/negative 
#Alice in Wonderland is
bing_alice <- tidy_alice %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort = TRUE) %>% 
  ungroup()

loobing_glass <- tidy_glass %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort = TRUE) %>% 
  ungroup()
#Graphing out the bing sentiment scores of the most common words

bing_alice %>% 
  group_by(sentiment) %>% 
  top_n(10) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  labs(y = "Sentiment Contribution", 
       x = NULL) +
  theme(axis.text = element_text(angle = 45))

loobing_glass %>% 
  group_by(sentiment) %>% 
  top_n(10) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n, fill = sentiment))+
  geom_col(show.legend = FALSE)
  labs(y = "Sentiment Contribution",
       x = NULL)+
  theme(axis.text = element_text(angle = 45))


#Alice in Wonderland Wordcloud
#Here we will be working with Word clouds to show frequency and sentiment
tidy_alice %>% 
  anti_join(stop_words) %>% 
  count(word) %>% 
  with(wordcloud(word, n, max.words = 100))

#Sentiment wordcloud
tidy_alice %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort = TRUE) %>% 
  acast(word ~ sentiment, value.var = "n", fill = 0) %>% 
  comparison.cloud(max.words = 100)
