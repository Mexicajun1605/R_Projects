library(tidyverse)
library(tidytext)
library(wordcloud)
library(DT)
library(reshape2)

#Questions about the protest
#1. What are the most common words?
#2. What nrc emotions are the most prevalent
# anger, fear, anticipation, trust, surprise, sadness, joy, and disgust
#3. What are the most common bigrams and what do they tell us

#Tweets with the hashtag IranProtests2022



#Reading in our tweets
tweets <- read_csv("/home/sam/R/Text_Mining/Iran_Protests/tweets.csv")

iran_tweets <- tweets$text

iran_tweets <- tibble(line = 1:119182, text = iran_tweets)

data("stop_words")

#Tidying our tweets by getting it into a tidy frame 
#and removing stop words
tidy_protest <- iran_tweets %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words)

#Creating a datatable for our tidy_protest
datatable(tidy_protest %>% 
            anti_join(stop_words) %>% 
            count(word, sort = TRUE),
          extensions = "Buttons",
          options = list(
            dom = "Bfrtip",
            buttons = c("copy", "print", "csv")))

#Creating a bigrams list and tidying it 
birangrams <- iran_tweets %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) 

#Separating the bigrams while maintaining the same data frame
filtered_bitweets <- birangrams %>% 
  separate(bigram, c("word1", "word2"), sep = " ")

#filtering out the stop words in the filtered tweets
birangrams2 <- filtered_bitweets %>% 
  filter(!word1 %in% stop_words$word) %>% 
  filter(!word1 %in% stop_words$word)

biran_count <- birangrams2 %>% 
  mutate(bigram = paste(word1, word2, sep = " ", collapse = NULL)) %>% 
  count(bigram, sort = TRUE) %>% 
  filter(n > 500)
  
biran_count
#Graphing out the most common words in the biran_count
ggplot(data = biran_count) + aes(bigram, n) + geom_col() + theme(axis.text = element_text(angle = 45))


#Finding the most common words in the tweets 
#Then graphing them
wordCount <- tidy_protest %>% 
  count(word, sort = TRUE) %>% 
  filter(n > 1000)

ggplot(data = wordCount) + aes(x = word, y = n) + geom_col(fill = "green") +
  theme(axis.text = element_text(angle = 45))

#Creating a wordcloud of the most common words
wordCount %>% 
  count(word) %>% 
  with(wordcloud(word, n, max.words = 1000))

#Sentiment Analysis
nrc <- get_sentiments("nrc")
bing <- get_sentiments("bing")
afinn <- get_sentiments("afinn")

#Performing a Bing Analysis 
bingAn <- tidy_protest %>% 
  inner_join(bing) %>% 
  count(index = line, sentiment) %>% 
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative)
bingAn

#Plotting out the Analysis over the whole of the corpus
bingAnplot <- bingAn %>% 
  ggplot(aes(index, sentiment)) + 
  geom_col(fill = "blue") +
  labs(title = "Bing Analysis of Iranian Tweets")
bingAnplot
#Gathering and plotting out what the 
#largest contributors are to 
#the general sentiment
bingRan <- tidy_protest %>% 
  inner_join(bing) %>% 
  count(word, sentiment, sort = TRUE) %>% 
  ungroup()

bingRanPlot <- bingRan %>% 
  group_by(sentiment) %>% 
  top_n(10) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n, fill = sentiment))+
  geom_col() +
  labs(title = "Sentiment contribution by word") +
  theme(axis.text = element_text(angle = 45))
bingRanPlot

#Plotting out a sentiment wordcloud, showing 
#common words contribution in size vs sentiment
tidy_protest %>% 
  inner_join(get_sentiments("bing")) %>% 
  count(word, sentiment, sort = TRUE) %>% 
  acast(word ~ sentiment, value.var = "n", fill = 0) %>% 
  comparison.cloud(max.words = 100)

#Using the nrc data to find the most common emotion
# in the tweets
nrc_protest <- tidy_protest %>% 
  inner_join(get_sentiments("nrc")) %>% 
  count(word, sentiment, sort = TRUE) %>% 
  rename(frequency = n) %>% 
  group_by(sentiment)

tweetmotions <- nrc_protest %>% 
  ggplot() + 
  aes(sentiment, frequency) + geom_col(fill = "red") +
  theme(axis.text = element_text(angle = 45))

#Finding which words contribute the most towards which emotions
nrc_freq <- nrc_protest %>% 
  filter(frequency > 3000) %>% 
  ggplot(aes(word, frequency, fill = sentiment)) +
  geom_col() + 
  theme(axis.text = element_text(angle = 45))
nrc_freq

#Performing an Afinn Analysis
afinnAn <- tidy_protest %>% 
  inner_join(afinn) %>% 
  count(word, value, sort = TRUE) %>% 
  rename(frequency = n)
afinnAn

#Plotting out the Afinn analysis over the corpus of tweets
afinnPlot <- afinnAn %>% 
  ggplot() + aes(value, frequency) +
  geom_col(fill = "darkgreen") + 
  theme(axis.text = element_text(angle = 45)) +
  labs(title = "Frequency of Afinn scores")
afinnPlot

#Finding which words are the most common in the Afinn analysis
#And what their score is 
afinnFreq <- afinnAn %>% 
  filter(frequency > 3000) %>% 
  ggplot() + aes(x= word, y = value) + 
  geom_col(fill = "darkred") +
  labs(title = "Values of most common words") +
  theme(axis.text = element_text(angle = 45))
afinnFreq