#Step 1: Load the required packages (including rtweet) in RStudio
library(rtweet)
library(textdata)
library(janeaustenr)
library(reshape2)
library(devtools)
library(widyr)
library(lubridate) # Date & Time
library(ggplot2)# Data Visualisation
library(dplyr) #Data Wrangling
library(tidytext) #Text Mining
library(tm) #Text Mining
library(wordcloud)
library(readr)
library(tidyr) #Tidy Text
library(RColorBrewer) #Data Visualisation
library(stringr) #String Manipulation
library(RSentiment) #Sentiment Analysis
library(cowplot) #Plot Arrange
library(ggthemes) #Data Visualisation
library(knitr)
library(kableExtra)
library(tm)
library(stopwords)


# read in our data
f <- file.choose("virusdata.csv")
virus <- read.csv(f)



virus %>% 
  unnest_tokens(output = word, input = text) %>% 
  anti_join(stop_words) %>% 
  count(word, sort = TRUE)

virus %>% 
  unnest_tokens(output = word, input = text) %>% 
  anti_join(stop_words) %>% 
  count(word, sort = TRUE) %>% 
  slice(1:10) %>% 
  ggplot() + geom_bar(aes(word, n), stat = "identity", fill = "#de5833") +
  theme_minimal() +
  labs(title = "Top unigrams of Africans COVID-19 Tweets")


virus %>% 
  unnest_tokens(word, text, token = "ngrams", n = 2) %>% 
  separate(word, c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  unite(word,word1, word2, sep = " ") %>% 
  count(word, sort = TRUE) %>% 
  slice(1:10 ) %>% 
  ggplot() + geom_bar(aes(word, n), stat = "identity", fill = "#de5833") +
  theme_minimal() +
  coord_flip() +
  labs(title = "Top Bigrams of Africans COVID-19 Tweets")


virus %>% 
  unnest_tokens(word, text, token = "ngrams", n = 3) %>% 
  separate(word, c("word1", "word2", "word3"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word) %>% 
  unite(word,word1, word2, word3, sep = " ") %>% 
  count(word, sort = TRUE) %>% 
  slice(1:10) %>% 
  ggplot() + geom_bar(aes(word, n), stat = "identity", fill = "#de5833") +
  theme_minimal() +
  coord_flip() +
  labs(title = "Top Trigrams of Africans COVID-19 Tweets")