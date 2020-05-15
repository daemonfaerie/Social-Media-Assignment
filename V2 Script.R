#read in the 2020 datafile
metootweets <- read.csv("C:\\MeToo\\metoo_tweets_Apr2020.csv", stringsAsFactors=FALSE)
head(metootweets$text)


library(tm)
library(wordcloud)
library(stringi)
library(wesanderson)
library(magrittr)
library(tidyverse)
library(tidytext)
library(glue)
library(stringr)
library(viridis)
library(dplyr)
library(textdata)
library(ggplot2)

#clean up the data and create a corpus
metootweets$text <- sapply(metootweets$text,function(row) iconv(row, "latin1", "ASCII", sub=""))
cloud <- Corpus(VectorSource(metootweets$text))
cloud <- cloud %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace) %>%
  tm_map(removeNumbers)%>%
  tm_map(content_transformer(tolower)) %>%
   tm_map(removeWords, stopwords('english')) %>%
  tm_map(removeWords, c('amp','metoo')) %>%
  tm_map(removeWords, c('&amp;','httpstcobvbyxqfehn', 'httpstcoclyjqbm', 'httpstcolhwfenfuv', 'spent', 'started', 'era', 'reade', 'reades', 'biden', 'joe', 'doesnt', 'tara', 'since', 'polygraphwhat', 'hashtag','metooim','via','and','can','now','new','dont','one','know','think','using','just','made', 'say', 'says', 'ive', 'needs', 'will', 'what', 'where', 'much', 'rather'))
  
	

#create wordcloud
wordcloud(cloud, max.words = 70, scale = c(3, 1),colors=brewer.pal(4, "Dark2"), random.color = TRUE, random.order = FALSE)


#Graphs
metootweets$text <-  sapply(metootweets$text, function(row) iconv(row, 'latin1','ASCII',sub=""))
metoo_sentiment <- metootweets %>%
  unnest_tokens(word, text)
metoo_sentiment_freq <- metoo_sentiment %>%
  inner_join(get_sentiments("nrc")) %>% 
  dplyr::count(sentiment, sort = TRUE) %>% 
  mutate(sentiment = reorder(sentiment, n)) %>% 
  ggplot(aes(sentiment,n, fill=sentiment)) + 
  geom_col(color='white', stat='identity') + 
  theme(axis.text.y=element_blank()) + 
  labs(x='Sentiment', y='Frequency') + 
  scale_fill_viridis(discrete=TRUE, option = "C") + 
  theme(text = element_text(size = 15))
metoo_sentiment_freq


metoo_sentiment_freq2 <- metoo_sentiment %>%
  inner_join(get_sentiments("nrc")) %>%
  dplyr::count(word, sentiment, sort = TRUE) %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) + 
  geom_col(show.legend=FALSE, stat='identity') + 
  facet_wrap(~sentiment, scales='free_y', nrow=3) + 
  labs(y = NULL, x = NULL) + 
  coord_flip() + 
  scale_fill_viridis(discrete=TRUE, option = "C") + 
  theme(text = element_text(size=10))
metoo_sentiment_freq2
