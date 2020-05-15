
# CODE SOURCED FROM https://rstudio-pubs-static.s3.amazonaws.com/448828_74b58f5f8f6346f5822dfd09fb326582.html#

# PREPARE - install the packages we are going to use (this is only needed once, once installed they are installed)
# CRIBBED FROM GUIDE HERE - http://jtleek.com/modules/01_DataScientistToolbox/02_09_installingRPackages/#1
install.packages("tm")
install.packages("wordcloud")
install.packages("stringi")
install.packages("wesanderson")
install.packages("magrittr")

#for graphs not working?
install.packages("tidyverse")
install.packages("tidytext")
install.packages("glue")
install.packages("stringr")
install.packages("viridis")
install.packages("dplyr")
install.packages("textdata")


# MAIN CODE


#read in the 2017 datafile
metootweets <- read.csv("C:\\MeToo\\metoo_tweets_dec2017.csv", stringsAsFactors=FALSE)
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
  tm_map(removeWords, c('&amp;','@funder:','funder','via','and','can','now','new','dont','one','know','think','using','just','made'))
  
	

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

