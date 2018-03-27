# Shakespeare text mining 1:
#Sentiment analysis

#load packages
library(tidyverse)
library(tidytext)
library(glue)
library(stringr)
library(tm)
library(ggplot2)

# 1. simple sentiment analysis

playList = list.files(pattern="txt")
df=data.frame()
for (play in playList){
  
  #read and preprocess text
  text=glue(read_file(play))
  text=str_trim(gsub("[A-Z]{2,}","",text)) # remove uppercase words
  text =tolower(text) # all words to lowercase
  text=removeWords(text,stopwords("en")) # remove function words
  # tokenize
  tokens = data_frame(text = text) %>% unnest_tokens(word, text)

  # Simple sentiments with "bing"
  sentiments = tokens %>%
    inner_join(get_sentiments("bing")) %>% # pull out only sentiment words
    count(sentiment) %>% # count the # of positive & negative words
    spread(sentiment, n, fill = 0) %>% # made data wide rather than narrow
    mutate(sentiment = positive - negative) # # of positive words - # of negative owrds

  playDF=data.frame(Play=play,Sentiments=names(sentiments),Values=t(sentiments)[,1])
  df=rbind(df,playDF)
}

sentimentsBarsSimple = ggplot(df, aes(x=Sentiments, y=Values)) + 
  geom_bar(stat="identity") +
  facet_wrap(~gsub(".txt","",Play))

sentimentsBarsSimple

###########################################

# 2. complex sentiments
df=data.frame()
for (play in playList){
  
  #read and preprocess text
  text=glue(read_file(play))
  text=str_trim(gsub("[A-Z]{2,}","",text)) # remove uppercase words
  text =tolower(text) # all words to lowercase
  text=removeWords(text,stopwords("en")) # remove function words
  # tokenize
  tokens = data_frame(text = text) %>% unnest_tokens(word, text)
  
  # complex sentiments with "nrc"
  sentiments = tokens %>%
    inner_join(get_sentiments("nrc")) %>% # pull out only sentiment words
    count(sentiment) %>% # count the # of positive & negative words
    spread(sentiment, n, fill = 0) # made data wide rather than narrow
  
  sentiments=sentiments/sum(sentiments) #proportion
  
  playDF=data.frame(Play=play,Sentiments=names(sentiments),Values=t(sentiments)[,1])
  df=rbind(df,playDF)
}

sentimentsBarsComplex = ggplot(df, aes(x=Sentiments, y=Values,fill=Sentiments)) + 
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~gsub(".txt","",Play))

sentimentsBarsComplex
