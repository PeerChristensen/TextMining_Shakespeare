# Shakespeare text mining 1:
#Sentiment analysis

#load packages
library(tidyverse)
library(tidytext)
library(glue)
library(stringr)
library(tm)
library(zoo)

# 1a. simple sentiment analysis

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

# 2a. Temporal sentiment analysis A:
for (play in playList){
  #read and preprocess text
  text=glue(read_file(play))
  text=str_trim(gsub("[A-Z]{2,}","",text)) # remove uppercase words
  text =tolower(text) # all words to lowercase
  text=removeWords(text,stopwords("en")) # remove function words
  # tokenize
  tokens = data_frame(text = text) %>% unnest_tokens(word, text)
  tokens <- tibble::rowid_to_column(tokens, "ID")
  # Simple sentiments with "bing"
  sentiments = tokens %>%
  inner_join(get_sentiments("bing")) 
  sentiments$polarity = NULL
  sentiments$polarity[sentiments$sentiment=="negative"] = -1
  sentiments$polarity[sentiments$sentiment=="positive"] =  1
  
  rollMean<-rollmean(sentiments$polarity, 50,fill = list(NA, NULL, NA))
  sentiments$rollMean=rollMean
  plot=ggplot(sentiments) +
    aes(ID,polarity, fill= sentiment) +
    geom_col() +
    geom_line(aes(ID,rollMean)) +
    ggtitle(gsub(".txt","",play)) +
    theme_minimal()
    print(plot)
}

# 2a. Temporal sentiment analysis B:

for (play in playList){
#read and preprocess text
text=glue(read_file(play))
text=str_trim(gsub("[A-Z]{2,}","",text)) # remove uppercase words
text =tolower(text) # all words to lowercase
text=removeWords(text,stopwords("en")) # remove function words
# tokenize
tokens = data_frame(text = text) %>% unnest_tokens(word, text)
tokens <- tibble::rowid_to_column(tokens, "ID")
# Simple sentiments with "bing"
sentiments = tokens %>%
  inner_join(get_sentiments("bing")) 
sentiments$polarity = NULL
sentiments$polarity[sentiments$sentiment=="negative"] = -1
sentiments$polarity[sentiments$sentiment=="positive"] =  1

means=colMeans(matrix(sentiments$polarity, nrow=30))
df=data.frame(row=seq(1:length(means)),means)

plot = df %>% 
  ggplot() +
  ggtitle(gsub(".txt","",play)) +
  theme_dark()+
  aes(row,means,fill=means) +
  geom_col() +
  ylim(-1,1) +
  scale_fill_gradient2(low = "red", mid = "white",
                       high = "blue", midpoint = 0, space = "Lab",
                       na.value = "grey50", guide = "colourbar")
  print(plot)
}

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
