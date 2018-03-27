

# topic modelling
library(tidyverse)
library(tidytext)
library(glue)
library(stringr)
library(wordcloud)
library(tm)
library(ggplot2)
library(topicmodels)


playList = list.files(pattern="txt")
df=data.frame()
plots=list()
characters=c("hamlet","macduff","macbeth","juliet","romeo","horatio",
             "banquo","gertrude","polonius","laertes","capulet")
other=c("i","thy","thee","thou","lord","lady","o","act","i","scene")

for (play in playList){
  
  text=glue(read_file(play))
  text=str_trim(gsub("[A-Z]{2,}","",text))
  text =tolower(text)
  text=removeWords(text,c("scene","act",other,characters,stopwords("english")))
  text <- data_frame(text = text,play=factor(gsub(".txt","",play))) %>% unnest_tokens(word, text)
  df =rbind(df,text)
}
  
  playWords <- df %>%
    count(play, word, sort = TRUE) %>%
    ungroup()
  
  totalWords <- playWords %>% 
    group_by(play) %>% 
    summarise(total = sum(n))

  playWords <- left_join(playWords, totalWords)
  
  playWords <- playWords %>%
    bind_tf_idf(word, play, n)

  
  playWords=playWords[playWords$tf_idf>0,]
  playWords=playWords[,1:3]
  
  #SPLIT PLAYS HERE !!!
  dfList=split(playWords, playWords$play)
  d=playWords[playWords$play=="hamlet",]
  
  playsDTM <- playWords %>%
    cast_dtm(play, word, n)

  playLDA <- LDA(playsDTM, k = 3, control = list(seed = 347))
  
  #per-topic per-word probabilities
  topics <- tidy(playLDA, matrix = "beta")
  print(topics)
  
  #docs assigned to topics
  topicsLDA <- as.matrix(topics(playLDA))
  
  #top terms per topic
  topTerms <- topics %>%
    group_by(topic) %>%
    top_n(5, beta) %>%
    ungroup() %>%
    arrange(topic, -beta) %>%
    mutate(order = rev(row_number())) # necessary for ordering words
  print(topTerms)

  plot=topTerms %>%
    ggplot(aes(order, beta, fill = factor(topic))) +
    ggtitle(i) +
    geom_col(show.legend = FALSE) +
    scale_x_continuous(
      breaks = topTerms$order,
      labels = topTerms$term,
      expand = c(0,0))+
    facet_wrap(~ topic,scales="free") +
    coord_flip()
  print(plot)

}


