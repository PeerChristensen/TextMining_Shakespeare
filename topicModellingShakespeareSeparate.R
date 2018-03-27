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
other=c("thy","thee","thou","lord","lady","o","act","i","scene")

for (play in playList){
  
  text=glue(read_file(play))
  #text =tolower(text)
  text=removeWords(text,c(other,characters,stopwords("en")))
  text=str_trim(gsub("[A-Z]{2,}","",text))
  text <- data_frame(text = text,play=factor(gsub(".txt","",play))) %>% unnest_tokens(word, text)

  playWords <- text %>%
  count(play, word, sort = TRUE) %>%
  ungroup()

  playsDTM <- playWords %>%
  cast_dtm(play, word, n)

  playLDA <- LDA(playsDTM, k = 5, control = list(seed = 347))

#per-topic per-word probabilities
  topics <- tidy(playLDA, matrix = "beta")

#top terms per topic
  topTerms <- topics %>%
    group_by(topic) %>%
    top_n(5, beta) %>%
    ungroup() %>%
    arrange(topic, -beta) %>%
    mutate(order = rev(row_number())) # necessary for ordering words
  
  plot=topTerms %>%
    ggplot(aes(order, beta, fill = factor(topic))) +
    ggtitle(gsub(".txt","",play)) +
    geom_col(show.legend = FALSE) +
    scale_x_continuous(
      breaks = topTerms$order,
      labels = topTerms$term,
      expand = c(0,0))+
    facet_wrap(~ topic,scales="free") +
    coord_flip()
    print(plot)
 
}


