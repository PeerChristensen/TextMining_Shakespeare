# Shakespeare text mining 4:
# Topic modelling

library(tidyverse)
library(tidytext)
library(glue)
library(stringr)
library(wordcloud)
library(tm)
library(ggplot2)
library(topicmodels)

omitWords=c("thy","thee","thou","lord","lady","o","act","i","scene","will","shall","come")

playList = list.files(pattern="txt")
df=data.frame()
for (play in playList){
  text = glue(read_file(play))
  text = str_trim(gsub("[A-Z]{2,}","",text))
  text = tolower(text)
  text = removeWords(text,c(omitWords,stopwords("en")))
  text = data_frame(text = text,play=factor(gsub(".txt","",play))) %>% unnest_tokens(word, text)
  df   = rbind(df,text)
}

playWords <- df %>%
  count(play, word, sort = TRUE) %>%
  ungroup()

playsDTM <- playWords %>%
  cast_dtm(play, word, n)

playLDA <- LDA(playsDTM, k = 9, control = list(seed = 347))

#per-topic per-word probabilities
topics <- tidy(playLDA, matrix = "beta")

#docs assigned to topics
as.matrix(topics(playLDA))

#top terms per topic
topTerms <- topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta) %>%
  mutate(order = rev(row_number())) # necessary for ordering words

plot=topTerms %>%
  ggplot(aes(order, beta, fill = factor(topic))) +
  ggtitle("Topics") +
  geom_col(show.legend = FALSE) +
  scale_x_continuous(
    breaks = topTerms$order,
    labels = topTerms$term,
    expand = c(0,0))+
  facet_wrap(~ topic,scales="free") +
  coord_flip()
plot

