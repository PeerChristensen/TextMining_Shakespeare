# Shakespeare text mining 2:
# Word frequency td_idf method and wordcloud

library(tidyverse)
library(tidytext)
library(glue)
library(stringr)
library(wordcloud)
library(tm)
library(wordcloud2)

##########################################
#td_idf method - term frequency–inverse document frequency

#read and preprocess all texts

playList = list.files(pattern="txt")
df=data.frame()
for (play in playList){
  text = glue(read_file(play))
  text = str_trim(gsub("[A-Z]{2,}","",text))
  text = tolower(text)
  text = data_frame(text = text, play=factor(gsub(".txt","",play))) %>% 
    unnest_tokens(word, text) 
  df   = rbind(df, text)
}

playWords = df %>%
  count(play, word, sort = TRUE) %>%
  ungroup()

totalWords = playWords %>% 
  group_by(play) %>% 
  summarise(total = sum(n))

playWords = left_join(playWords, totalWords)

#term frequency, we want to get rid of the long tails
ggplot(playWords, aes(n/total, fill = play)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.009) +
  facet_wrap(~play, ncol = 2, scales = "free_y")

#tf idf - Notice that idf and thus tf-idf are zero for these extremely common words appearing in all plays
playWords = playWords %>%
  bind_tf_idf(word, play, n)

#words with high td_idf
playWords = playWords %>%
  select(-total)  %>% 
  arrange(play,desc(tf_idf))

playWords = playWords %>%
  mutate(word = reorder(word, tf_idf)) %>%
  group_by(play) %>%
  top_n(10,tf_idf) %>% 
  ungroup() %>%
  arrange(play, tf_idf) %>%
  mutate(row = row_number()) 

playWords %>% 
  ggplot(aes(row, tf_idf, fill = play)) +
  geom_col(show.legend = FALSE) +
  labs(y = "tf-idf") +
  facet_wrap(~play, ncol = 2, scales = "free") +
  scale_x_continuous( 
    breaks = playWords$row,
    labels = playWords$word) +
  coord_flip()


##########################################
# wordclouds
set.seed(7863)

p=playWords %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(play) %>% 
  top_n(100)

for (play in playList){
  
  df=p[p$play==gsub(".txt","",play),]
  wordcloud(words = df$word, freq = df$n, min.freq = 1,
            max.words=100, random.order=FALSE, rot.per=0.35, 
            colors=brewer.pal(8, "Dark2"))
}

# custom big cloud
p2=playWords %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  top_n(10000) %>%
  select(word,n)

set.seed(1112)
wordcloud2(p2, figPath = "silh2.png", size = 0.5, color = "snow", backgroundColor="black")
#letterCloud(p2, word = "SHAKESPEARE", size = 3, color = "random-light", backgroundColor="grey")

