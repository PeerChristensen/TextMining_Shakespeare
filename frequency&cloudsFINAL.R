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
  text=glue(read_file(play))
  text=removeWords(text,stopwords("en"))
  text=str_trim(gsub("[A-Z]{2,}","",text))
  text =tolower(text)
  text <- data_frame(text = text,play=factor(gsub(".txt","",play))) %>% unnest_tokens(word, text)
  df =rbind(df,text)
}

playWords <- df %>%
  count(play, word, sort = TRUE) %>%
  ungroup()
playWords

totalWords <- playWords %>% 
  group_by(play) %>% 
  summarise(total = sum(n))
totalWords

playWords <- left_join(playWords, totalWords)

playWords

#term frequency, we want to get rid of the long tails
ggplot(playWords, aes(n/total, fill = play)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.009) +
  facet_wrap(~play, ncol = 2, scales = "free_y")

#tf idf
playWords <- playWords %>%
  bind_tf_idf(word, play, n)
playWords
#Notice that idf and thus tf-idf are zero for these extremely common words appearing in all plays

#words with high td_idf
playWords %>%
  select(-total) %>%
  arrange(desc(tf_idf))

playWords %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(play) %>% 
  top_n(15) %>% 
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = play)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~play, ncol = 2, scales = "free") +
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
cloud=wordcloud2(p2, figPath = "silh2.png", size = 0.5, color = "snow", backgroundColor="black")
#letterCloud(p2, word = "SHAKESPEARE", size = 3, color = "random-light", backgroundColor="grey")

library("htmlwidgets")
saveWidget(cloud,"tmp.html",selfcontained = F)

webshot("tmp.html","fig_1.pdf", delay =5, vwidth = 480, vheight=480)