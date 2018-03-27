#FINAL
# text analysis of all Shakespeare's plays
#td_idf method - term frequency–inverse document frequency


library(tidyverse)
library(tidytext)
library(glue)
library(stringr)
library(wordcloud)
library(tm)


##########################################
#td_idf method - term frequency–inverse document frequency

#read and preprocess all texts

a = list.files(pattern="txt")
df=data.frame()
for (i in a){
  text=glue(read_file(i))
  text=removeWords(text,c("act","i","scene",stopwords("en")))
  text=str_trim(gsub("[A-Z]{2,}","",text))
  text =tolower(text)
  text <- data_frame(text = text,play=factor(gsub(".txt","",i))) %>% unnest_tokens(word, text)
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
#not surprisingly proper nouns that occur frequently, but not across plays

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
