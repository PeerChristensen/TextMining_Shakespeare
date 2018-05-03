# Shakespeare text mining 3:
# N-gram analysis

#load packages
library(igraph)
library(ggraph)
library(dplyr)
library(tidyverse)
library(tidytext)
library(glue)
library(tm)

# prep data
playList = list.files(pattern="txt")
df=data.frame()d
for (play in playList){
  text=glue(read_file(play))
  text=str_trim(gsub("[A-Z]{2,}","",text))
  text =tolower(text)
  text <- data_frame(text = text,play=factor(gsub(".txt","",play))) %>% 
    unnest_tokens(bigram, text, token = "ngrams", n = 2)
  #text=removeWords(text,c("I","Enter",stopwords("en")))
  df =rbind(df,text)
}

# n-gram frequency
df %>%
  count(bigram, sort = TRUE)

# apply td_idf
#df2 <- df %>%
#  count(play, bigram) %>%
#  bind_tf_idf(bigram, play, n)
#df2

bigrams_separated <- df %>%
  separate(bigram, c("word1", "word2"), sep = " ")

stopWords_shakespeare = c(stopwords("en"),"art","thou","dost","hast","enter","act","scene","s","first","second","third")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stopWords_shakespeare & 
           !word2 %in% stopWords_shakespeare)

# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(play, word1, word2, sort = TRUE)

bigrams = bigram_counts %>% 
  unite(bigram, word1, word2, sep=" ")  %>% 
  arrange(play,desc(n))

bigrams = bigrams %>%
  mutate(bigram = as.character(reorder(bigram, n))) %>%
  group_by(play) %>%
  top_n(7,n) %>%
  ungroup() %>%
  arrange(play, n) %>%
  mutate(row = row_number()) %>%
  group_by(play) %>%
  top_n(7,row) %>%
  ungroup

#plot
bigrams %>% 
  ggplot(aes(row, n, fill = play)) +
  geom_col(show.legend = FALSE) +
  labs(y = "n") +
  coord_flip(ylim = c(1,20)) +
  scale_x_continuous( 
    breaks = bigrams$row,
    labels = bigrams$bigram) +
  facet_wrap(~play, ncol = 2, scales = "free") 

####################################################

# n-gram network
#bigramSep <- df %>%
#  separate(bigram, c("word1", "word2"), sep = " ")

bigramCounts <- bigrams_filtered %>% 
  count(word1, word2,  sort = TRUE)

bigrams = bigramCounts %>% 
  unite(bigram, word1, word2, sep=" ")  %>% 
  arrange(desc(n))


# filter for only relatively common combinations
bigramGraph <- bigramCounts %>%
  filter(n > 10) %>%
  graph_from_data_frame()

bigramGraph

set.seed(71191)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigramGraph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_edge_link() +
  geom_node_point(color = "lightgreen", size = 3) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()
