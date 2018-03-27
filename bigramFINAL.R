# Shakespeare text mining 3:
# N-gram analysis

#load packages
library(igraph)
library(ggraph)
library(dplyr)
library(tidytext)

# prep data
playList = list.files(pattern="txt")
df=data.frame()
for (play in playList){
  text=glue(read_file(play))
  text=removeWords(text,c("I",stopwords("en")))
  text=str_trim(gsub("[A-Z]{2,}","",text))
  text =tolower(text)
  text <- data_frame(text = text,play=factor(gsub(".txt","",play))) %>% 
    unnest_tokens(bigram, text, token = "ngrams", n = 2)
  df =rbind(df,text)
}

# n-gram frequency
df %>%
  count(bigram, sort = TRUE)

# apply td_idf
df2 <- df %>%
  count(play, bigram) %>%
  bind_tf_idf(bigram, play, n)
df2

#plot
df2 %>% 
  group_by(play) %>% 
  top_n(8, tf_idf) %>% 
  ungroup() %>%
  mutate(bigram = reorder(bigram, tf_idf)) %>%
  ggplot(aes(bigram, tf_idf, fill = play)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~play, ncol = 2, scales = "free") +
  coord_flip()

####################################################

# n-gram network
bigramSep <- df %>%
  separate(bigram, c("word1", "word2"), sep = " ")
bigramCounts <- bigramSep %>% 
  count(word1, word2, sort = TRUE)

# filter for only relatively common combinations
bigramGraph <- bigramCounts %>%
  filter(n > 15) %>%
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




