# Simple text analysis of Macbeth
# sentiment analysis, wordcloud and word frequency

library(tidyverse)
library(tidytext)
library(glue)
library(stringr)
library(wordcloud)
library(tm)
library(ggplot2)

#read and preprocess text
text <- glue(read_file("macbeth.txt"))
#remove capitalized names
#text =tolower(text)
text=removeWords(text,c("act","i","scene",stopwords("en")))

text=str_trim(gsub("[A-Z]{2,}","",text))

# tokenize
tokens <- data_frame(text = text) %>% unnest_tokens(word, text)

# sentiments "bing" pos v neg, "nrc" emotions
sentiments = tokens %>%
  inner_join(get_sentiments("bing")) %>% # pull out only sentiment words
  count(sentiment) %>% # count the # of positive & negative words
  spread(sentiment, n, fill = 0) %>% # made data wide rather than narrow
  mutate(sentiment = positive - negative) # # of positive words - # of negative owrds

df=data.frame(Sentiments=names(sentiments),Values=t(sentiments)[,1])

sentimentsBars = ggplot(df, aes(x=Sentiments, y=Values)) + 
  geom_bar(stat="identity") 
  
sentimentsBars

#############################

# sort by frequency
freq = tokens %>% count(word,sort=T)

#instances with variations of 'blood'
str_count(tokens, c("blood","bloody","bleed","bled","bleeding"))

#word cloud
wordcloud(words = freq$word, freq = freq$n, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

# frequency bar plot
freq2=freq[1:15,]

freqBar=ggplot(freq2,aes(reorder(word,n),n)) + 
  geom_bar(stat = "identity",fill="darkred") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("Words") +
  ylab("Frequency") +
  coord_flip() +
  theme_minimal()
freqBar

#########################

