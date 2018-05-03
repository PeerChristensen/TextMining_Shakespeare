#download Shakespeare with gutenbergr

library(gutenbergr)
library(tidyverse)
shakespeare <- gutenberg_works(author == "Shakespeare, William") %>%
  gutenberg_download(meta_fields = "title")
