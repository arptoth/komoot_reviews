library(tidyverse)
library(ggplot2)
library(tidytext)
library(wordcloud)


# Read US App Store reviews about Komoot
reviews <- read_json("reviews.json")

# Translate to Tibble data frame
reviews <-  bind_rows(reviews)

# Average ratings of Komoot App in the US Store based on these 157 reviews
mean(reviews$score)

# Distributions of ratings
reviews %>%  ggplot(aes(score)) + geom_bar() + theme_minimal()

# Number of reviews grouped by version
version_counts <- reviews %>% select(version) %>% group_by(version) %>% summarise(Count=n()) %>% arrange(desc(Count))

# Top 10 reviewed versions
version_counts %>% 
  head() %>% 
  ggplot(aes(x=version, y=Count)) + 
  geom_bar(stat = "identity") +
  theme_minimal()


# Average ratings by version
reviews %>% select(version, score) %>% 
  group_by(version) %>% 
  summarise(mean(score), Count=n()) %>% 
  arrange(desc(Count)) %>% 
  as_tibble()


# Testing tokenization
#example_text <- reviews[1,] %>% select(text)
#example_text %>% tidytext::unnest_tokens(sentence, text, token = "words")
#example_text %>% tidytext::unnest_tokens(sentence, text, token = "sentences")


# Who writes longer reviews?
sentence_data <- reviews %>% 
  select(userName, text) %>% 
  tidytext::unnest_tokens(sentence, text, token = "sentences")

head(sentence_data)

word_data <- reviews %>% 
  select(userName, text) %>% 
  tidytext::unnest_tokens(word, text, token = "words")

head(word_data)

sentences_count <- sentence_data %>% 
  group_by(userName) %>% 
  summarise(n_sentences = n_distinct(sentence))

# Top 10 reviewers by sentence numbers
sentences_count %>% arrange(desc(n_sentences))


word_count <- word_data %>% 
  group_by(userName) %>% 
  summarise(n_words = n_distinct(word))

# Top 10 reviewers by word count
word_count %>% arrange(desc(n_words))


# most frequent words 
word_data %>%
  anti_join(get_stopwords()) %>%
  count(word, sort = TRUE)


# wordclouds
word_data %>%
  anti_join(get_stopwords()) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))


# Title Analysis
title_words <- reviews %>% 
  select(userName, title) %>% 
  tidytext::unnest_tokens(word, title, token = "words")


# Word counts in title by userName
title_count <- title_words %>% 
  group_by(userName) %>% 
  summarise(Count = n()) %>% 
  arrange(desc(Count))


# Most frequent words in titles
title_words %>% 
  anti_join(get_stopwords()) %>% 
  count(word, sort=T) %>% 
  with(wordcloud(word, n, max.words = 100))





# TODO: 
# Modelling, Score prediction based on text
# Sentiment analysis

