library(tidyverse)
library(ggplot2)
library(tidytext)


# Read US App Store reviews about Komoot
reveiws <- read_json("reviews.json")

# Translate to Tibble data frame
reveiws <-  bind_rows(reveiws)

# Average ratings of Komoot
mean(reveiws$score)

# Distributions of ratings
reveiws %>%  ggplot(aes(score)) + geom_bar()

# Number of reviews grouped by version
version_counts <- reveiws %>% select(version) %>% group_by(version) %>% summarise(Count=n()) %>% arrange(desc(Count))

# Top 10 reveiwed versions
version_counts %>% 
  head() %>% 
  ggplot(aes(x=version, y=Count)) + 
  geom_bar(stat = "identity")


# Average ratings by version
reveiws %>% select(version, score) %>% 
  group_by(version) %>% 
  summarise(mean(score), Count=n()) %>% 
  arrange(desc(Count)) %>% 
  as_tibble()



