library(tidyverse)
library(ggplot2)
library(tidytext)
library(wordcloud)
library(caret)
library(glmnet)
library(h2o)
library(lime)


# Data import and cleaning ------------------------------------------------

# Read US App Store reviews about Komoot
reviews <- read_json("reviews.json")

# Translate to Tibble data frame
reviews <-  bind_rows(reviews)



# Exploratory data analysis -----------------------------------------------


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



# Title Analysis ----------------------------------------------------------


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



# Modelling, estimating score by words ------------------------------------


sparse_matrix <- reviews %>% 
  select(userName, text) %>% 
  tidytext::unnest_tokens(word, text, token = "words") %>% 
  anti_join(get_stopwords()) %>%
  count(userName, word, sort = TRUE) %>% 
  cast_sparse(userName, word, n)
  
dim(sparse_matrix)


sparse_matrix <- as.data.frame(as.matrix(sparse_matrix))


sparse_matrix$userName <- rownames(sparse_matrix)
sparse_matrix$userName

train_df <- left_join(x=sparse_matrix, y=reviews[,c("userName", "score")])


train_df %>% select(userName, score, fun) %>% as_tibble()



# H2O MODELLING -----------------------------------------------------------

# Set seed because of reproducability
n_seed = 12345

# Create target and feature list
target = "score" # Result
features = setdiff(colnames(train_df), c("target", "userName"))
print(features)


# Start a local H2O cluster (JVM)
h2o.init()

# H2O dataframe
h_data <-  as.h2o(train_df)


# Split Train/Test
h_split = h2o.splitFrame(h_data, ratios = 0.75, seed = n_seed)
h_train = h_split[[1]] # 75% for modelling
h_test = h_split[[2]] # 25% for evaluation




# Train a Default H2O GBM model
model_gbm = h2o.gbm(x = features,
                    y = target,
                    training_frame = h_train,
                    model_id = "my_gbm",
                    seed = n_seed)
print(model_gbm)


# Evaluate performance on test
h2o.performance(model_gbm, newdata = h_test)
h2o.predict(model_gbm, newdata = h_test)




### Test other algos with AutoML

model_automl <- h2o.automl(x = features,
                           y = target,
                           training_frame = h_train,
                           nfolds = 5,               # Cross-Validation
                           max_runtime_secs = 30,   # Max time
                           max_models = 100,         # Max no. of models
                           stopping_metric = "RMSE", # Metric to optimize
                           project_name = "my_automl",
                           exclude_algos = NULL,     # If you want to exclude any algo 
                           seed = n_seed)


model_automl@leaderboard 

model_automl@leader

h2o.performance(model_automl@leader, newdata = h_test)

new <- NULL
new$pred <- as.vector(h2o.predict(model_automl@leader, newdata = h_test))
new$actual <- as_data_frame(h_test)$score

as_data_frame(new) %>% head(n=20)

# Make explanations -------------------------------------------------------

explainer = lime(x = as.data.frame(h_train[, 1:50]),model = model_automl@leader)

# Extract one sample (change `1` to any row you want)
d_samp = as.data.frame(h_test[1, 1:50])
# Assign a specifc row name (for better visualization)
row.names(d_samp) = "Sample 1" 
# Create explanations
explanations = lime::explain(x = d_samp,
                             explainer = explainer,
                             n_permutations = 5000,
                             feature_select = "auto",
                             n_features = 13) # Look top x features

lime::plot_features(explanations, ncol = 1)
  




# TODO: 
# Sentiment analysis

