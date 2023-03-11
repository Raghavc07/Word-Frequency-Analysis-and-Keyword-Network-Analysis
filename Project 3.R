# Installing the required Libraries
library(dplyr)
library(stringr)
library(tidytext)
library(janeaustenr)
library(ggplot2)
library(tidyr)
library(igraph)
library(ggraph)


# Task 2

# Importing dataset
df_data <- read.csv("task2.csv", na.strings = "")


# Adding column year depending on date
df_tweet <- df_data %>% 
  mutate(year = case_when(date >= "2022-01-01" & date < "2023-04-01" ~ 2022,
                          date >= "2021-01-01" & date < "2022-01-01" ~ 2021,
                          date >= "2020-01-01" & date < "2021-01-01" ~ 2020,
                          date >= "2019-01-01" & date < "2020-01-01" ~ 2019,
                          date >= "2018-01-01" & date < "2019-01-01" ~ 2018,
                          date >= "2017-01-01" & date < "2018-01-01" ~ 2017)) %>% 
  select(year, tweet) %>% filter(year > 2016 & year < 2023)

# Converting sentences to words
# Removing stop words
tweet_words <- df_tweet %>% unnest_tokens(word, tweet) %>% 
  count(year, word, sort = TRUE) %>% filter(!word %in% stop_words$word)

# Removing words having less tfidf value
tweet_words <- tweet_words %>% bind_tf_idf(word, year, n) %>% 
  arrange(desc(tf_idf)) %>% slice_max(tf_idf, n = nrow(tweet_words)/1.25)

# Total words by the user in each year
total_tweet_words <- tweet_words %>% group_by(year) %>% 
  summarize(total = sum(n))

# tweet_words = total words + tweet world
tweet_words <- left_join(tweet_words, total_tweet_words) %>% 
  relocate(total, .after = n)

# Adding word frequency
tweet_words <- tweet_words %>% mutate(word_freq = n/total) %>% 
  relocate(word_freq, .after = total)

# Word frequency for each year
wf_2022 <- tweet_words %>% filter(year == 2022)
wf_2022
wf_2021 <- tweet_words %>% filter(year == 2021)
wf_2021
wf_2020 <- tweet_words %>% filter(year == 2020)
wf_2021
wf_2019 <- tweet_words %>% filter(year == 2019)
wf_2019
wf_2018 <- tweet_words %>% filter(year == 2018)
wf_2018
wf_2017 <- tweet_words %>% filter(year == 2017)
wf_2017

# Top 10 words by highest value of word frequency
wf_2022[,c(2,5)] %>% slice_max(word_freq, n = 10)
wf_2021[,c(2,5)] %>% slice_max(word_freq, n = 10)
wf_2020[,c(2,5)] %>% slice_max(word_freq, n = 10)
wf_2019[,c(2,5)] %>% slice_max(word_freq, n = 10)
wf_2018[,c(2,5)] %>% slice_max(word_freq, n = 10)
wf_2017[,c(2,5)] %>% slice_max(word_freq, n = 10)

# Histogram of word frequency for each year
ggplot(tweet_words, aes(word_freq, fill = year)) + 
  geom_histogram(show.legend = FALSE, bins = 20) +
  xlim(NA, 0.0009) +
  facet_wrap(~year, ncol = 2, scales = "free_y")

# Log-log plot of word frequency vs rank for each year using Zipf's law
freq_by_rank <- tweet_words %>% arrange(desc(word_freq)) %>%  group_by(year) %>% 
  mutate(rank = row_number()) %>% ungroup()

rank_subset <- freq_by_rank %>% filter(year == 2022) %>% 
  filter(rank < 100, rank > 1)

lm(log10(word_freq) ~ log10(rank), data = rank_subset)

rank_subset <- freq_by_rank %>% filter(year == 2021) %>% 
  filter(rank < 100, rank > 1)

lm(log10(word_freq) ~ log10(rank), data = rank_subset)

rank_subset <- freq_by_rank %>% filter(year == 2020) %>% 
  filter(rank < 1000, rank > 10)

lm(log10(word_freq) ~ log10(rank), data = rank_subset)

rank_subset <- freq_by_rank %>% filter(year == 2019) %>% 
  filter(rank < 500, rank > 5)

lm(log10(word_freq) ~ log10(rank), data = rank_subset)

rank_subset <- freq_by_rank %>% filter(year == 2018) %>% 
  filter(rank < 500, rank > 5)

lm(log10(word_freq) ~ log10(rank), data = rank_subset)

rank_subset <- freq_by_rank %>% filter(year == 2017) %>% 
  filter(rank < 100, rank > 1)

lm(log10(word_freq) ~ log10(rank), data = rank_subset)

freq_by_rank %>% 
  ggplot(aes(rank, word_freq, color = factor(year), group=year)) + 
  geom_abline(intercept = -1.58, slope = -0.67, color = "gray50", linetype = 2)+
  geom_abline(intercept = -1.56, slope = -0.67, color = "gray50", linetype = 2)+
  geom_abline(intercept = -1.78, slope = -0.63, color = "gray50", linetype = 2)+
  geom_abline(intercept = -1.85, slope = -0.59, color = "gray50", linetype = 2)+
  geom_abline(intercept = -2.21, slope = -0.45, color = "gray50", linetype = 2)+
  geom_abline(intercept = -2.31, slope = -0.37, color = "gray50", linetype = 2)+
  geom_line(size = 1.1, alpha = 0.8, show.legend = TRUE) +
  scale_x_log10() + scale_y_log10()

# Bigram network graph for each year
tweet_bigrams <- df_tweet %>% select(year, tweet) %>% 
  unnest_tokens(bigram, tweet, token = "ngrams", n = 2) %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  unite(bigram, word1, word2, sep = " ") %>% count(year, bigram) %>%
  bind_tf_idf(bigram, year, n) %>% arrange(desc(tf_idf)) %>%
  separate(bigram, c("word1", "word2"), sep = " ")

# Removing words with less tfidf value
tweet_bigrams <- tweet_bigrams %>% 
  slice_max(tf_idf, n = nrow(tweet_bigrams)/1.25) %>% 
  select(year, word1, word2, n)

# Bigram network graph for 2022
tweet_bigram_2021 <- tweet_bigrams %>% filter(year == 2022) %>% 
  select(- year) %>% filter(n > 3) %>%  graph_from_data_frame()

ggraph(tweet_bigram_2021, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

# Bi-gram network graph for 2021
tweet_bigram_2021 <- tweet_bigrams %>% filter(year == 2021) %>% 
  select(- year) %>% filter(n > 3) %>%  graph_from_data_frame()

ggraph(tweet_bigram_2021, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

# Bi-gram network graph for 2020
tweet_bigrams_2020 <- tweet_bigrams %>% filter(year == 2020) %>% 
  select(- year) %>% filter(n > 12) %>%  graph_from_data_frame()

ggraph(tweet_bigrams_2020, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

# Bi-gram network graph for 2019
tweet_bigrams_2019 <- tweet_bigrams %>% filter(year == 2019) %>% 
  select(- year) %>% filter(n > 13) %>%  graph_from_data_frame()

ggraph(tweet_bigrams_2019, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

# Bi-gram network graph for 2018
tweet_bigrams_2018 <- tweet_bigrams %>% filter(year == 2018) %>% 
  select(- year) %>% filter(n > 9) %>%  graph_from_data_frame()

ggraph(tweet_bigrams_2018, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

# Bi-gram network graph for 2017
tweet_bigrams_2017 <- tweet_bigrams %>% filter(year == 2017) %>% 
  select(- year) %>% filter(n > 3) %>%  graph_from_data_frame()

ggraph(tweet_bigrams_2017, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)