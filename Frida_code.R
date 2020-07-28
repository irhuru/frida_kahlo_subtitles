# Loading packages

library(tidyverse)
library(tidytext)
library(stringi)
library(igraph)
library(ggraph)
library(syuzhet)

# Loading sentiments in Spanish

source("https://raw.githubusercontent.com/7PartidasDigital/R-LINHD-18/master/get_sentiments.R")
sentimientos = readr::read_tsv("https://tinyurl.com/SentiEsp",
                                col_types = "cccn",
                                locale = default_locale())

# Loading texts

Frida_English = readLines("Frida_English.txt")
Frida_Spanish = readLines("Frida_Spanish.txt")

# Removing no-text elements from each character vector

Frida_English = stri_remove_empty(Frida_English, TRUE)
Frida_Spanish = stri_remove_empty(Frida_Spanish, TRUE)

# Creating a data frame from each vector

subtitles_Frida_English = tibble(subtitle = seq_along(Frida_English),
                  text = Frida_English)

subtitles_Frida_Spanish = tibble(subtitle = seq_along(Frida_Spanish),
                                   text = Frida_Spanish)

# Tokenizing each text

words_Frida_English = subtitles_Frida_English %>%
  unnest_tokens(word, text)

words_Frida_Spanish = subtitles_Frida_Spanish %>%
  unnest_tokens(word, text)

# Dividing each text into sentences and counting number of words

sentences_Frida_English = subtitles_Frida_English %>%
  unnest_tokens(sentences, text, token = "sentences") %>%
  mutate(words = str_count(sentences,
                            pattern = "\\w+"))

sentences_Frida_Spanish = subtitles_Frida_Spanish %>%
  unnest_tokens(sentences, text, token = "sentences") %>%
  mutate(words = str_count(sentences,
                              pattern = "\\w+"))

# Counting number of tokens per text

ntokens_Frida_English = words_Frida_English %>%
  count(word, sort = TRUE) %>%
  mutate(f_relative = n / sum(n)) 

ntokens_Frida_Spanish = words_Frida_Spanish %>%
  count(word, sort = TRUE) %>%
  mutate(f_relative = n / sum(n))

# Graph: Average number of words by subtitle sentence

movies = c("Frida_English","Frida_Spanish")
means = c(mean(sentences_Frida_English$words), mean(sentences_Frida_Spanish$words))
movies_data = data.frame(movies, means)
movies_data$movies = as.factor(movies_data$movies)

movies_graph = ggplot(data = movies_data, aes(x= reorder(movies, -means), y= means, fill = movies)) +
  geom_bar(stat="identity", width=0.5) + ggtitle("Average number of words by subtitle sentence") + xlab("Movie") +
  ylab("Average number of words") + theme_bw() + scale_y_continuous(limits=c(0, 8)) + 
  geom_hline(yintercept = mean(means), linetype = "dashed", colour = "black", size = 0.5) + 
  theme(legend.position = "none")

# Graph: Distribution of words by subtitle line

sentences_Frida_English$movie = "Frida_English"
sentences_Frida_Spanish$movie = "Frida_Spanish"

movies_words_data = rbind(sentences_Frida_English, sentences_Frida_Spanish)

graph_movies_words = ggplot(data = movies_words_data, aes(x = movie, y = words, fill = movie)) + 
  geom_violin() + geom_boxplot(width=0.1, fill = "white") + theme_bw() + ggtitle("Distribution of words by subtitle line") + 
  xlab("Movie") +
  ylab("Distribution of words") + theme(legend.position = "none")

# Removing stopwords

stopwords_es = get_stopwords("es")
stopwords_en = get_stopwords("en")

words_Frida_English_empty = words_Frida_English %>%
  anti_join(stopwords_en)
words_Frida_English_empty$movie = "Frida_English"

words_Frida_Spanish_empty = words_Frida_Spanish %>%
  anti_join(stopwords_es)
words_Frida_Spanish_empty$movie = "Frida_Spanish"

# Most frequent words by movie

words_Frida_Spanish_empty_freq = words_Frida_Spanish_empty %>%
  count(word, sort = TRUE) %>%
  filter(n > 20)
words_Frida_Spanish_empty_freq$movie = "Frida_Spanish"

words_Frida_English_empty_freq = words_Frida_English_empty %>%
  count(word, sort = TRUE) %>%
  filter(n > 20)
words_Frida_English_empty_freq$movie = "Frida_English"

words_movies_empty_data = rbind(words_Frida_Spanish_empty_freq, 
                                          words_Frida_English_empty_freq)

graph_words_frequency = words_movies_empty_data %>%
  ggplot(aes(reorder(word, n), n, fill = movie)) + geom_bar(stat = "identity") +
  facet_wrap(~movie, scales = "free") + labs(x = "", y = "") + coord_flip() + 
  theme_bw() + ggtitle("Words appearing more than 20 times") +
  theme(legend.position = "none")

# Bi-gram analysis

  # Frida (Spanish)

bigr_Frida_Spanish = subtitles_Frida_Spanish %>%
  unnest_tokens(bigram,
                text,
                token = "ngrams",
                n = 2)

bigr_separate_Frida_Spanish = bigr_Frida_Spanish %>%
  separate(bigram,
           c("word1", "word2"),
           sep = " ")

bigr_filtered_Frida_Spanish = bigr_separate_Frida_Spanish %>%
  filter(!word1 %in% stopwords_es$word,
         !word2 %in% stopwords_es$word)

bigrams_Frida_Spanish = bigr_filtered_Frida_Spanish %>%
  count(word1, word2, sort = T) %>%
  filter(n < 50)

grafo_bigr_Frida_Spanish = bigrams_Frida_Spanish %>%
  filter(n >= 2) %>%
  graph_from_data_frame()

ggraph(grafo_bigr_Frida_Spanish, layout = "nicely") +
  geom_edge_link(aes(edge_alpha = n),
                 show.legend = FALSE,
                 arrow = arrow(type = "closed",
                               length = unit(3, "mm"))) +
  geom_node_point(color = "red", size = 3) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

  # Frida (English)

bigr_Frida_English = subtitles_Frida_English %>%
  unnest_tokens(bigram,
                text,
                token = "ngrams",
                n = 2)

bigr_separate_Frida_English = bigr_Frida_English %>%
  separate(bigram,
           c("word1", "word2"),
           sep = " ")

bigr_filtered_Frida_English = bigr_separate_Frida_English %>%
  filter(!word1 %in% stopwords_en$word,
         !word2 %in% stopwords_en$word)

bigrams_Frida_English = bigr_filtered_Frida_English %>%
  count(word1, word2, sort = T) %>%
  filter(n < 50)

grafo_bigr_Frida_English = bigrams_Frida_English %>%
  filter(n >= 2) %>%
  graph_from_data_frame()

ggraph(grafo_bigr_Frida_English, layout = "nicely") +
  geom_edge_link(aes(edge_alpha = n),
                 show.legend = FALSE,
                 arrow = arrow(type = "closed",
                               length = unit(3, "mm"))) +
  geom_node_point(color = "red", size = 3) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

# Sentiment analysis

  # Frida (Spanish)

names(words_Frida_Spanish)[names(words_Frida_Spanish) == "word"] = "palabra"
words_Frida_Spanish$id = seq.int(nrow(words_Frida_Spanish))

sentiment_Frida_Spanish = words_Frida_Spanish %>%
  mutate(indice = id %/% 100 + 1) %>%
  inner_join(get_sentiments("nrc")) %>%
  count(indice=indice, sentimiento) %>%
  ungroup() %>%
  spread(sentimiento, n, fill = 0) %>%
  mutate(sentimiento = positivo - negativo) %>%
  ggplot(aes(indice, sentimiento)) +
  geom_bar(stat = "identity", alpha = 0.8,
           color = "indianred2", fill = "indianred2") +
  theme_bw() + labs(x = "Movie progress",
                    y = "Sentiment transformation") +
  ggtitle(expression(paste("Sentiment analysis in the Spanish version of ",
                           italic("Frida"))))

  # Frida (English)

words_Frida_English$id = seq.int(nrow(words_Frida_English))

sentiment_Frida_English = words_Frida_English %>%
  mutate(index = id %/% 100 + 1) %>%
  inner_join(get_sentiments("nrc")) %>%
  count(index=index, sentiment) %>%
  ungroup() %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>%
  ggplot(aes(index, sentiment)) +
  geom_bar(stat = "identity", alpha = 0.8,
           color = "indianred2", fill = "indianred2") +
  theme_bw() + labs(x = "Movie progress",
                       y = "Sentiment transformation") +
  ggtitle(expression(paste("Sentiment analysis in the English version of ",
                           italic("Frida"))))
















