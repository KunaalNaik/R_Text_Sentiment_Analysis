
#Sample Text
text <- c("Because I could not stop for Death -",
          "He kindly stopped for me -",
          "The Carriage held but just Ourselves -",
          "and Immortality")

#Whats in "text"
text

#Convert to tibble dataframe for dplyr
library(dplyr)
text_df <- tibble(line = 1:4, text = text)

#Check DataFrame
text_df


#Now lets make into one-token-per-row format
library(tidytext)
text_df_word <- text_df %>%
  unnest_tokens(word, text)

text_df_no_stop_words <- text_df_word %>%
  anti_join(stop_words)

# https://rdrr.io/cran/tidytext/man/unnest_tokens.html


#lets Analyze a Books by Jane Austen
library(janeaustenr)
library(dplyr)
library(stringr)

raw_book <- austen_books()

#create some columns
original_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, 
                                     regex("^chapter [\\divxlc]",
                                           ignore_case = TRUE)))) %>%
  ungroup()

#Check DataFrame
original_books

#Now lets make into one-token-per-row format
tidy_books <- original_books %>%
  unnest_tokens(word, text)

#Check data
tidy_books

#Remove Stop Words
data(stop_words)

tidy_books <- tidy_books %>%
  anti_join(stop_words)

# Count of Word Occurance
tidy_books %>%
  count(word, sort = TRUE) 

#Lets plot it
library(ggplot2)

tidy_books %>%
  count(word, sort = TRUE) %>%
  filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)


### Sentiments
library(tidytext)

get_sentiments("afinn")

get_sentiments("bing")

get_sentiments("nrc")

#Lets Analyse the Books
tidy_books <- austen_books() %>%
  group_by(book) %>%
  mutate(
    linenumber = row_number(),
    chapter = cumsum(str_detect(text, 
                                regex("^chapter [\\divxlc]", 
                                      ignore_case = TRUE)))) %>%
  ungroup() %>%
  unnest_tokens(word, text)

#get Sentiment - Joy
nrc_joy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")

tidy_books %>%
  filter(book == "Emma") %>%
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE)

# Most Common Positive and Neative Words
bing_word_counts <- tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

bing_word_counts

bing_word_counts %>%
  group_by(sentiment) %>%
  slice_max(n, n = 10) %>% 
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL)


#Wordclouds
library(wordcloud)

tidy_books %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

# Negative
library(reshape2)

tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)
