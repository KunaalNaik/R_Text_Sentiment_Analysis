library(dplyr)
library(tidytext)
library(RColorBrewer)
library(ggplot2)
library(wordcloud)
library(tm)
options(warn=-1)

dataset <- read.csv('C:\\Users\\DELL\\Documents\\Jigsaw Courses\\Courses\\IIM Marketing and E-Commerce\\Sentiment Analysis\\Airlines\\Tweets.csv')

str(dataset)

dataset$text <- as.character(dataset$text)
tidy_dataset <- dataset %>%
  unnest_tokens(word, text)

summary(dataset$airline_sentiment)

# Visualization of whether the sentiment of the tweets was positive, neutral, or negative for each airlines
ggplot(dataset, aes(x = airline_sentiment, fill = airline_sentiment)) +
  geom_bar() +
  facet_grid(. ~ airline) +
  theme(axis.text.x = element_text(angle=65, vjust=0.6),
        plot.margin = unit(c(3,0,3,0), "cm"))


#The Most Frequent Words in Positive Sentiment
positive <- tidy_dataset %>% 
  filter(airline_sentiment == "positive")


list <- c("to", "the","i", "a", "you", "for", "on", "and", "is", "are", "am", 
          "my", "in", "it", "me", "of", "was", "your", "so","with", "at", "just", "this",
          "http", "t.co", "have", "that", "be", "from", "will", "we", "an", "can")

positive <- positive %>%
  filter(!(word %in% list))


wordcloud(positive[,15],
          max.words = 100,
          random.order=FALSE, 
          rot.per=0.30, 
          use.r.layout=FALSE, 
          colors=brewer.pal(10, "Blues"))

positive <- positive %>%
  count(word, sort = TRUE) %>%
  rename(freq = n)

head(positive, 21)
