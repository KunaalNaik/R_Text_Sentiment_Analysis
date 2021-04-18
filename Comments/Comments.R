# This R program is designed and test for whatsapp chats(group or individual). 
#You can export the chats from whatsapp to your email ID and download it to your PC and work on it.
# It is defined by the kaggle/python docker image: https://github.com/kaggle/docker-python
library(ggplot2)
library(lubridate)
library(scales)
library(reshape2)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(stringr)
library(syuzhet) 
library(dplyr ) 

#get the data from whatsapp chat 
text <- readLines("C:\\Users\\DELL\\Documents\\Jigsaw Courses\\Courses\\IIM Marketing and E-Commerce\\Sentiment Analysis\\Comments\\GBcomments.csv", n = 10000)
#Convert the file to UTF-8 encoding
s2 <- iconv(text, "UTF-8", "ASCII", sub = "")
text <- s2


#let us create the corpus
docs <- Corpus(VectorSource(text))

#clean our chat data
trans <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, trans, "/")
docs <- tm_map(docs, trans, "@")
docs <- tm_map(docs, trans, "\\|")
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, stemDocument)

#create the document term matrix
dtm <- TermDocumentMatrix(docs)
mat <- as.matrix(dtm)
v <- sort(rowSums(mat),decreasing=TRUE)

#Data frame
data <- data.frame(word = names(v),freq=v)


#generate the wordcloud
set.seed(1056)
wordcloud(words = data$word, freq = data$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))


#fetch sentiment words from texts
Sentiment <- get_nrc_sentiment(text)
head(Sentiment)
text <- cbind(text,Sentiment)

#count the sentiment words by category
TotalSentiment <- data.frame(colSums(text[,c(2:11)]))
names(TotalSentiment) <- "count"
TotalSentiment <- cbind("sentiment" = rownames(TotalSentiment), TotalSentiment)
rownames(TotalSentiment) <- NULL

#total sentiment score of all texts
ggplot(data = TotalSentiment, aes(x = sentiment, y = count)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Total Count") + ggtitle("Total Sentiment Score")