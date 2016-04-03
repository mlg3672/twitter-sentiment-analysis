setwd("~/Documents/Python-Projects/sentiment_analysis")
# Q. what is the text of the airline tweets?
# Q. how can we predict the sentiment of tweets?
library("RSQLite")

# Import Data from SQL --------
setwd("~/Documents/Python-Projects/sentiment_analysis/airline-twitter-sentiment")

# connect to the sqlite file
con <- dbConnect(drv=RSQLite::SQLite(), dbname="database.sqlite")

alltables = dbListTables(con) # get a list of all tables
alltables

# get the Tweets table as a data.frame
p1 = dbGetQuery( con,'select * from Tweets' )
# count the number of Tweets in the SQLite table

library(dplyr)
# only look at columns with sentiment, reason, airline, text
data = select(p1,airline_sentiment, negativereason, airline, text)
head(data)

# determine word frequency for each sentiment
# split into positive, negative tweets
positive = data[data$airline_sentiment=='positive',]
negative = data[data$airline_sentiment == 'negative',]
neutral = data[data$airline_sentiment == 'neutral',]
empty = data[data$airline_sentiment == '',]
library(tm); library(SnowballC)
library(wordcloud)

# several words appear often that are not clearly positive or negative
wordsToRemove = c('americanair', 'virginamerica', 'much','will' , 'southwestair','now','cant', 'can', 'ive', 'jetblue','just','get','got','usairways','united')

# generate a function to analyse corpus text
analyseText = function(text_to_analyse){
  # analyse text and generate matrix of words
  # Returns a dataframe containing 1 tweet per row, one word per column
  # and the number of times the word appears per tweet
  CorpusTranscript = Corpus(VectorSource(text_to_analyse))
  CorpusTranscript = tm_map(CorpusTranscript, content_transformer(tolower), lazy = T)
  CorpusTranscript = tm_map(CorpusTranscript, PlainTextDocument, lazy = T)
  CorpusTranscript = tm_map(CorpusTranscript, removePunctuation)
  CorpusTranscript = tm_map(CorpusTranscript, removeWords, wordsToRemove)
  CorpusTranscript = tm_map(CorpusTranscript, removeWords, stopwords("english"))
  CorpusTranscript = DocumentTermMatrix(CorpusTranscript)
  CorpusTranscript = removeSparseTerms(CorpusTranscript, 0.97) # keeps a matrix 97% sparse
  CorpusTranscript = as.data.frame(as.matrix(CorpusTranscript))
  colnames(CorpusTranscript) = make.names(colnames(CorpusTranscript))
  
  return(CorpusTranscript)
}
# analyze txt of negative words ----
words = analyseText(negative$text)
dim(words)
neg_words=colSums(words)
neg_words<-neg_words[order(neg_words,decreasing = T)]
# top 10 negative words in order of greatest to least frequent are flight, cancelled, service, hours, help, hold, customer, time, plane, and delayed

# analyze txt of positive words -----
words = analyseText(positive$text)
dim(words)
pos_words=colSums(words)
pos_words<-pos_words[order(pos_words,decreasing = T)]
# top 10 positive words in order of greatest to least frequent are thank / thanks, flight, great, service, love, customer, guys, good, best, and awesome
# combine thank and thanks
pos_words[1]<-sum(pos_words[1],pos_words[2])
pos_words<-pos_words[-2]

# analyze txt of neutral words ----
words = analyseText(neutral$text)
dim(words)
neu_words=colSums(words)
neu_words<-neg_words[order(neu_words,decreasing = T)]
neutral$text[1:15] #look at first 15 tweets
# a few of the neutral tweets seem miscategorized.

# word clouds
par(mfrow = c(1,2))

wordcloud(freq = as.vector(neg_words), words = names(neg_words),random.order = FALSE,
          random.color = FALSE, colors = brewer.pal(9, 'Blues')[4:9])

wordcloud(freq = as.vector(pos_words), words = names(pos_words),random.order = FALSE,
          random.color = FALSE, colors = brewer.pal(9, 'Greens')[4:9])


# Summary of Text analysis
# # top 10 negative words in order of greatest to least frequent are flight, cancelled, service, hours, help, hold, customer, time, plane, and delayed
# top 10 positive words in order of greatest to least frequent are thank / thanks, flight, great, service, love, customer, guys, good, best, and awesome
# top 10 neutral words in order of greatest to least frequent are bag, don't, call, cancelled, delayed, flight, amp, flightled, back, customer
