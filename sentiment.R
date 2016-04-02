setwd("~/Documents/Python-Projects/sentiment_analysis/airline-twitter-sentiment")
# Q. how can we predict the sentiment of tweets?
library("RSQLite")

# Import Data from SQL --------
# connect to the sqlite file
con <- dbConnect(drv=RSQLite::SQLite(), dbname="database.sqlite")

alltables = dbListTables(con) # get a list of all tables
alltables

# get the Tweets table as a data.frame
p1 = dbGetQuery( con,'select * from Tweets' )
# count the number of Tweets in the SQLite table
p2 = dbGetQuery( con,'select count(*) from Tweets' )

#Select Tweets about Delta Airline
p4 = dbGetQuery(con, "select * from Tweets where airline like '%Delta%'")

#Exploratory Analysis ---------
dim(p1) # displays dimensions of data
str(p1) # displays structure of data

#The dataset contains 14,485 tweets and 15 columns of variables

# Q. What is the sentiment of these tweets?
# let's look at airline sentiment column 
table(p1$airline_sentiment)/dim(p1)[1] #table of sentiment by proportion
# Most tweets are labelled as negative

# Let's visualize this. 
sumData = as.data.frame(table(p1$airline_sentiment)/dim(p1)[1])
colnames(sumData) <- c('Sentiment','Frequency')
library(ggplot2)
gpie = ggplot(sumData, aes(x = "", y = Frequency, fill = Sentiment))
gpie + geom_bar(stat = 'identity') + coord_polar("y", start = 0) +
  theme(axis.title.x = element_blank()) + geom_text(aes(y = Frequency/3 + c(0, cumsum(Frequency)[-length(Frequency)]),
                                                        label = round(Frequency, 2)), size = 4) + ggtitle('Tweet Sentiment')

# Q. How many tweets does each airline have? What is the general sentiment?
table(p1$airline)/dim(p1)[1]
# United as the most tweets, Virgin America has the least tweets
sumData = as.data.frame(table(p1$airline,p1$airline_sentiment)/dim(p1)[1])
colnames(sumData)<-c('Airline', 'Sentiment','Share_Tweets')
gbar = ggplot(sumData, aes(x = Airline, y = Share_Tweets, fill = Sentiment)) + ggtitle('Proportion of Tweets per Airline') +
  theme(plot.title = element_text(size = 14, face = 'bold', vjust = 1), axis.title.x = element_text(vjust = -1))
gbar + geom_bar(stat = 'identity', position = 'fill')
# Tweets about United, US Air, and American are mostly negative.
# Virgin America has the largest proportion of positive tweets

# Q. Why are all these tweets so negative?
sumData<-as.data.frame(table(p1$negativereason)/dim(p1)[1])
colnames(sumData)<- c('Reason','Share_Tweets')
gpie = ggplot(sumData, aes(x = "", y = Share_Tweets, fill = Reason))
gpie + geom_bar(stat = 'identity') + coord_polar("y", start = 0) +
  theme(axis.title.x = element_blank()) + geom_text(aes(y = Share_Tweets/3 + c(0, cumsum(Share_Tweets)[-length(Share_Tweets)]),
                                                        label = round(Share_Tweets, 2)), size = 4) + ggtitle('Negative Tweet Reasons')
#  37% of tweets have no specified reason
# of the specified reasons customer service issue and late flight most common

# Q. what are the reasons for negative tweets for each airline

# Q. are these sentiments wide spread? what do retweets tell us?

# Q. how much data is missing (NAs)?

# Q. what do the tweet locations tell us?

# Q. what does the time tell us?

# Exploratory Data Analysis Summary
# most tweets are negative
# United as the most tweets, Virgin America has the least tweets
# Tweets about United, US Air, and American are mostly negative.
# Virgin America has the largest proportion of positive tweets