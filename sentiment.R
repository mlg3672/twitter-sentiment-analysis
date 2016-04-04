setwd("~/Documents/Python-Projects/sentiment_analysis/airline-twitter-sentiment")
# Q. how can we predict the sentiment of tweets?
# Q. what columns/value/trends are in the data given?
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

# Q. What is the sentiment of these tweets? -----
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

# Q. Why are all these tweets so negative? ----
sumData<-as.data.frame(table(p1$negativereason)/dim(p1)[1])
colnames(sumData)<- c('Reason','Share_Tweets')
gpie = ggplot(sumData, aes(x = "", y = Share_Tweets, fill = Reason))
gpie + geom_bar(stat = 'identity') + coord_polar("y", start = 0) +
  theme(axis.title.x = element_blank()) + geom_text(aes(y = Share_Tweets/3 + c(0, cumsum(Share_Tweets)[-length(Share_Tweets)]),
                                                        label = round(Share_Tweets, 2)), size = 4) + ggtitle('Negative Tweet Reasons')
#  37% of tweets have no specified reason. Of the specified reasons customer service issue and late flight most common

# Q. what are the reasons for negative tweets for each airline
sumData <- as.data.frame(table(p1$negativereason,p1$airline)/dim(p1)[1])
colnames(sumData)<-c('Reason','Airline','Share_Tweets')
gbar = ggplot(sumData, aes(x = Airline, y = Share_Tweets, fill = Reason)) + ggtitle('Share of Negative Tweets per Airline') +
  theme(plot.title = element_text(size = 14, face = 'bold', vjust = 1), axis.title.x = element_text(vjust = -1))
gbar + geom_bar(stat = 'identity', position = 'fill')
# No reason given for half of Delta, Southwest, Virgin America tweets
# American, US Airways tweets negative due to customer service issues
# United equal tweets devoted to customer service issues and late flight

# Q. are these sentiments wide spread? what do retweets tell us?
table(p1$retweet_count,p1$airline_sentiment)[1,]/table(p1$airline_sentiment)
# negative tweets are retweeted more often than neutral and positive tweets
sumData<-as.data.frame(table(p1$retweet_count,p1$airline_sentiment))
colnames(sumData)<-c('retweets','sentiment','retweet_count')
negRetweets<-sum(as.integer(sumData[sumData$sentiment=='negative',1])*as.integer(sumData[sumData$sentiment=='negative',3]))
posRetweets<-sum(as.integer(sumData[sumData$sentiment=='positive',1])*as.integer(sumData[sumData$sentiment=='positive',3]))
negRetweets/posRetweets
#  negative retweets are 4 times more than positive retweets
table(p1$retweet_count)/dim(p1)[1]
# most tweets (94.7%) are not retweeted.
table(p1$retweet_count,p1$airline)[1,]/colSums(table(p1$retweet_count,p1$airline))
# United most retweets, American and Virgin American the least retweets
table(p1$retweet_count)
# there are 6 tweets that have been retweeted more than 20 times. Why?
p1[p1$retweet_count>20,]$text
# The first tweet talks about a flight delay on US Airways
# The second tweet is negative but not clear what happened on US Airways.
# The third tweet is positive refering to Southwest
# The forth and fifth tweeets complain of the same thing. They both refer to the language in an ad. They are both negative sentiments towards Delta.
 
# Q. how much data is missing (NAs)?-----
library(plyr)
# fill with NA cells in dataframe containing "", " " or the string NA
p1 = as.data.frame(apply(p1, 2, function(x) gsub("^$|^ $", NA, x)))
apply(p1, 2, function(x) sum(is.na(x)))/dim(p1)[1]
# airline_sentiment_gold, negativereason_gold, tweet_coord are mostly empty
# 40-30% values also missing from negativereason, tweet_location, and user_timezone

# Q. what do the tweet locations/timezone tell us?-------
sumData<-as.data.frame(table(p1$user_timezone))
colnames(sumData) = c('timezone', 'Frequency')
sumData = sumData[order(sumData$Frequency, decreasing = TRUE),]
dim(sumData)
head(sumData, 8)
# most tweets from Eastern U.S., top 75% of tweets from U.S., 7% from Quito capital of Ecudaor?!
table(p1$user_timezone,p1$airline)

tail(unique(p1$tweet_location))
# tweet_location variable is very messy, unformatted, not useful
head(unique(p1$tweet_coord))
# only 7% of data have coordinates, not enough to be useful

# Visualize location of Tweets -----
# clean up names for geocoding
sumData$timezone<-as.character(sumData$timezone)
sumData$timezone[4]<-c('Atikokan, Canada')
sumData$timezone[5]<-c('Boise, ID')
sumData$timezone[6]<-c('Chicago, IL')
sumData$timezone[7]<-c('Detroit, MI')
sumData$timezone[8]<-c('Los Angeles,CA')
sumData$timezone[63]<-c('Los Angeles, CA') # Pacific Time
sumData$timezone[31]<-c("New York, NY") #Eastern Time
sumData$timezone[58]<-c("Arizona") # Mountain Time
sumData$timezone[28]<-c("Boise, ID") # Central Time
sumData$timezone[9]<-c("New York, NY") 
# change timezone to coordinates
library(ggmap)
dat<-geocode(as.character(sumData$timezone)) #geocode timezones
sumData$long<-dat[,1]
sumData$lat<-dat[,2]
dim(sumData)
require(maps)
world_map <- map_data("world")
g1 = ggplot()
g1 = g1 + geom_polygon(data=world_map, aes(x=long, y=lat, group = group), colour="black", fill = 'lightblue') + 
  ggtitle("Location of tweets across the World")
g1 = g1 + geom_point(data=sumData, aes(x=long, y=lat, size = sumData$Frequency), color="coral1") + scale_size(name="Total Tweets")
g1 + ylim(-50, 80)

#isolate US only
USdata<-sumData[c(3:11,28,31,58,63),]
states <- map_data("state")
g2 = ggplot()
g2 =g2 + geom_polygon(data=states, aes(x=long, y=lat, group = group), colour="black", fill = 'lightblue') + 
  ggtitle("Location of tweets across the States")
g2 = g2 + geom_point(data=sumData, aes(x=long, y=lat, size = sumData$Frequency), color="coral1") + scale_size(name="Total Tweets")
g2 + xlim(-125, -65) + ylim(25, 50)

# Exploratory Data Analysis Summary ------
# most tweets are negative
# United has the most tweets, Virgin America has the least tweets
# Tweets about United, US Air, and American are mostly negative.
# Virgin America has the largest proportion of positive tweets
#  37% of tweets have no specified reason. Of the specified reasons customer service issue and late flight most common

# No reason given for half of Delta, Southwest, Virgin America negative tweets
# American, US Airways tweets negative due to customer service issues
# United equal tweets devoted to customer service issues and late flight
# most tweets (94%) are not retweeted.
# most tweets from Eastern U.S., top 75% of tweets from U.S., 7% from Quito capital of Ecudaor?!
# airline_sentiment_gold, negativereason_gold, tweet_coord are mostly empty
# 40-30% values also missing from negativereason, tweet_location, and user_timezone
# top retweets are negative concerning Delta and US Air