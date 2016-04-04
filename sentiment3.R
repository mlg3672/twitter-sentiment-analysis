# I question whether we can use a larger lexicon to prediction twitter sentiment 
# maybe some of the tweets will be categorized differently.

# sentiment analysis is ...
# detecting attitudes of a person(twitter user) to a target(airline)
# simpliest task: is it positive or negative - polarity detection

setwd("~/Documents/Python-Projects/sentiment_analysis")
# upload lexicon --------
lexicon<-read.table(file = "inqdict.txt",header = F, sep = ' ',stringsAsFactors = F, fill=T)
str(lexicon)
dim(lexicon)
# contains 10,093 words, the third row contains sentiment
orderedLex<-table(lexicon$V3)[order(table(lexicon$V3),decreasing=T)]
head(orderedLex)
# most words are without a sentiment, 1,261 negative words and 1,099 positive words
# Let's isolate the words and sentiment only
words<-data.frame(sentiment=lexicon$V3,words =lexicon$V1,stringsAsFactors = F)
str(words)
words$words <- tolower(words$words)# make words lower case
# then, sort negative and positive words
negTerms<-data.frame(words[words$sentiment=='Neg',])
dim(negTerms) # contains 1,261 negative words
posTerms<-data.frame(words[words$sentiment=='Pos',])
dim(posTerms) # contains 1,099 positive words
neutral<-data.frame(words[words$sentiment=='',])
dim(neutral) # contains 4,258 neutral words
travel_words<-data.frame(words[words$sentiment=='Travel',])
travel_words # there are 7 words in the category of travel 


# step 1 of 3 : tokenisation (breaking tweet into words) - preprocessing 
## examples include: identifying words in all caps as shouting , isolating emoticons
## another example: add NOT in front of negation words
tokens<-read.table(file = "dict.csv",header = T, sep = '\t',stringsAsFactors = F, fill=T)
posText<-tokens[tokens$sentiment=='positive',]
negText<-tokens[tokens$sentiment=='negative',]
# step 2 of 3: feature extraction 
#function to calculate number of words in each category within a sentence
sentimentScore <- function(sentences, vNegTerms, negTerms, posTerms, vPosTerms){
  final_scores <- matrix('', 0, 5)
  scores <- laply(sentences, function(sentence, vNegTerms, negTerms, posTerms, vPosTerms){
    initial_sentence <- sentence
    #remove unnecessary characters and split up by word 
    sentence <- gsub('[[:punct:]]', '', sentence)
    sentence <- gsub('[[:cntrl:]]', '', sentence)
    sentence <- gsub('\\d+', '', sentence)
    sentence <- tolower(sentence)
    wordList <- str_split(sentence, '\\s+')
    words <- unlist(wordList)
    #build vector with matches between sentence and each category
    vPosMatches <- match(words, vPosTerms)
    posMatches <- match(words, posTerms)
    vNegMatches <- match(words, vNegTerms)
    negMatches <- match(words, negTerms)
    #sum up number of words in each category
    vPosMatches <- sum(!is.na(vPosMatches))
    posMatches <- sum(!is.na(posMatches))
    vNegMatches <- sum(!is.na(vNegMatches))
    negMatches <- sum(!is.na(negMatches))
    score <- c(vNegMatches, negMatches, posMatches, vPosMatches)
    #add row to scores table
    newrow <- c(initial_sentence, score)
    final_scores <- rbind(final_scores, newrow)
    return(final_scores)
  }, vNegTerms, negTerms, posTerms, vPosTerms)
  return(scores)
}    

#build tables of positive and negative sentences with scores
posResult <- as.data.frame(sentimentScore(posText, vNegTerms, negTerms, posTerms, vPosTerms))
negResult <- as.data.frame(sentimentScore(negText, vNegTerms, negTerms, posTerms, vPosTerms))
posResult <- cbind(posResult, 'positive')
colnames(posResult) <- c('sentence', 'vNeg', 'neg', 'pos', 'vPos', 'sentiment')
negResult <- cbind(negResult, 'negative')
colnames(negResult) <- c('sentence', 'vNeg', 'neg', 'pos', 'vPos', 'sentiment')    

#combine the positive and negative tables
results <- rbind(posResult, negResult)
# step 3 of 4: prediction model - does the word exist?
# naiveBayes - binary multinomial 
## which means algorithm only counts whether word occurs rather than how many times it occurs in a tweet
## for this reason we remove all duplicates in all tweets
# cross validation break up data into 10 folds

# step 3 of 3: validation - compare model against test set


# Future work: other algorithms to try include maxENT and SVM
# more advanced : rank the attitude 1 to 5