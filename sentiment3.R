# I question whether we can use a larger lexicon to prediction twitter sentiment 
# maybe some of the tweets will be categorized differently.

# sentiment analysis is ...
# detecting attitudes of a person(twitter user) to a target(airline)
# simpliest task: is it positive or negative - polarity detection

# 
# step 1 of 3 : tokenism - preprocessing 
## examples include: identifying words in all caps as shouting , isolating emoticons
## another example: add NOT in front of negation words

# step 2 of 3: feature extraction / prediction model - does the word exist?
# naiveBayes - binary multinomial 
## which means algorithm only counts whether word occurs rather than how many times it occurs in a tweet
## for this reason we remove all duplicates in all tweets
# cross validation break up data into 10 folds

# step 3 of 3: validation - compare model against test set


# Future work: other algorithms to try include maxENT and SVM
# more advanced : rank the attitude 1 to 5