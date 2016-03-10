import re


# new function
# pre process tweets 
nTts = list()
for x in tweets:
    #to take out special characters @,!,#
    twt = re.sub('[^A-Za-z0-9]+', ' ', x)
    # make all lower case
    twt = twt.lower()
    # take out stopWords
    if stopWords in twt:
        #twt - stopWords fix
    nTts.append(twt)
print(nTts)   


stopWords = ['a','and','the','i','it','this']

tweets = [('hey @shawn i found this website it is awesome! #skatetricks'), ('@Shorty so happy got a new car today #birthday'), ('i have great friends #friends')]