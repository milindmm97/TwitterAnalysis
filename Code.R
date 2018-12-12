library(stringr)
library(twitteR)
library(xlsx)
library(plyr)

api_key<- "SfOXeq5JkZfGhK4MkEpzIExqB"
api_secret <- "6RdpLsQlzxUXeNS84PlrfKedJ0iuXN1cCbpU0AZx65kgcHsdta"
access_token <- "1009310760-BMJia7jbEmhA0mMWOfcdt6U6w16U8xb4OEtmZOd"
access_token_secret <- "xbkxGbH2e4z4pLFuoTvAMOXzwdR1ccGycFP9sCTJxX9aW"
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

setwd() #open folder containing pos and neg words txt
neg = scan("negative-words.txt", what="character", comment.char=";")
pos = scan("positive-words.txt", what="character", comment.char=";")
neg = c(neg,'corruption')
neg = c(neg, 'wtf')
tweets = searchTwitter('Modi',n=1000)
Tweets.text = laply(tweets,function(t)t$getText()) # gets text from Tweets


score.sentiment = function(tweets, pos.words, neg.words)
  
  
{
  
  require(plyr)
  require(stringr)
  
  scores = laply(tweets, function(tweet, pos.words, neg.words) {
    
    
    
    tweet = gsub('https://','',tweet) # removes https://
    tweet = gsub('http://','',tweet) # removes http://
    tweet=gsub('[^[:graph:]]', ' ',tweet) ## removes graphic characters 
    #like emoticons 
    tweet = gsub('[[:punct:]]', '', tweet) # removes punctuation 
    tweet = gsub('[[:cntrl:]]', '', tweet) # removes control characters
    tweet = gsub('\\d+', '', tweet) # removes numbers
    tweet=str_replace_all(tweet,"[^[:graph:]]", " ") 
    
    tweet = tolower(tweet) # makes all letters lowercase
    
    word.list = str_split(tweets, '\\s+') # splits the tweets by word in a list
    
    words = unlist(word.list) # turns the list into vector
    
    pos.matches = match(words, pos.words) ## returns matching 
    #values for words from list 
    neg.matches = match(words, neg.words)
    
    pos.matches = !is.na(pos.matches) ## converts matching values to true of false
    neg.matches = !is.na(neg.matches)
    
    score = sum(pos.matches) - sum(neg.matches) # true and false are 
    #treated as 1 and 0 so they can be added
    
    return(score)
    
  }, pos.words, neg.words )
  
  scores.df = data.frame(score=scores, text=tweets)
  
  return(scores.df)
  
}

analysis = score.sentiment(Tweets.text, pos, neg) 