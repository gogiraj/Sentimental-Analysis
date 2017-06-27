## **** Twitter Sentiment Analysis for Airlines and comparing them  **************

# load the package
library(devtools)
library(plyr)
library(twitteR)
library(stringr)
library(ggplot2)
library(doBy)

# install.packages("doBy")
# get the 500 most recent tweets mentioning '@delta':

# sessionInfo()
api_key <- "33IdKjB0fdRvi9ip3MnvzMinh"
api_secret <- "WIOwnxsLTpN5xvGYzaC9TiADM2RRew4maYtnYPVSxLGKZSOFpi"
access_token <- "47717736-IF0PhKjohLlJEd6xK2yWY5H2FQH3vRvd7neT9dGjz"  
access_token_secret <- "pJLM1zeiAzlVWVCGaLSMltDAfvEzdsYpRntiUoNW4ENEg"

setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)


delta.tweets = searchTwitter('@delta', n=1000)
delta.tweets1 = searchTwitter('@AmericanAir', n=1000)
delta.tweets2 = searchTwitter('@united', n=1000)

length(delta.tweets)
class(delta.tweets)

tweet = delta.tweets[[1]]
class(tweet)

tweet$getScreenName()
tweet$getText()

tweet

## Extract the tweet text
## The function name is determined by the input and output data types. We
## have a list and would like a simple array output, so we use "laply":

delta.text = laply(delta.tweets, function(t) t$getText() )
delta.text1 = laply(delta.tweets1, function(t) t$getText() )
delta.text2 = laply(delta.tweets2, function(t) t$getText() )



length(delta.text)[1] 
head(delta.text, 5)

setwd("C:/Users/Gagan/Desktop/MUIT/Data Mining/textminingcodespptoutputplots")
getwd()

## Load sentiment word lists
Gd.neg = scan('negative-words.txt', what='character', comment.char=';')
Gd.pos = scan('positive-words.txt', what='character', comment.char=';')

## Adding few industry-specific and/or especially emphatic terms
pos.words = c(Gd.pos, 'upgrade')
neg.words = c(Gd.neg, 'wtf', 'wait', 'waiting', 'epicfail', 'mechanical')

### SAMPLE
sample = c("You're awesome and I love you",
           "I hate and hate and hate. So angry. Die!",
           "Impressed and amazed: you are peerless in your achievement of
unparalleled mediocrity.")

score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr);
  require(stringr);
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    sentence = gsub('[^A-z ]','', sentence)
    sentence = tolower(sentence);
    word.list = str_split(sentence, '\\s+');
    words = unlist(word.list);
    pos.matches = match(words, pos.words);
    neg.matches = match(words, neg.words);
    pos.matches = !is.na(pos.matches);
    neg.matches = !is.na(neg.matches);
    score = sum(pos.matches) - sum(neg.matches);
    return(score);
  }, pos.words, neg.words, .progress=.progress );
  scores.df = data.frame(score=scores, text=sentences);
  return(scores.df);
}


## sample = score.sentiment(sample, pos.words, neg.words)
## sample ## Sample sentiment analysis result

result = score.sentiment(delta.text, pos.words, neg.words)
result1 = score.sentiment(delta.text1, pos.words, neg.words)
result2 = score.sentiment(delta.text2, pos.words, neg.words)


class(result)
result$score

result[1,'score']

delta.scores = score.sentiment(delta.text, pos.words,neg.words, .progress='text')
delta.scores1 = score.sentiment(delta.text1, pos.words, neg.words, .progress='text')
delta.scores2 = score.sentiment(delta.text2, pos.words, neg.words, .progress='text')

delta.scores$airline = 'Delta'
delta.scores$code = "DL"

delta.scores1$airline = 'American' 
delta.scores1$code = 'AA' 

delta.scores2$airline = "Continental"
delta.scores2$code = "CO"


## Plot Delta's score distribution
hist(delta.scores$score)

## Plot ggplot2
qplot(delta.scores$score)

## combine all the results into a single "all.scores" data.frame:
## rbind() combines rows from data.frames, arrays, and matrices
## all.scores = rbind( american.scores, continental.scores, delta.scores,
##                    jetblue.scores, southwest.scores, united.scores, us.scores )

all.scores = rbind( delta.scores,delta.scores1, delta.scores2 )


## Compare score distributions
ggplot(data=all.scores) + # ggplot works on data.frames, always
  geom_histogram(mapping=aes(x=score, fill=airline), binwidth=1) +
  facet_grid(airline~.) + # make a separate plot for each airline
  theme_bw() + scale_fill_brewer() # plain display, nicer colors

##  Ignore the middle
## Let's focus on very negative (<-2) and positive (>2) tweets:

all.scores$very.pos = as.numeric( all.scores$score >= 2 )
all.scores$very.neg = as.numeric( all.scores$score <= -2 )

## For each airline ( airline + code ), let's use the ratio of very positive to
## very negative tweets as the overall sentiment score for each airline:

twitter.df = ddply(all.scores, c('airline', 'code'), summarise,
                   pos.count = sum( very.pos ), neg.count = sum( very.neg ) )
twitter.df$all.count = twitter.df$pos.count + twitter.df$neg.count
twitter.df$score = round( 100 * twitter.df$pos.count /twitter.df$all.count )

## Sort with orderBy() from the doBy package:
orderBy(~-score, twitter.df)


## ***************** 
## Compare Twitter sentiment with ACSI satisfaction score

library(XML)
acsi.url = 'http://www.theacsi.org/index.php? option=com_content&view=article&id=147&catid=&Itemid=212&i=Airlines'

acsi.df = readHTMLTable(acsi.url, header=T, which=1, stringsAsFactors=F)

# only keep column #1 (name) and #18 (2010 score)
acsi.df = acsi.df[,c(1,18)]
head(acsi.df,1)

colnames(acsi.df) = c('airline', 'score')

                 

# acsi.df$code = c(NA, NA, 'CO', NA, 'AA', 'DL',
#                 'US', 'NW', 'UA', NA, NA, NA, NA, NA)

acsi.df$score = as.numeric(acsi.df$score)

acsi.df$code = c(NA, NA, NA, NA, NA, 'AA', 'DL', NA, NA, NA, NA, NA, NA, 'CO')


## Join and compare

compare.df = merge(twitter.df, acsi.df, by='code',
                   suffixes=c('.twitter', '.acsi'), all=T)



compare.df = subset(compare.df, all.count < 150)


## an actual result!
## ggplot will even run lm() linear (and other) regressions for you with its geom_smooth() layer:

ggplot( compare.df ) + geom_point(aes(x=score.twitter, y=score.acsi, color=airline.twitter), size=5) +
  geom_smooth(aes(x=score.twitter, y=score.acsi, group=1), se=F, method="lm") + theme_bw()
    + opts(legend.position=c(0.2,0.85))


