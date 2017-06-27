#installing twitteR
install.packages("twitteR")
library(twitteR)

#installing ROAuth
install.packages("ROAuth")
library(ROAuth)

install.packages("RCurl")
library(RCurl)

#installing twitteR
install.packages("plyr")
library(plyr)

#installing stringr
install.packages("stringr")
library(stringr)

#Install ggplot2
install.packages("ggplot2")
#--This may not workout so try using another one
install.packages('ggplot2', dep = TRUE)
library(ggplot2)

#installing bitops
install.packages("bitops")
library(bitops)


#installing digest
install.packages("digest")
library(digest)

#installing rjson
install.packages("rjson")
library(rjson)

# Installing httr
install.packages("httr")
library(httr)

install.packages("base64enc")
library("base64enc")

install.packages("tm")
# install.packages("twitteR", "RCurl", "RJSONIO", "stringr")

install.packages("RJSONIO")

library(ROAuth)
library(twitteR)
library(RCurl)
library(plyr)
library(ggplot2)
library(bitops)
library(digest)
library(rjson)
library(httr)
library(base64enc)
library(RJSONIO)
library(stringr)

options(httr_oauth_cache=T)

##### Twitter Credentials to access twitter account #####
api_key <- "DWZrkTBvG8XU74rCth4jr2etg"

api_secret <- "JIdtf3CJblIsHTwq3oBfikeGONV0CldRq42jXC3bz2jH5P3VnE"

access_token <- "172317191-ZtOZMfrED5ICXQ4FkFM88ikPK87tijactKGvyDx2"

access_token_secret <- "ElEwGFJxZ3WnDJBFHQ9FyEXef2AaYfhkoQD1mOePWGqrj"

##### Setup the connection #####
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

##### Search for the tweet ##### 
tweet <- searchTwitter("#Demonetisation OR #Modi", n=500, since="2016-11-08")

head(tweet)
#tweet <- searchTwitter("#Demonitisation OR #Modi OR #Namo OR #Blackmony", n=5000, lang="en", geocode="51.278236,-0.95171109,100mi", since="2016-11-08")
class(tweet)

##### Transform tweets list into a data frame #####
tweet_df <- twListToDF(tweet)

##### Extract Tweet text from complete tweet details #####
tweets.text <- lapply(tweet, function(t)t$getText())
class(tweets.text)

head(tweets.text)

##### Write the tweet dataframe in local #####
write.csv(tweet_df, file='D:/MUIT/Data Mining/Demonitisation.csv', row.names=T)

## tweet_df <- read.csv("D:/MUIT/Data Mining/Demonitisation.csv")

##### Cleaning data using Package #####
##### Creating a vector source #####
library(tm)
mycorpus <- Corpus(VectorSource(tweets.text))
x <- as.character(mycorpus)
str(mycorpus)

##### Removing white spaces
mycorpus1 <- tm_map(mycorpus, stripWhitespace)

# Converting text to lower case
mycorpus2 <- tm_map(mycorpus1, tolower)

# Removing stopwords
mycorpus3 <- tm_map(mycorpus2, removeWords, stopwords("english"))

# Removing Punctuation 
mycorpus4 <- tm_map(mycorpus3, removePunctuation)

# Removing Numbers
mycorpus5 <- tm_map(mycorpus4, removeNumbers)

# Plain text documents
mycorpus6 <- tm_map(mycorpus5, PlainTextDocument)

# Stemming 
mycorpus7 <- tm_map(mycorpus6,stemDocument)

##### Convert the corpus to Term Matrix #####
data_dtm1 <- DocumentTermMatrix(mycorpus7)

##### Inspect Element #####
inspect(data_dtm1)

### Frequent Words coming in the text document #####
frequent<-findFreqTerms(data_dtm1,lowfreq = 100,highfreq = Inf)
frequent


#### Generating Word Cloud #######
##################################
install.packages("wordcloud",dependencies = TRUE)
install.packages("stringr",dependencies = TRUE)
library(stringr)
library(RColorBrewer)
library(wordcloud)
wordcloud::wordcloud(mycorpus7,max.words=100, random.order = FALSE)

pal <- brewer.pal(9,"YlGnBu")
pal <- pal[-(1:4)]
set.seed(123)

wordcloud::wordcloud(words = mycorpus7,scale=c(8,0.2),max.words=100,
                     random.order=FALSE,rot.per=0.35, use.r.layout=FALSE, colors=pal)

wordcloud::wordcloud(words = mycorpus7,max.words=100,
                     random.order=FALSE,rot.per=0.35, use.r.layout=FALSE, colors=pal)

dev.off()


### Counting the occurrence of the words in the document #######
install.packages("slam", dependencies = TRUE)
library(slam)
freq<-colapply_simple_triplet_matrix(data_dtm1,FUN = sum)
head(freq)

## World Cloud based on frequeny
pal2 <- brewer.pal(8,"Dark2")
png("D:/MUIT/Data Mining/wordcloud_om.png", width=3280,height=2800)
wordcloud(t.d$word,t.d$freq, scale=c(8,.2),min.freq=10,
          max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal2)
dev.off()


### Writing it out in csv #####
write.csv(frequent,file = "D:/MUIT/Data Mining/frequent_words_final.csv")
write.csv(freq,file = "D:/MUIT/Data Mining/total_words_count_final.csv")


######## Sentiment Analysis ######################
##################################################
install.packages("syuzhet")
library(syuzhet)
install.packages("lubridate")
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
install.packages("dplyr")
library(dplyr)
sentiment<-get_nrc_sentiment(x)
sentiment
t<-as.matrix(sentiment)
write.csv(sentiment,"sentiment_score.csv")

getwd()

### Visualizing the sentiment score ####
########################################

## Sentimental emotion expressions Analysis
sentimentTotals <- data.frame(colSums(sentiment[,c(1:8)]))
names(sentimentTotals) <- "count"
sentimentTotals <- cbind("sentiment" = rownames(sentimentTotals), sentimentTotals)
rownames(sentimentTotals) <- NULL

ggplot(data = sentimentTotals, aes(x = sentiment, y = count)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Total Count") + ggtitle("Total Sentiment Score for all Tweets")


## Sentimental Positive Negative Analysis

sentimentNegPos <- data.frame(colSums(sentiment[,c(9:10)]))
names(sentimentNegPos) <- "count"
sentimentNegPos <- cbind("sentiment" = rownames(sentimentNegPos), sentimentNegPos)
rownames(sentimentNegPos) <- NULL

ggplot(data = sentimentNegPos, aes(x = sentiment, y = count)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Total Count") + ggtitle("Total Sentiment Score for all Tweets")



