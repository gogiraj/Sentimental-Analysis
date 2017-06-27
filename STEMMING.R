
install.packages("devtools")
install.packages("rjson")
install.packages("bit64")
install.packages("httr")
install.packages("plyr")
install.packages("twitteR")
install.packages("SnowballC")

require("devtools")
require("rjson")
require("bit64")
require("httr")
require("plyr")
require("twitteR")
require("SnowballC")


# sessionInfo()
api_key <- "BfvZ8xs30MkNQFAWLk0zf4WIz"
api_secret <- "oLDZqzXuJjVDz5Ps5ybfzQUNigxtP8NDOy3mpmBNZE3tmHs7GS"
access_token <- "47717736-IF0PhKjohLlJEd6xK2yWY5H2FQH3vRvd7neT9dGjz"  
access_token_secret <- "pJLM1zeiAzlVWVCGaLSMltDAfvEzdsYpRntiUoNW4ENEg"

#INSTALL RTOOLS
# find_rtools()
install.packages("rtools")
# devtools::install_github("jrowen/twitteR", ref = "oauth_httr_1_0",version="0.6.1")
library(devtools)
library(plyr)
library(twitteR)

install.packages("httr")
library(devtools) #if not installed, do that obviously

#A restart of R might be necessary if you previously had httr installed.
library(httr)
library(SnowballC)
# setup_twitter_oauth(consumerKey, consumerSecret, accessKey, accessSecret)

setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)



tweets=searchTwitter('#DeMonetisation',n=100)
df <- do.call("rbind", lapply(tweets, as.data.frame))
# install.packages("C:/Users/Admin/Downloads/twitteR_1.1.8.tar.gz",repos=NULL, type="source",dependencies = TRUE)
# install.packages("C:/Users/Admin/Downloads/plyr_1.8.2.tar.gz",repos=NULL, type="source",dependencies = TRUE)
# install.packages("C:/Users/Admin/Downloads/httr_0.6.1.tar.gz",repos=NULL, type="source",dependencies = TRUE)
install.packages('tm')
library(tm)
text <- df$text

review_source <- VectorSource(text)
corpus <- Corpus(review_source)
#corpus <- tm_map(corpus,
#                   content_transformer(function(x) iconv(x, to='UTF-8-MAC', sub='byte')),
#                   mc.cores=1)
corpus <- tm_map(corpus,
                 content_transformer(function(x) iconv(x, to='UTF-8', sub='byte')),
                 mc.cores=1)

corpus <- tm_map(corpus,removePunctuation)
corpus <- tm_map(corpus,content_transformer(tolower))
corpus <- tm_map(corpus,stripWhitespace)
corpus <- tm_map(corpus,removeWords,stopwords("english"))
# Stemming 
corpus <- tm_map(corpus,stemDocument)


t.tdm <- TermDocumentMatrix(corpus)
t.m <- as.matrix(t.tdm)
t.v <- sort(rowSums(t.m),decreasing=TRUE)
t.d <- data.frame(word = names(t.v),freq=t.v)

write.csv(t.d,"C:/Users/Gagan/Desktop/MUIT/Data Mining/textminingcodespptoutputplots/testmiFirst04.csv")

head(t.d)
#install.packages("wordcloud")
library("wordcloud")
pal2 <- brewer.pal(8,"Dark2")
png("C:/Users/Gagan/Desktop/MUIT/Data Mining/textminingcodespptoutputplots/wordcloud_demo_Stem01.png", width=3280,height=2800)
wordcloud(t.d$word,t.d$freq, scale=c(8,.2),min.freq=3,
          max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal2)
dev.off()



# lemmatization and tagging in R ** Its a rule based
# try package koRpus which allow to use Treetagger :
install.packages("koRpus")
library(koRpus)


tagged.results <- treetag(c("demonetis","demonetisationãâ","demonetisationbhakt","demonetisationtragedi"), treetagger="manual", 
                          format="obj",TT.tknz=FALSE , lang="en", TT.options=list(path="C:/Program Files/TreeTagger", preset="en"))
tagged.results@TT.res

##     token tag lemma lttr wclass                               desc stop stem
## 1     run  NN   run    3   noun             Noun, singular or mass   NA   NA
## 2     ran VVD   run    3   verb                   Verb, past tense   NA   NA
## 3 running VVG   run    7   verb Verb, gerund or present participle   NA   NA
# See the lemma column for the result you're asking for.