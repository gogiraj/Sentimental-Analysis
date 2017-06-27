# 1
# retrieved the titles of the XKCD web comics 
 install.packages("RXKCD")
 install.packages("tm")
 install.packages("wordcloud")
library(RXKCD)
library(tm)
library(wordcloud)
library(RColorBrewer)
 
# System File xkcd.csv from package RXKCD
path <- system.file("xkcd", package = "RXKCD")
datafiles <- list.files(path)
xkcd.df <- read.csv(file.path(path, datafiles))
head(xkcd.df)

xkcd.corpus <- Corpus(DataframeSource(data.frame(xkcd.df[, 3])))

# Code for cleaning the data
xkcd.corpus <- tm_map(xkcd.corpus, removePunctuation)
xkcd.corpus <- tm_map(xkcd.corpus, tolower)
xkcd.corpus <- tm_map(xkcd.corpus, function(x) removeWords(x, stopwords("english")))
xkcd.corpus <- tm_map(xkcd.corpus, PlainTextDocument)

## xkcd.corpus <- tm_map(lords, stripWhitespace)
## xkcd.corpus <- tm_map(lords, removeWords, stopwords("english"))
## You can also Remove words manually
## xkcd.corpus <- tm_map(lords, removeWords, c("noble","whatever"))

####

# A term-document matrix is a mathematical matrix that describes the frequency of terms 
# that occur in a collection of documents. 

tdm <- TermDocumentMatrix(xkcd.corpus)
tdm
m <- as.matrix(tdm)

# Word Counts
v <- sort(rowSums(m),decreasing=TRUE)

d <- data.frame(word = names(v),freq=v)
head(d)

# Building the wordcloud
pal <- brewer.pal(9, "BuGn")
pal <- pal[-(1:2)]
png("C:/Users/Gagan/Desktop/MUIT/Data Mining/textminingcodespptoutputplots/wordcloud.png", width=1280,height=800)
wordcloud(d$word,d$freq, scale=c(8,.3),min.freq=2,max.words=100, random.order=T, rot.per=.15, colors=pal, vfont=c("sans serif","plain"))
dev.off()

# 2
install.packages("XML")
require(XML)
require(tm)
require(wordcloud)
require(RColorBrewer)

library(XML)
library(tm)
library(wordcloud)
library(RColorBrewer)

u = "http://cran.r-project.org/web/packages/available_packages_by_date.html"
t = readHTMLTable(u)[[1]]
edit(t)

ap.corpus <- Corpus(DataframeSource(data.frame(as.character(t[,3]))))
ap.corpus <- tm_map(ap.corpus, removePunctuation)
ap.corpus <- tm_map(ap.corpus, tolower)
ap.corpus <- tm_map(ap.corpus, function(x) removeWords(x, stopwords("english")))
ap.corpus <- tm_map(ap.corpus, PlainTextDocument)


ap.tdm <- TermDocumentMatrix(ap.corpus,
                             control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE),
                                            stopwords = TRUE))
ap.tdm
ap.m <- as.matrix(ap.tdm)
ap.v <- sort(rowSums(ap.m),decreasing=TRUE)
ap.d <- data.frame(word = names(ap.v),freq=ap.v)
#table(ap.d$freq)
pal2 <- brewer.pal(8,"Dark2")
png("C:/Users/Gagan/Desktop/MUIT/Data Mining/textminingcodespptoutputplots/wordcloud_packages.png", width=3280,height=2800)
wordcloud(ap.d$word,ap.d$freq, scale=c(8,.2),min.freq=3,
          max.words=Inf, random.order=FALSE, rot.per=.15, colors=pal2)
dev.off()

# TRY STEMMING/LEMMATIZING  
# TRY ON A DIFFERENT DATASET