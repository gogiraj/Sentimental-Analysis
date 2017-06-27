#R code - copy paste the entire mail text in r studio and change the working directory for the code to work
# Objective - To perform text mining in a set of blogs
install.packages('tm')
install.packages('SnowballC')
install.packages('wordcloud')

#1 Loading data into R
setwd('D:/MUIT/IVY Data Mining/textmining')
library(tm)
# create a collection of documents (technically referred to as a Corpus) 
doc_path <- setwd('D:/MUIT/IVY Data Mining/textmining')
docs <- Corpus(DirSource(doc_path))
docs
typeof(docs)
summary(docs)
writeLines(as.character(docs[[30]]))


#2 Pre-processing
# The tm package offers a number of transformations that ease the tedium of cleaning data. 
getTransformations() 
# To create a custom transformation. The tm package provides the ability to do this via the content_transformer function
# This function takes a function as input, the input function should specify what transformation needs to be done.
# There are colons and hyphens without spaces between the words separated by them. Using the removePunctuation transform  without fixing this will cause the two words on either side of the symbols  to be combined. 
toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern," ", x))})   # content_transformer takes as an input a function and applies it on each document in a corpus. The function is automatically provided the document as the first parameter.

docs <- tm_map(docs, toSpace, "-")
docs <- tm_map(docs, toSpace, ":")
writeLines(as.character(docs[[30]]))
# we can now apply the removePunctuation transformation
docs <- tm_map(docs, removePunctuation)
# several  "non-standard" punctuation marks have not been removed.
# copy-n-paste these symbols directly from the relevant text file to ensure that they are accurately represented in toSpace
docs <- tm_map(docs, toSpace, "'")
docs <- tm_map(docs, toSpace, "'")
writeLines(as.character(docs[[30]]))
docs <- tm_map(docs, toSpace, ".") # not done
docs <- tm_map(docs, toSpace, "\"") # not done
writeLines(as.character(docs[[30]]))
# Since R is case sensitive, we need to convert all of them to lower.
docs <- tm_map(docs,tolower)
writeLines(as.character(docs[[30]]))
docs <- tm_map(docs, removeNumbers)
writeLines(as.character(docs[[30]]))
# The next step is to remove common words  from the text. These  include words such as articles (a, an, the), conjunctions (and, or but etc.), common verbs (is), qualifiers (yet, however etc) . The tm package includes  a standard list of such stop words (https://en.wikipedia.org/wiki/Stop_words) as they are referred to. We remove stop words using the standard removeWords transformation
library(SnowballC)
word_list_to_be_removed <- stopwords("english")
word_list_to_be_removed <- c(word_list_to_be_removed,"jaydeep")
idx <- which(word_list_to_be_removed=="the")
word_list_to_be_removed<- word_list_to_be_removed[-idx]
docs <- tm_map(docs, removeWords, word_list_to_be_removed)
?stopwords

# there are some characters that has still not been removed
docs <- tm_map(docs, PlainTextDocument)
temp_text <- as.character(docs[[30]])
problem_char <- substr(temp_text[30],1,1)
docs <- tm_map(docs, toSpace, problem_char)
writeLines(as.character(docs[[30]]))

temp_text <- as.character(docs[[30]])
pos <- regexpr('empty', temp_text[39])
problem_char <- substr(temp_text[39],pos-1,pos-1)
docs <- tm_map(docs, toSpace, problem_char)
writeLines(as.character(docs[[30]]))

temp_text <- as.character(docs[[30]])
pos <- regexpr('time', temp_text[39])
problem_char <- substr(temp_text[39],pos+4,pos+4)
docs <- tm_map(docs, toSpace, problem_char)
writeLines(as.character(docs[[30]]))
# we remove all extraneous whitespaces using the stripWhitespace transformation
docs <- tm_map(docs, stripWhitespace)
writeLines(as.character(docs[[30]]))


#3 Stemming
# Stemming is the process of reducing such related words to their common root
# Simple stemming algorithms (such as the one in tm) are relatively crude: they work by chopping off the ends of words. This can cause problems: for example, the words mate and mating might be reduced to mat instead of mate.  That said, the overall benefit gained from stemming more than makes up for the downside of such special cases.
# There is a more sophisticated procedure called lemmatization that takes grammatical context into account. Among other things, determining the lemma of a word requires a knowledge of its part of speech (POS) - i.e. whether it is a noun, adjective etc. There are POS taggers that automate the process of tagging terms with their parts of speech. Although POS taggers are available for R (openNLP, for example),
docs <- tm_map(docs,stemDocument)
writeLines(as.character(docs[[30]]))



#4 Document Term Matrix
# a matrix that lists all occurrences of words in the corpus, by document
# This is usually huge & sparse
dtm <- DocumentTermMatrix(docs)
#docs <- tm_map(docs, PlainTextDocument) #IN CASE OF ERROR IN ABOVE STATEMENT
typeof(dtm)
inspect(dtm[1:2,1000:1005])


#5 Mining the corpus
# Frequency of occurrence of each word in the corpus
freq <- colSums(as.matrix(dtm))
ord <- order(freq,decreasing=TRUE)
freq[ord[1:10]]

# lot of most freq words might have little or no discriminating power in determining relevance
# The least frequent terms can be more interesting than one might think. 
# terms that occur rarely are likely to be more descriptive of specific documents.
freq[tail(ord)]

# question is how to strike a balance between frequency and specificity
# We can enforce rules to remove the words that occur too few or too much, similary very small words or very large words are removed (as they might be wrong instance of multiple words getting concatenated)
dtmr <-DocumentTermMatrix(docs, control=list(wordLengths=c(4, 20),bounds = list(global = c(3,Inf))))
dtmr
freqr <- colSums(as.matrix(dtmr))
length(freqr)
ordr <- order(freqr,decreasing=TRUE)
freqr[head(ordr)]
freqr[tail(ordr)]
# Finding terms that have high freq
findFreqTerms(dtmr,lowfreq=80) #results are alphabetical not in the order of freq

#check for correlations between some of these and other terms that occur in the corpus.  In this context, correlation is a quantitative measure of the co-occurrence of words in multiple documents.
findAssocs(dtmr,"project",0.6) # generates the list of terms that have atleast 60% occurance with project
findAssocs(dtmr,"problem",0.6)
findAssocs(dtmr,"system",0.6)


#6 Visualizations
wf <- data.frame(term=names(freqr),occurrences=freqr)

library(ggplot2)

p <- ggplot(subset(wf, freqr>100), aes(x = term, y = occurrences, fill="red"))
p <- p + geom_bar(stat="identity")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p

?ggplot

#wordcloud
library(wordcloud)
library(RColorBrewer)
#setting the same seed each time ensures consistent look across clouds
set.seed(42)
#limit words by specifying min frequency
wordcloud(names(freqr),freqr, min.freq=70)
#.add color
wordcloud(names(freqr),freqr,min.freq=70,colors=brewer.pal(6,"Dark2")) 
