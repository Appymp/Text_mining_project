rm(list = ls())
#packages
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(tidyverse)


#Import the data
lupus<-read.csv2("lupus.csv",sep=";",header=TRUE,row.names=1)
head(lupus)

#Explore the data
## Number of rows of data
nrow(lupus)

#distribution by year
lupus%>%ggplot(aes(Year))+ geom_bar() + 
  ggtitle('Proportion of Titles by Year')

#distribution by year class
lupus%>%ggplot(aes(Year_class))+ geom_bar() + 
  ggtitle('Proportion of Titles by Year_class')

#number of publications per author
lupus %>% group_by(First_Author) %>% summarise(nTitles = n_distinct(Title))%>%
  arrange(desc(nTitles))%>% ggplot(aes(nTitles)) + geom_density()


### Import and prepare data
corpus = VCorpus(VectorSource(lupus$Abstract))
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removeNumbers)
corpus = tm_map(corpus, removeWords, stopwords("en"))
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, stripWhitespace)
stemCorpus = tm_map(corpus, stemDocument,language="en")

### Create document term matrix
## - only words having from 4 to 20 characters
## - in at least 3 and at most 27 documents
#dtmr <-DocumentTermMatrix(stemdocs, control=list(wordLengths=c(4, 20), bounds = list(global = c(3,27))))
dtm<-DocumentTermMatrix(stemCorpus)
tm::inspect(dtm)

freqr <- colSums(as.matrix(dtm))
freqr
length(freqr) #number of unique words in corpus
sum(freqr) #total number of words in corpus

### Create frequency matrix
matdtm<-as.matrix(dtm)
termlist <- sort(colSums(matdtm),decreasing=TRUE)
termlistdf <- data.frame(word = names(termlist),freq=termlist)
head(termlist, 10)

#distribution of number of times each word is used
termlistdf%>%ggplot(aes(freq))+ geom_density() + 
  ggtitle('Distribution of words by freq in document corpus')

# Bar chart of terms (appearing more thant 150 times)
wf=data.frame(term=names(freqr),occurrences=freqr)
p <- ggplot(subset(wf, freqr>500), aes(term, occurrences))
p <- p + geom_bar(stat="identity")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p

# Terms used at least 'n' times in the corpus
findFreqTerms(dtm,lowfreq=500)


# Finding associations with a specific term: correlation higher than a given threshold
findAssocs(dtm,"patient",0.2)

set.seed(1234)

# words: the list of words to be represented in the wordcloud
# freq: frequencies of words
# min.freq: minimum frequency for a word to be plotted
# max.words: maximum number of words to be plotted (least frequent terms are dropped)
# random.order: plot words in random order; if FALSE, they are plotted in decreasing frequency
# rot.per: proportion of words vertically plotted
# colors: color words from least to most frequent


wordcloud(words = termlistdf$word, freq = termlistdf$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))






