library(here)
library(tidyverse)
library(tm)
library(readxl)
library(data.table)
library(SnowballC)
library(fuzzyjoin)
library(wordcloud)
library(RColorBrewer)
library(Xplortext)
library(factoextra)

###Import dataset
lupus <- fread("lupus.csv")

####Description of the data####

## Number of rows of data
nrow(lupus)

## unique titles
n_distinct(lupus$Title)

###Nb of authors
unique(lupus$First_Author)
#409

###Authors is more than 2 publications
lupus%>%
  group_by(First_Author)%>%
  count()%>%
  filter(n > 2)

## Bar chart of count of authors by number of publications(titles)
lupus%>%
  group_by(First_Author)%>% summarise(ntitles = n_distinct(Title))%>%
  ggplot(aes(as.factor(ntitles))) + geom_bar() + 
  ggtitle('Count of Authors by number of Titles')+xlab('Number of Titles') + ylab('Count of Authors')

## Top 10 authors by titles published
lupus%>%
  group_by(First_Author)%>% summarise(ntitles = n_distinct(Title))%>%arrange(desc(ntitles))%>%head(10)

###Nb of journals
unique(lupus$Journal)
#112

## Bar chart of count of titles by journal
lupus%>%
  group_by(Journal)%>% summarise(ntitles = n_distinct(Title))%>%
  ggplot(aes(as.factor(ntitles))) + geom_bar() + 
  ggtitle('Count of Journals by number of Titles')+xlab('Number of Titles') + ylab('Count of Journals')

## Top 10 journals by titles published
lupus%>%
  group_by(Journal)%>% summarise(ntitles = n_distinct(Title))%>%arrange(desc(ntitles))%>%head(10)
  

## Bar chart of count of authors by journals
lupus%>%
  group_by(Journal)%>% summarise(nauthors = n_distinct(First_Author))%>%
  ggplot(aes(as.factor(nauthors))) + geom_bar() + 
  ggtitle('Count of Journals by number of Authors')+xlab('Number of authors') + ylab('Count of Journals')

#Top 10 journals by authors 
lupus%>%
  group_by(Journal)%>% summarise(nauthors = n_distinct(First_Author))%>%arrange(desc(nauthors))%>%head(10)

####Publication starting 1994 to 2012 (18 years)
##distribution by year
lupus%>%ggplot(aes(as.factor(Year)))+ geom_bar() + 
  ggtitle('Number of Titles by Year')+xlab('Year') + ylab('Titles published')

## Top 10 journals by volume of publishing per year
lupus%>%
  group_by(Year,Journal)%>% summarise(ntitles = n_distinct(Title))%>%arrange(desc(ntitles))%>%head(10)%>%
  ggplot(aes(x=reorder(Journal,-ntitles),ntitles,fill=as.factor(Year)))+
  geom_bar(stat = 'identity',position='dodge', colour = 'black',alpha=1)+
  theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=0.5))+
  ggtitle('Top 10 journals by volume of publishing') + xlab('Journal') + ylab('Number of titles')+
  guides(fill=guide_legend(title="Year"))
  

##distribution by year class
lupus%>%ggplot(aes(Year_class))+ geom_bar() + 
  ggtitle('Number of Titles by 2 year groups')+xlab('Year range') + ylab('Titles published')



####Stat descriptive####

### Create a corpus with "content" column values
corpus = VCorpus(VectorSource(lupus$Abstract))

### Turn text into lowercase (if any uppercase)
corpus = tm_map(corpus, content_transformer(tolower))

### Remove numbers (if any)
corpus = tm_map(corpus, removeNumbers)

### Remove English stopwords (articles, prepositions...)
corpus = tm_map(corpus, removeWords, stopwords("en"))

### Remove punctuation
corpus = tm_map(corpus, removePunctuation)


### Removes extra whitespaces
corpus = tm_map(corpus, stripWhitespace)

### Stemming (needs SnowballC package)

stemCorpus = tm_map(corpus, stemDocument,language="en")

### Create document term matrix
#dtm<-DocumentTermMatrix(corpus)
stemDtm<-DocumentTermMatrix(stemCorpus)


### Create frequency matrix

stemFreq<-colSums(as.matrix(stemDtm))

# Ordering terms (descending)
ord <- order(stemFreq,decreasing=TRUE)

# Most frequently occurring terms
stemFreq[head(ord,10)]


### Display frequencies (with stemming)
stemResults<-data.frame(names(stemFreq),count=stemFreq,row.names=NULL)


stemResults%>%
  rename(terms = names.stemFreq.)%>%
  mutate(terms = fct_reorder(terms, count))%>%
  filter(count > 500)%>%
  ggplot(aes(x = count, y = terms)) +
  geom_bar(stat="identity")+
  theme_bw()


### Script for wordcloud

### Create frequency matrix
matdtm<-as.matrix(stemDtm)
termlist <- sort(colSums(matdtm),decreasing=TRUE)
termlistdf <- data.frame(word = names(termlist),freq=termlist)
head(termlist, 10)


### Generate wordcloud
# Fixing the seed makes it possible to obtain the same cloud each time
#the code is executed
set.seed(1234)

wordcloud(words = termlistdf$word, freq = termlistdf$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

##SLE = Systemic Lupus Erythematosus


#### CA#####


### Prepare textual data non aggregated #### 
lup.TD<-TextData(lupus, var.text=6, idiom="en", Fmin=100, Dmin=80,
                 stop.word.tm=TRUE,graph=FALSE)

summary(lup.TD,ndoc=0)

##Plot

plot(lup.TD,nword=15,sel="word",
     title="First 15 most frequent words",xtitle="frequency")

### Prepare textual data aggregated data by Year #### 
lup_agg.TD<-TextData(lupus, var.text=6, idiom="en", Fmin=100, Dmin=80,
                 var.agg = "Year",
                 stop.word.tm=TRUE,graph=FALSE)

summary(lup_agg.TD,ndoc=0)

##Plot

plot(lup_agg.TD,nword=15,sel="word",
     title="First 15 most frequent words",xtitle="frequency")



# Frequency table
TableLex<-as.matrix(lup.TD$DocTerm)
#TableLex<-as.matrix(lup_agg.TD$DocTerm)

dm1 <- addmargins(TableLex)
rownames(dm1)[nrow(dm1)]<-"Total"
colnames(dm1)[ncol(dm1)]<-"Total"
print(dm1, zero.print = ".")

# Line profiles 
dm2<-rbind(TableLex,apply(TableLex,2,sum))
dm2<-prop.table(as.matrix(dm2),margin=1) 
dm2<-cbind(dm2,apply(dm2,1,sum))
rownames(dm2)[nrow(dm2)]<-"Mean profile"
colnames(dm2)[ncol(dm2)]<-"Total"
print(round(dm2*100,2))


# Column profiles 
dm3<-cbind(TableLex,apply(TableLex,1,sum))
dm3<-prop.table(as.matrix(dm3),margin=2)
dm3<-rbind(dm3,apply(dm3,2,sum))
rownames(dm3)[nrow(dm3)]<-"Total"
colnames(dm3)[ncol(dm3)]<-"Mean profile"
round(dm3*100,2)

# (Pearson) Chi-square Test
res.chi2<-chisq.test(TableLex)
print(res.chi2)

# Observed frequencies
tabobser<-res.chi2$observed
tabobser <- addmargins(tabobser)
rownames(tabobser)[nrow(tabobser)]<-"Total"
colnames(tabobser)[ncol(tabobser)]<-"Total"
print(tabobser)

# Expected frequencies (under the hypothesis of independance)
tabexpect<-res.chi2$expected
tabexpect <- addmargins(tabexpect)
rownames(tabexpect)[nrow(tabexpect)]<-"Total"
colnames(tabexpect)[ncol(tabexpect)]<-"Total"
round(tabexpect,0)

# Measure of association
tau<-(res.chi2$observed)/res.chi2$expected
round(tau,2)

# Correspondance analysis of lexical data 
res.LexCA<-LexCA(lup.TD, graph=FALSE)

# Total inertia
res.LexCA[["Inertia"]]

# Eigenvalues
res.LexCA$eig

# Histogram of eigen values
plot(res.LexCA,eigen=TRUE,selDoc=NULL,selWord=NULL,title="Eigen values")


# Results for documents (1 document = 1 Sex X Age category)
summary(res.LexCA,nword=0)

# Results for terms
summary(res.LexCA,ndoc=0)

# Plotting documents (categories) on plan 1-2
plot(res.LexCA,selWord=NULL,xlim=c(-0.3,0.3),ylim=c(-0.3,0.3),cex=1.2,col.doc="black",title="Documents (categories) on plan 1-2")
#lines(res.LexCA$row$coord[1:3,1],res.LexCA$row$coord[1:3,2],lwd=1.5,col="grey20")
#lines(res.LexCA$row$coord[4:6,1],res.LexCA$row$coord[4:6,2],lwd=1.5,col="grey20")

# Plotting terms on plan 1-2
plot(res.LexCA,gtype="DocWord",selDoc=NULL,xlim=c(-0.3,0.3),ylim=c(-0.3,0.3),col.word="black",cex=1.2,
     title="Terms on plan 1-2")

# Graph overlay: documents and terms on the same graph
plot(res.LexCA,gtype="DocWord",xlim=c(-0.3,0.3),ylim=c(-0.3,0.3),col.doc="grey50",col.word="black",cex=1.2,
     title="Joint representation of terms and documents on plan 1-2")
#lines(res.LexCA$row$coord[1:3,1],res.LexCA$row$coord[1:3,2],lwd=1,col="black")
#lines(res.LexCA$row$coord[4:6,1],res.LexCA$row$coord[4:6,2],lwd=1,col="black")


# Confidence ellipses for documents (1 document = 1 Sex X Age category)
ellipseLexCA(res.LexCA,selWord=NULL,col.doc="black",
             title="Confidence ellipses for documents")
#lines(res.LexCA$row$coord[1:3,1],res.LexCA$row$coord[1:3,2],lwd=1,col="blue")
#lines(res.LexCA$row$coord[4:6,1],res.LexCA$row$coord[4:6,2],lwd=1,col="blue")

# Confidence ellipses for terms with highest contributions on axis 1 or 2
ellipseLexCA(res.LexCA,selWord="meta 0.5",selDoc=NULL,col.word="black",
             title="Confidence ellipses for some terms")




##R shiny
###Studying documents and words with CA and MCA
# + use of explor package

library(Xplortext)
library(explor)


# Create data table non agg data
lup.TD<-TextData(lupus, var.text=6, idiom="en", Fmin=100, Dmin=80,
                 stop.word.tm=TRUE,graph=FALSE)


DocTerm1 <- cbind(as.matrix(lup.TD$DocTerm))
res.CA <- CA(DocTerm1, graph = TRUE)

plot(res.CA, invisible = "row")
plot(res.CA)

fviz_ca_col(res.CA, repel = TRUE)


#fviz_ellipses(res.CA)

##Articles which characterise the dimension
# Contributions of titles to dimension 1
fviz_contrib(res.CA, choice = "row", axes = 1, top = 10)
# Contributions of titles to dimension 2
fviz_contrib(res.CA, choice = "row", axes = 2, top = 5)

# Contributions of words to dimension 1
fviz_contrib(res.CA, choice = "col", axes = 1, top = 20)
## 18 words characterise the 1st dimension

# Contributions of words to dimension 2
fviz_contrib(res.CA, choice = "col", axes = 2, top = 9)
## 8 words contribute to the 2nd dimension

# Contributions of Articles to to dimension 1 and 2
fviz_contrib(res.CA, choice = "row", axes = 1:2, top = 15)
## After 7 articles, there is a sharp drop in contribution to the 2 axes

# Contributions of Words to to dimension 1 and 2
fviz_contrib(res.CA, choice = "col", axes = 1:2, top = 15)
##12 words contribute significantly to the 2 axes

# Finding associations with a specific term: correlation higher than a given threshold
#findAssocs(stemDtm,"anti",0.9)



# Plot the top  contributing words and Articles
# And the top 15 columns
fviz_ca_biplot(res.CA,  
               select.row = list(contrib = 15),
               select.col = list(contrib = 12), repel=T)






# Create data table agg data
lup_agg.TD<-TextData(lupus, var.text=6, idiom="en", Fmin=100, Dmin=80,
                     var.agg = "Year",
                     stop.word.tm=TRUE,graph=FALSE)


DocTerm1_agg <- cbind(as.matrix(lup_agg.TD$DocTerm))
res_agg.CA <- CA(DocTerm1_agg, graph = TRUE)

# Run the R-shiny visualisation package
explor(res.CA)




#####Adding the drug dataset####

drugs <- fread("Input/drugs.csv")


lup_drug <- lupus %>% 
  fuzzy_inner_join(drugs, by = c("Abstract" = "methotrexate"), match_fun = str_detect)


### Prepare textual data aggregated data by Year #### 
lup_agg_drug.TD<-TextData(lup_drug, var.text=8, idiom="en",
                     var.agg = "Year",
                     stop.word.tm=FALSE,graph=FALSE)

summary(lup_agg_drug.TD, ndoc = 0)

DocTerm1_agg_drug <- cbind(as.matrix(lup_agg_drug.TD$DocTerm))
res_agg_drug.CA <- CA(DocTerm1_agg_drug, graph = TRUE)





