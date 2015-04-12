wiki = read.csv('../data/wiki.csv')
#convert Vandal to factor
wiki$Vandal = as.factor(wiki$Vandal)

##1.2 bag of words
#creating a matrix
library(tm)
library(SnowballC)
#create corpus
corpusAdded = Corpus(VectorSource(wiki$Added))
#remove english stopwords
corpusAdded = tm_map(corpusAdded, removeWords, stopwords("English"))
#stem the words in the document
corpusAdded = tm_map(corpusAdded, stemDocument)
#build document term matrix
dtmAdded = DocumentTermMatrix(corpusAdded)

##1.3 create sparse matrix
sparseAdded = removeSparseTerms(dtmAdded, .997)

##1.4 convert to data.frame
wordsAdded = as.data.frame(as.matrix(sparseAdded))
#prepend words with an "A"
colnames(wordsAdded) = paste("A", colnames(wordsAdded))

##1.4 create a data.frame for removed words (copy lines above)
#create corpus
corpusRemoved = Corpus(VectorSource(wiki$Removed))
#remove english stopwords
corpusRemoved = tm_map(corpusRemoved, removeWords, stopwords("English"))
#stem the words in the document
corpusRemoved = tm_map(corpusRemoved, stemDocument)
#build document term matrix
dtmRemoved = DocumentTermMatrix(corpusRemoved)

##1.3 create sparse matrix
sparseRemoved = removeSparseTerms(dtmRemoved, .997)

##1.4 convert to data.frame
wordsRemoved = as.data.frame(as.matrix(sparseRemoved))
#prepend words with an "R"
colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))

##1.5 combine A and R data.frames
wikiWords = cbind(wordsAdded, wordsRemoved)
#add vandal column
wikiWords$Vandal = wiki$Vandal

library(caTools)
set.seed(123)
split = sample.split(wikiWords, SplitRatio = 0.7)
train = subset(wikiWords, split == TRUE)
test = subset(wikiWords, split == FALSE)

library(rpart)
library(rpart.plot)
wikiCART = rpart(Vandal ~ ., data = train, method = "class")
#prp(wikiCART)
#create prediction model
predictWiki = predict(wikiCART, newdata = test, type = "class")
table(test$Vandal, predictWiki)

##2.1 Problem Specific Knowledge
wikiWords2 = wikiWords
#create a new column that shows if a website url was added
wikiWords2$HTTP = ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)

wikiTrain2 = subset(wikiWords2, split==TRUE)
wikiTest2 = subset(wikiWords2, split==FALSE)
                   
wikiCART2 = rpart(Vandal ~ ., data = wikiTrain2, method = "class")
predictWikiCART2 = predict(wikiCART2, newdata = wikiTest2, type = "class")
table(wikiTest2$Vandal, predictWikiCART2)

##2.3  
wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))
##2.4 new CART model
train2 = subset(wikiWords2, split == TRUE)
test2 = subset(wikiWords2, split == FALSE)
wikiWords2CART = rpart(Vandal ~ ., data = train2, method = "class")
predictwikiWords2CART = predict(wikiWords2CART, newdata = test2, type = "class")
table(test2$Vandal, predictwikiWords2CART)

#3.1
wikiWords3 = wikiWords2
#add two pieces of meta data
wikiWords3$Minor = wiki$Minor
wikiWords3$Loggedin = wiki$Loggedin

train3 = subset(wikiWords3, split == TRUE)
test3 = subset(wikiWords3, split == FALSE)
wikiWords3CART = rpart(Vandal ~ ., data = train3, method = "class")
predictwikiWords3CART = predict(wikiWords3CART, newdata = test3, type = "class")
table(test3$Vandal, predictwikiWords3CART)


