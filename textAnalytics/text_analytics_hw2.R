trials = read.csv('../data/clinical_trial.csv', stringsAsFactors=FALSE)
#1.1
max(nchar(trials$abstract))
#1.2
nrow(subset(trials, nchar(abstract)==0))
#1.3
trials$title[match(min(nchar(trials$title)),nchar(trials$title))]

##2.1
library(tm)
library(SnowballC)
corpusTitle = Corpus(VectorSource(trials$title))
corpusAbstract = Corpus(VectorSource(trials$abstract))
corpusTitle = tm_map(corpusTitle, tolower)
corpusAbstract = tm_map(corpusAbstract, tolower)
corpusTitle = tm_map(corpusTitle, PlainTextDocument)
corpusAbstract = tm_map(corpusAbstract, PlainTextDocument)
corpusTitle = tm_map(corpusTitle, removePunctuation)
corpusAbstract = tm_map(corpusAbstract, removePunctuation)
corpusTitle = tm_map(corpusTitle, removeWords, stopwords("English"))
corpusAbstract = tm_map(corpusAbstract, removeWords, stopwords("English"))
corpusTitle = tm_map(corpusTitle, stemDocument)
corpusAbstract = tm_map(corpusAbstract, stemDocument)
dtmTitle = DocumentTermMatrix(corpusTitle)
dtmAbstract = DocumentTermMatrix(corpusAbstract)
#create sparse matrices
sparseTitle = removeSparseTerms(dtmTitle, .95)
sparseAbstract = removeSparseTerms(dtmAbstract, .95)
dtmTitle = as.data.frame(as.matrix(sparseTitle))
dtmAbstract = as.data.frame(as.matrix(sparseAbstract))

#2.3 most common word in abstract
sort(colSums(dtmAbstract))

##3.1
colnames(dtmTitle) = paste0("T", colnames(dtmTitle))
colnames(dtmAbstract) = paste0("A", colnames(dtmAbstract))

##3.2
dtm = cbind(dtmTitle, dtmAbstract)
dtm$trial = trials$trial

##3.3
library(caTools)
set.seed(144)
split = sample.split(dtm, SplitRatio = 0.7)
train = subset(dtm, split == TRUE)
test = subset(dtm, split == FALSE)

##3.4
library(rpart)
library(rpart.plot)
trialCART = rpart(trial ~ ., data = train, method = "class")

predTrain = predict(trialCART)[,2]
summary(predTrain)

pred = predict(trialCART, newdata = test, type = "class")
table(test$trial, pred)
