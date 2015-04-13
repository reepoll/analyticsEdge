emails = read.csv('../data/emails.csv', stringsAsFactors=FALSE)

##1.5
max(nchar(emails$text))
match(min(nchar(emails$text)), nchar(emails$text))

##2.1
library(tm)
library(SnowballC)
corpus = Corpus(VectorSource(emails$text))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("English"))
corpus = tm_map(corpus, stemDocument)
dtm = DocumentTermMatrix(corpus)
spdtm = removeSparseTerms(dtm, .95)
emailsSparse = as.data.frame(as.matrix(spdtm))
##2.3
sort(colSums(emailsSparse))
##2.4
emailsSparse$spam = emails$spam
hamSparse = subset(emailsSparse, spam=0)
sort(colSums(hamSparse))
