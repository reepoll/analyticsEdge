tweets = read.csv('./data/tweets.csv', stringsAsFactors=FALSE)
tweets$Negative = as.factor(tweets$Avg <= -1)

library(tm)
library(SnowballC)
#create corpus
corpus = Corpus(VectorSource(tweets$Tweet))

#begin stemming
#convert to lowercase
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
#remove punctionuation
corpus = tm_map(corpus, removePunctuation)
#remove stop words
corpus = tm_map(corpus, removeWords, c("apple", stopwords("English")))
corpus[[1]]
#stemdocument
corpus = tm_map(corpus, stemDocument)

#create a matrix of words
freq = DocumentTermMatrix(corpus)
#inspect matrix [document obj, terms obj]
inspect(freq[1000:1005,505:515])
#find most popular terms (20 or above appearances)
findFreqTerms(freq, lowfreq = 20)
##removing terms that don't appear often to ease comptuational complexities
#remove sparse terms (corpus, keep terms that appear in this 1 - .xx of tweets)
sparse = removeSparseTerms(freq, 0.995)

#convert sparse to data frame
tweetsSparse = as.data.frame(as.matrix(sparse))
#make sure colnames are named appropriately
colnames(tweetsSparse) = make.names(colnames(tweetsSparse))
#add the dependent var (tweets$Negative) to the data.frame
tweetsSparse$Negative = tweets$Negative

#split data into test/train
library(caTools)
set.seed(123)
split = sample.split(tweetsSparse$Negative, SplitRatio = 0.7)
train = subset(tweetsSparse, split = TRUE)
test = subset(tweetsSparse, split == FALSE)

findFreqTerms(freq, lowfreq = 100)

library(rpart)
library(rpart.plot)
tweetCART = rpart(Negative ~ ., data = train, method = "class")
prp(tweetCART)
predictCART = predict(tweetCART, newdata = test, type = "class")
table(test$Negative, predictCART)

library(randomForest)
set.seed(123)
tweetRF = randomForest(Negative ~ ., data = train, method = "class")
predictRF = predict(tweetRF, newdata = test)
table(test$Negative, predictRF)

#build regression model
tweetLog = glm(Negative ~ ., data=train, family="binomial")  
predictLog = predict(tweetLog, newdata=test, type="response")
table(test$Negative, predictLog > 0.5)
