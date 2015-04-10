#trees/STEVENS
stevens = read.csv('./data/stevens.csv')
library(caTools)
set.seed(3000)
spl = sample.split(stevens$Reverse, SplitRatio=.7)
train = subset(stevens, spl==TRUE)
test = subset(stevens, spl==FALSE)
library(rpart)
library(rpart.plot)

stevensTree = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = train, method = 'class', minbucket = 5)

table(test$Reverse, predictCART)

predictCART = predict(stevensTree, newdata = test, type = "class")
library(ROCR)

predictROC = predict(stevensTree, newdata = test)
pred = prediction(predictROC[,2], test$Reverse)
perf = performance(pred, 'tpr', 'fpr')
plot(perf)



#random forests
library(randomForest)

test$Reverse = as.factor(test$Reverse)
train$Reverse = as.factor(train$Reverse)

set.seed(200)
stevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = train, nodesize = 25, ntree=200)
predictForest = predict(stevensForest, newdata=test)
table(test$Reverse, predictForest)


#cross validation
library(caret)
library(e1071)
numfolds = trainControl(method="cv", number=10)
cpGrid = expand.grid(.cp=seq(0.01,0.5,0.01))
train(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = train, method = "rpart", trControl=numfolds, tuneGrid= cpGrid)

stevensTreeCV = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = train, method = 'class', cp = 0.18)
predictCV = predict(stevensTreeCV, newdata = test, type="class")
table(test$Reverse, predictCV)


##trees/CLAIMS
claims = read.csv('./data/ClaimsData.csv')

#proportion of this types claims
table(claims$bucket2009) /nrow(claims)

#setup split on data
library(caTools)
set.seed(88)
split = sample.split(claims$bucket2009, SplitRatio=0.6)
ClaimsTrain = subset(claims, split==TRUE)
ClaimsTest = subset(claims, split==FALSE)

#baseline
table(ClaimsTest$bucket2009, ClaimsTest$bucket2008)
#baseline = sum of diagnol/nrows(ClaimsTest)
#penaltyerror 
PenaltyMatrix = matrix(c(0,1,2,3,4,2,0,1,2,3,4,2,0,1,2,6,4,2,0,1,8,6,4,2,0), byrow=TRUE, nrow=5)
as.matrix(table(ClaimsTest$bucket2009, ClaimsTest$bucket2008))*PenaltyMatrix
sum(as.matrix(table(ClaimsTest$bucket2009, ClaimsTest$bucket2008))*PenaltyMatrix)/nrow(ClaimsTest)

#quesion about all preds being type 1
Type1Vector = rep(1,183202)
table(ClaimsTest$bucket2009, Type1Vector)
122978/nrow(ClaimsTest)
newPenaltyMatrix = matrix(c(0,2,4,6,8), byrow=TRUE, nrow=5)
sum(as.matrix(table(ClaimsTest$bucket2009, Type1Vector))*newPenaltyMatrix)/nrow(ClaimsTest)

library(rpart)
library(rpart.plot)

claimsTree = rpart(bucket2009 ~ age + alzheimers + arthritis + cancer + copd +  depression + diabetes + heart.failure + ihd + kidney + osteoporosis + stroke + reimbursement2008 + bucket2008, data = ClaimsTrain, method = 'class', cp = 0.00005, parms=list(loss=PenaltyMatrix))
#prp(claimsTree)

PredictTest = predict(claimsTree, newdata=ClaimsTest, type="class")
table(ClaimsTest$bucket2009, PredictTest)
sum(as.matrix(table(ClaimsTest$bucket2009, PredictTest))*PenaltyMatrix)/nrow(ClaimsTest)
