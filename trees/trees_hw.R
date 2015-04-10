gerber = read.csv('./data/gerber.csv')

#2. Which of the four "treatment groups" had the largest percentage of people who actually voted (voting = 1)?
tapply(gerber$voting, gerber$civicduty, mean)
#or
nrow(subset(gerber, voting==1 & neighbors==1)) / nrow(subset(gerber, neighbors==1))

#3. Build a logistic regression model for voting using the four treatment group variables as the independent variables
LogModel = glm(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, family="binomial")

#4. Using a threshold of 0.3, what is the accuracy of the logistic regression model? (When making predictions,
predictLog = predict(LogModel, type="response")
table(gerber$voting, predictLog > 0.3)
table(gerber$voting, predictLog > 0.5)

#5. Build a CART tree for voting using all data and the same four treatment variables we used before.
library(rpart)
library(rpart.plot)
CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber)

#6. to force the complete tree to be built. 
CARTmodel2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)
prp(CARTmodel2)

#6. Make a new tree that includes the "sex" variable, again with cp = 0.0.
CARTmodel3 = rpart(voting ~ civicduty + hawthorne + self + neighbors + sex, data=gerber, cp=0.0)
prp(CARTmodel3)

#7. 
CARTmodel4 = rpart(voting ~ control, data=gerber, cp=0.0)
CARTmodel5 = rpart(voting ~ control + sex, data=gerber, cp=0.0)
predictmodel4 = predict(CARTmodel4)
prp(CARTmodel4, digits=6)

#8
prp(CARTmodel5, digits=6)

#3.3. 
LogModelSex = glm(voting ~ sex + control, data=gerber, family="binomial")

#3.4
Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(LogModelSex, newdata=Possibilities, type="response")

#3.5
LogModel2 = glm(voting ~ sex + control + sex:control, data=gerber, family="binomial")

#3.6
predict(LogModel2, newdata=Possibilities, type="response")

##Letter Recognition
#1.1.
letters = read.csv('./data/letters_ABPR.csv')
letters$isB = as.factor(letters$letter == "B")
set.seed(50)
library(caTools)
split = sample.split(letters$isB, SplitRatio = .5)

train = subset(letters, split == TRUE)
test = subset(letters, split == FALSE)

#1.2 build tree model
library(rpart)
CARTb = rpart(isB ~ . - letter, data=train, method="class")
pred = predict(CARTb, newdata=test, type="class")

#1.3
library(randomForest)
set.seed(100)
lettersForest = randomForest(isB ~ . - letter, data=train, method="class")
predForest = predict(lettersForest, newdata = test)
table(test$isB, predForest)
(1163+363)/nrow(test)

#2.1
letters$letter = as.factor( letters$letter )
set.seed(2000)
split = sample.split(letters$letter, SplitRatio = .5)
test = subset(letters, split==TRUE)
train = subset(letters, split==FALSE)
CARTmodel2 = rpart(letter ~ . -isB, data=train, method="class")
pred = predict(CARTmodel2, newdata=test, type="class")
table(test$letter, pred)

#2.3
library(randomForest)
set.seed(1000)
letterForest = randomForest(letter ~ . -isB, data=train)
pred = predict(lettersForest, newdata=test)

table(test$letter, pred)

##last set

#1.2
census = read.csv('./data/census.csv')
library(caTools)
set.seed(2000)
split = sample.split(census, SplitRatio = 0.6)
train = subset(census, split==TRUE)
test = subset(census, split==FALSE)

censusglm = glm( over50k ~ . , data = train, family="binomial")
summary(censusglm)

pred = predict(censusglm, newdata=test,type="response")
table(test$over50k, pred > 0.5)

#AUC
library(ROCR)
ROCRpred = prediction(pred, test$over50k)
as.numeric(performance(ROCRpred, "auc")@y.values)

#2.1
library(rpart.plot)
CARTmodelC = rpart(over50k ~ ., data = train, method="class")
prp(CARTmodelC)

#AUC
library(ROCR)
predictTest = predict(CARTmodelC, newdata = test)
predictTest = predictTest[,2]
ROCRpred = prediction(predictTest, test$over50k)
as.numeric(performance(ROCRpred, "auc")@y.values)

#3.1
set.seed(1)
trainSmall = train[sample(nrow(train), 2000), ]

set.seed(1)
over50kForest = randomForest(over50k ~ ., data=trainSmall)
pred = predict(over50kForest, newdata = test)
table(test$over50k, pred)

vu = varUsed(over50kForest, count=TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(over50kForest$forest$xlevels[vusorted$ix]))

varImpPlot(over50kForest)

#4.1
set.seed(2)
library(caret)
fitControl = trainControl( method = "cv", number = 10 )
cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002))
train( over50k ~ . , data = train, method = "rpart", trControl = fitControl, tuneGrid = cartGrid )

CARTmodelCP = rpart(over50k ~ ., data = train, method="class", cp=.002)
pred = predict(CARTmodelCP, newdata=test, type="class")


