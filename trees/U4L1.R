quality = read.csv('./data/quality.csv')
table(quality$PoorCare)

#standard method is to predict baseline based on observed values
#(values form the table call above) 98/33 = baseline

#we need to splot our data set to obtain a trianing and test set
#using caTools
library(caTools)
set.seed(88)
#the ,75 used below reflects our calc fromabove
split = sample.split(quality$PoorCare, SplitRatio = 0.75)

#creating train and test sets
train = subset(quality, split == TRUE)
test = subset(quality, split == FALSE)

qualLog = glm(PoorCare ~ OfficeVisits + Narcotics, data=train, family=binomial)
qualLog2 = glm(PoorCare ~ StartedOnCombination + ProviderCount, data=train, family=binomial)

predictTrain = predict(qualLog, type='response')

tapply(predictTrain, train$PoorCare, mean)