flu = read.csv('./data/FluTrain.csv')

#max ILI week in data frame
flu$Week[match(max(flu$ILI), flu$ILI)]

#max query in df
flu$Week[match(max(flu$Queries), flu$Queries)]

#plots
hist(flu$ILI)
plot(log(flu$ILI), flu$Queries)

#creating LMs
FluTrend1 = lm(log(ILI) ~ Queries, data=flu)
summary(FluTrend1)

fluTest = read.csv('./data/FluTest.csv')

#create lm of a log relationship?
PredTest1 = exp(predict(FluTrend1, newdata = fluTest))

which(fluTest$Week == "2012-03-11 - 2012-03-17")
PredTest1[11]

fluTest$ILI[11]-PredTest1[11])/fluTest$ILI[11]

#RMS
RMS = sqrt(mean((fluTest$ILI - PredTest1)^2))
#or
SSE = sum((fluTest$ILI - PredTest1)^2)
RMS = sqrt(SSE/nrow(fluTest))


#add new lag data
library(zoo)

ILILag2 = lag(zoo(flu$ILI), -2, na.pad=TRUE)
flu$ILILag2 = coredata(ILILag2)

#find na's
sapply(flu, function(x) sum(is.na(x)))
 
#plot lags
plot(log(flu$ILILag2), log(flu$ILI))

#train a lm on Queries and Lag2
FluTrend2 = lm(log(ILI) ~ log(ILILag2) + Queries, data = flu)

#add new lag data to test
ILILag2 = lag(zoo(fluTest$ILI), -2, na.pad=TRUE)
fluTest$ILILag2 = coredata(ILILag2)

#find na's
sapply(fluTest, function(x) sum(is.na(x)))

#filling in the missing values
fluTest$ILILag2[1] = flu$ILI[416]
fluTest$ILILag2[2] = flu$ILI[417]

#RMS
PredTest2 = exp(predict(FluTrend2, newdata = fluTest))

SSE = sum((PredTest2 - fluTest$ILI)^2)
RMS = sqrt(SSE/nrow(fluTest))