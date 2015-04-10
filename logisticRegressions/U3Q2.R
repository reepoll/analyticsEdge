pisa_test = read.csv('./data/pisa2009test.csv')
pisa = read.csv('./data/pisa2009train.csv')

nrow(pisa)
tapply(pisa$readingScore, pisa$male == 1, mean)
#find na's in a data frame
sapply(pisa, function(x) sum(is.na(x)))

pisa = na.omit(pisa)
pisa_test = na.omit(pisa_test)

pisa$raceeth = relevel(pisa$raceeth, "White")
pisa_test$raceeth = relevel(pisa_test$raceeth, "White")

#regression on all variables
lmScore = lm(readingScore ~ ., data = pisa)

#calc RMSE
RMSE = sqrt(mean((73.81)^2))

scorePred = predict(lmScore, newdata = pisa_test)
SSE = sum((scorePred - pisa_test$readingScore)^2)
RMSE = sqrt(SSE/nrow(pisa_test))

SST = mean(pisa$readingScore)
SSE_test = sum((SST - pisa_test$readingScore)^2)

SSE_model = sum((scorePred - pisa_test$readingScore)^2)

R2 = 1 - SSE_model / SSE_test
