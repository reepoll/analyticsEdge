wine = read.csv('./data/wine.csv')
wine_test = read.csv('./data/wine_test.csv')

model_1 = lm(Price ~ AGST, data=wine)
model_2 = lm(Price ~ AGST + HarvestRain, data = wine)
model_3 = lm(Price ~ WinterRain + AGST + HarvestRain + Age + FrancePop, data=wine)

#correlations
cor(wine$Age, wine$FrancePop)
cor(wine)

#predictions
predictTest = predict(model_4, newdata=wine_test)
SSE = sum((wine_test$Price - predictTest)^2)
SST = sum((wine_test$Price - mean(wine$Price))^2)
1 - SSE/SST