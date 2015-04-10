cc = read.csv('./data/climate_change.csv')

train = subset(cc, Year <= 2006)
test = subset(cc, Year > 2006)

TempModel1 = lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols, data = train)
TempModel2 = lm(Temp ~ MEI + TSI + Aerosols + N2O, data = train)

#using step
TempModelSteps = step(TempModel1)
summary(TempModelSteps)

#predict
PredictModel = predict(TempModelSteps, newdata=test)
SSE = sum((PredictModel - test$Temp)^2)
SST = sum((mean(train$Temp) - test$Temp)^2)
R2 = 1 - SSE/SST
RMSE = 1 - sqrt(SSE/nrow(test))