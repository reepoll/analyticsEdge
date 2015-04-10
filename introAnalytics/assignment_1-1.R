#Loading the data
nrowscrime = read.csv('./data/MVTweek1.csv')
str(crime)
max(crime$ID)
min(crime$Beat)
arrests = subset(crime, Arrest == TRUE)
nrow(arrests)
nrow(subset(crime, LocationDescription == 'ALLEY'))

#Dates
DateConvert = as.Date(strptime(crime$Date, '%m/%d/%y %H:%M'))
crime$Month = months(DateConvert)
crime$Weekday = weekdays(DateConvert)
crime$Date = DateConvert
table(crime$Month)
table(crime$Weekday)
arrests = subset(crime, Arrest == TRUE)
table(arrests$Month)

#Visualizing Crime Trends
hist(crime$Date, breaks=100)
boxplot(crime$Date ~ crime$Arrest)
table(subset(crime, Year == 2001)$Arrest)
table(subset(crime, Year == 2007)$Arrest)

#4. Popular Locations 
sort(table(crime$LocationDescription))
top5 = subset(crime, LocationDescription == 'STREET' | LocationDescription == 'PARKING LOT/GARAGE(NON.RESID.)' | LocationDescription == 'ALLEY' | LocationDescription == 'GAS STATION' | LocationDescription == 'DRIVEWAY - RESIDENTIAL')
nrow(top5)
##refresh factor
top5$LocationDescription = factor(top5$LocationDescription)
table(top5$LocationDescription)


