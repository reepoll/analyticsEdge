cps = read.csv('./data/CPSData.csv')
sort(table(cps$Region))

table(cps$Region, is.na(cps$Married))
table(cps$Age, is.na(cps$Married))
#2.5
sort(tapply(is.na(cps$MetroAreaCode), cps$State, mean))

cc = read.csv('./data/CountryCodes.csv')
metro = read.csv('./data/MetroAreaCodes.csv')

#merge dat library
cps = merge(cps, metro, by.x='MetroAreaCode', by.y='Code', all.x=TRUE)
table(is.na(cps$MetroArea))
#3.5
sort(tapply(cps$Race == 'Asian', cps$MetroArea, mean))
#3.6
sort(tapply(cps$Education == 'No high school diploma', cps$MetroArea, mean, na.rm=TRUE))

#merge data library
cps = merge(cps, cc, by.x='CountryOfBirthCode', by.y='Code', all.x=TRUE)

#4.1
table(is.na(cps$Country))
tapply(cps$Country != 'United States', cps$MetroArea == 'New York-Northern New Jersey-Long Island, NY-NJ-PA', mean, na.rm=TRUE)
sort(tapply(cps$Country == 'India', cps$MetroArea, mean, na.rm=TRUE))
sort(tapply(cps$Country == 'Brazil', cps$MetroArea, mean, na.rm=TRUE))
sort(tapply(cps$Country == 'Somalia', cps$MetroArea, mean, na.rm=TRUE))

