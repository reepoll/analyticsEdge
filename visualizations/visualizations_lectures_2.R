#HEATMAPS
library(ggplot2)
mvt = read.csv('../data/mvt.csv', stringsAsFactors = FALSE)

mvt$Date = strptime(mvt$Date, format="%m/%d/%y %H:%M")
mvt$Weekday = weekdays(mvt$Date)
mvt$Hour = mvt$Date$hour

WeekdayCounts = as.data.frame(table(mvt$Weekday))

ggplot(WeekdayCounts, aes(x = Var1, y = Freq)) + geom_line(aes(group=1), alpha=.3) + xlab("Day of the Week") + ylab("Total Vehicle Thefts")
#create an ordered factor variable
WeekdayCounts$Var1 = factor(WeekdayCounts$Var1, ordered=TRUE, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

#going for heatmap
DayHourCounts = as.data.frame(table(mvt$Weekday, mvt$Hour))
#need to create numeric hour var
DayHourCounts$Hour = as.numeric(as.character(DayHourCounts$Var2))
DayHourCounts$Var1 = factor(WeekdayCounts$Var1, ordered=TRUE, levels=c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
ggplot(DayHourCounts, aes(x = Hour, y = Freq)) + geom_line(aes(group=Var1, color=Var1, size = 2))

#create heatmap, w labels, and custom gradient
ggplot(DayHourCounts, aes(x = Hour, y = Var1)) + geom_tile(aes(fill=Freq)) + scale_fill_gradient(name="Total MV Thefts", low="white", high="red") + theme(axis.title.y = element_blank())

#load maps
library(maps)
library(ggmap)

chicago = get_map(location =  "chicago", zoom = 11)
ggmap(chicago)
#first look at data
ggmap(chicago) + geom_point(data=mvt[1:100,],aes(x=Longitude,y=Latitude))
#format the data
LatLongCounts = as.data.frame(table(round(mvt$Latitude,2),round(mvt$Longitude,2)))
LatLongCounts$Lat = as.numeric(as.character(LatLongCounts$Var1))
LatLongCounts$Long = as.numeric(as.character(LatLongCounts$Var2))
#COOL PLOT
ggmap(chicago) + geom_point(data=LatLongCounts, aes(x=Long, y=Lat, color = Freq, size = Freq)) + scale_color_gradient(low="yellow", high="red")
#more like a typical heat map AWESOME
ggmap(chicago) + geom_tile(data=LatLongCounts, aes(x=Long, y=Lat, alpha=Freq), fill="red")

#removing tile of freq = 0... tiles over water
LatLongCounts2 = subset(LatLongCounts, Freq>0)
ggmap(chicago) + geom_tile(data=LatLongCounts2, aes(x=Long, y=Lat, alpha=Freq), fill="red")

##FBI murders
murders = read.csv('../data/murders.csv')
statesMap = map_data("state")
#create an R plot of the USA
ggplot(statesMap, aes(x=long, y=lat, group=group)) + geom_polygon(fill="white", color="black")
#create a region variable in murders df to match the states df
murders$region = tolower(murders$State)
#now merge the dfs on region
murderMap = merge(statesMap, murders, by="region")
ggplot(murderMap, aes(x=long, y=lat, group=group, fill=Murders)) + geom_polygon(color="black") + scale_fill_gradient(low="black", high="red", guide="legend")
#create murder rate var
murderMap$Rate = murderMap$Murders/murderMap$Population * 100000
ggplot(murderMap, aes(x=long, y=lat, group=group, fill=Rate)) + geom_polygon(color="black") + scale_fill_gradient(low="black", high="red", guide="legend")
#remove DC (high outlier)
ggplot(murderMap, aes(x=long, y=lat, group=group, fill=Rate)) + geom_polygon(color="black") + scale_fill_gradient(low="black", high="red", guide="legend", limits=c(0,10))
#gunownership rates'
murderMap$GunRate = murderMap$GunOwnership/murderMap$Population
ggplot(murderMap, aes(x=long, y=lat, group=group, fill=GunRate)) + geom_polygon(color="black") + scale_fill_gradient(low="black", high="red", guide="legend")