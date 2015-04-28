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