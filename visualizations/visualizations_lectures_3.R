mit = read.csv('../data/intl.csv')
library(ggplot2)
ggplot(mit, aes(x=Region, y=PercentOfIntl)) + geom_bar(stat="identity") + geom_text(aes(label=PercentOfIntl))

#reorder factor to sort bar from high to low
mit = transform(mit, Region = reorder(Region, -PercentOfIntl))
#make decimals > 0
mit$PercentOfIntl = mit$PercentOfIntl*100
#fix labels
ggplot(mit, aes(x=Region, y=PercentOfIntl)) + 
        geom_bar(stat="identity", fill="dark blue") + 
        geom_text(aes(label=PercentOfIntl), vjust=-0.4) + 
        ylab("Percent of International Students") +
        theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))
