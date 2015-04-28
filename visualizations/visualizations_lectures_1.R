who = read.csv('../data/WHO.csv')

library(ggplot2)

#old way of plotting
plot(who$GNI, who$FertilityRate)

#build ggplot object
scatterplot = ggplot(who, aes(x = GNI, y = FertilityRate))
#create scatter plot w object
scatterplot + geom_point()
#create line plot w object
scatterplot + geom_line()
#add point properties to a scatter plot
scatterplot + geom_point(color="blue", size=3, shape=17)
#add title and size to file
FertilityGNIPlot = scatterplot + geom_point(color="blue", size=3, shape=15) + ggtitle('Fertility Rate vs. Gross National Income')
pdf("myPlot.pdf")
print(FertilityGNIPlot)
#close the file
dev.off()

#building advanced plots... with color for region
scatplot2 = ggplot(who, aes(x = GNI, y = FertilityRate, color = Region)) #Region is factor var
scatplot2 = ggplot(who, aes(x = GNI, y = FertilityRate, color = LifeExpectancy)) #le is continous var
scatplot2 + geom_point(size = 2, shape = 20)

#looks non-linear
ggplot(who, aes(x = FertilityRate, y = Under15, color = Region)) + geom_point() + scale_color_brewer(palette="Dark2")
#log transformation
ggplot(who, aes(x = log(FertilityRate), y = Under15)) + geom_point()
#build model, looks like a good fit
model = lm(Under15 ~ FertilityRate, data = who)
#add model line to plot
ggplot(who, aes(x = log(FertilityRate), y = Under15)) + geom_point() + stat_smooth(method = "lm", level=0.99) #se = False removes confidence line #plot 99% confidence interval



