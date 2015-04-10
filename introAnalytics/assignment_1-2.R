boeing = read.csv('./data/BoeingStock.csv')
coke = read.csv('./data/CocaColaStock.csv')
ibm = read.csv('./data/IBMStock.csv')
ge = read.csv('./data/GEStock.csv')
pg = read.csv('./data/ProcterGambleStock.csv')

boeing$Date = as.Date(boeing$Date, '%m/%d/%y')
ibm$Date = as.Date(ibm$Date, "%m/%d/%y")
ge$Date = as.Date(ge$Date, "%m/%d/%y")
coke$Date = as.Date(coke$Date, "%m/%d/%y")
pg$Date = as.Date(pg$Date, "%m/%d/%y")

qplot(data = coke, x = Date, y = StockPrice, geom = 'line')
#or
plot(x = coke$Date, y = coke$StockPrice, type = 'l', col = 'red')
lines(pg$Date, pg$StockPrice, col = 'blue')
#draw vert line
abline(v=as.Date(c('2000-03-01')), lwd=2)

plot(coke$Date[301:432], coke$StockPrice[301:432], type='l', col='red', ylim=c(0,210))
lines(boeing$Date[301:432], boeing$StockPrice[301:432], col='blue')
lines(ge$Date[301:432], ge$StockPrice[301:432], col='orange')
lines(ibm$Date[301:432], ibm$StockPrice[301:432], col='purple')
lines(pg$Date[301:432], pg$StockPrice[301:432], col='green')
abline(v=as.Date(c('2000-02-01')))
abline(v=as.Date(c('1997-09-01')))
abline(v=as.Date(c('1997-12-01')))

ibm_mon = tapply(ibm$StockPrice, months(ibm$Date), mean)
ibm_log = ibm_mon > mean(ibm$StockPrice)

