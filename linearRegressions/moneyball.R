base = read.csv('./data/baseball.csv')
moneyball = subset(base, Year < 2002)str

#how many wins to get into the playoffs
qplot(data=base, x = W, y=Team, color=Playoffs)

#how many runs needed for 95 wins
moneyball$RD = moneyball$RS - moneyball$RA
plot(moneyball$RD, moneyball$W)
WinsReg = lm(W ~ RD, data=moneyball)
#then solve linear equation based on summary (WinsReg)

#how to score more runs
RunsReg = lm(RS ~ SLG + OBP, data=moneyball)
OpsRunsReg = lm(RA ~ OOBP + OSLG, data=moneyball)


teamRank = c(1,2,3,3,4,4,4,4,5,5)
wins2012 = c(94,88,95,88,93,94,98,97,93,94)
wins2013 = c(97,97,92,93,92,96,94,96,92,90)