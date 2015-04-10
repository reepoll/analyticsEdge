nba = read.csv('./data/NBA_train.csv')
nba_test = read.csv('./data/NBA_test.csv')

nba$PTSdiff = nba$PTS - nba$oppPTS
plot(nba$PTSdiff, nba$W)
WinsReg = lm(W ~ PTSdiff, data=nba)
summary(WinsReg)

PTSreg = lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + DRB + TOV + STL + BLK, data=nba)
summary(PTSreg)

#calling residiuals
PTSreg$residuals
SSE = sum(PTSreg$residuals^2)
RMSE = sqrt(SSE/nrow(nba))
PTSreg3 = lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + STL + BLK, data=nba)