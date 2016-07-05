setwd("/media/mic0331/Data/Data Analysis/Courses/Edx/MITx: 15.071x The Analytics Edge")
NBA = read.csv("./dataset/NBA_train.csv")
str(NBA)
table(NBA$W, NBA$Playoffs)
NBA$PTSdiff = NBA$PTS - NBA$oppPTS
plot(NBA$PTSdiff, NBA$W)
WinsReg = lm(W~PTSdiff, data = NBA)
summary(WinsReg)

W = 41 + .0326 * PTSdiff

PointsReg = lm(PTS ~X2PA + X3PA + FTA + AST + ORB + DRB + TOV + STL + BLK, data = NBA)
summary(PointsReg)
PointsReg$residuals
SSE = sum(PointsReg$residuals^2)
SSE
RMSE = sqrt(SSE/nrow(NBA))
RMSE
mean(NBA$PTS)
summary(PointsReg)
PointsReg2 = lm(PTS ~X2PA + X3PA + FTA + AST + ORB + DRB + STL + BLK, data = NBA)
summary(PointsReg2)
PointsReg3 = lm(PTS ~X2PA + X3PA + FTA + AST + ORB + STL + BLK, data = NBA)
summary(PointsReg3)
PointsReg4 = lm(PTS ~X2PA + X3PA + FTA + AST + ORB + STL, data = NBA)
summary(PointsReg4)
SSE = sum(PointsReg4$residuals^2)
RMSE = sqrt(SSE/nrow(NBA))
SSE
RMSE
NBA_test = read.csv("./dataset/NBA_test.csv")
PointsPredictions = predict(PointsReg4, newdata = NBA_test)
SSE = sum((PointsPredictions - NBA_test$PTS)^2)
SST = sum((mean(NBA$PTS) - NBA_test$PTS)^2)
R2 = 1 - SSE / SST
RMSE = sqrt(SSE/nrow(NBA_test))
RMSE

