setwd("/media/mic0331/Data/Data Analysis/Courses/Edx/MITx: 15.071x The Analytics Edge")
wine = read.csv("./dataset/wine.csv")
str(wine)
summary(wine)
model1 = lm(Price ~ AGST, data = wine)
summary(model1)
model1$residuals
SSE = sum(model1$residuals^2)
SSE
model2 = lm(Price ~ AGST + HarvestRain, data = wine)
summary(model2)
SSE = sum(model2$residuals^2)
SSE
model3 = lm(Price ~ AGST + HarvestRain + WinterRain + Age + FrancePop, data = wine)
summary(model3)
SSE = sum(model3$residuals^2)
SSE
model = lm(Price ~ HarvestRain + WinterRain, data = wine)
summary(model)
model4 = lm(Price ~AGST + HarvestRain + WinterRain + Age, data = wine)
summary(model4)
cor(wine$WinterRain, wine$Price)
cor(wine$Age, wine$FrancePop)
cor(wine)
model5 = lm(Price ~ AGST + HarvestRain + WinterRain, data = wine)
summary(model5)
cor(wine$HarvestRain, wine$WinterRain)
wineTest = read.csv("./dataset/wine_test.csv")
str(wineTest)
predictTest = predict(model4, newdata = wineTest)
predictTest
SSE = sum((wineTest$Price - predictTest) ^ 2)
SST = sum((wineTest$Price - mean(wine$Price)) ^2)
1 - SSE/SST

baseball = read.csv("./dataset/baseball.csv")
str(baseball)
moneyball = subset(baseball, Year < 2002)
str(moneyball)
moneyball$RD = moneyball$RS - moneyball$RA
str(moneyball)
plot(moneyball$RD, moneyball$W)
WinsReg = lm(W ~ RD, data = moneyball)
summary(WinsReg)

80.8814+0.1058*(713-614)

str(moneyball)
RunsReg = lm(RS ~OBP + SLG + BA, data=moneyball)
summary(RunsReg)
RunsReg = lm(RS ~OBP + SLG, data=moneyball)
summary(RunsReg)

-804.63+2737.77*.311+1584.91*.405
-837.38+2913.6*.297+1514.29*.370

teamRank = c(1,2,3,3,4,4,4,4,5,5)
wins2012 = c(94,88,95,88,93,94,98,97,93,94)
wins2013 = c(97,97,92,93,92,96,94,96,92,90)
cor(teamRank, wins2012)
cor(teamRank, wins2013)














