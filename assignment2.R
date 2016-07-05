setwd("/media/mic0331/Data/Data Analysis/Courses/Edx/MITx: 15.071x The Analytics Edge")
pisaTrain = read.csv("./dataset/pisa2009train.csv")
pisaTest = read.csv("./dataset/pisa2009test.csv")
pisaTrain = na.omit(pisaTrain)
pisaTest = na.omit(pisaTest)

pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")
lmScore = lm(readingScore ~ ., data = pisaTrain)
summary(lmScore)
prediction = predict(lmScore, newdata = pisaTest)
summary(prediction)

#baseline
mean(pisaTrain$readingScore)
SST = sum((pisaTest$readingScore - mean(pisaTrain$readingScore)) ^2)
SST

SSE = sum((pisaTest$readingScore - prediction) ^ 2)

1 - SSE/SST
# ------------------------------------------------------------------------------------------------------------------------------------
FluTrain = read.csv("./dataset/FluTrain.csv")
summary(FluTrain)
idx <- which.max(FluTrain$ILI)
FluTrain[idx,]
idx <- which.max(FluTrain$Queries)
FluTrain[idx,]
hist(FluTrain$ILI)
plot(log(FluTrain$ILI), FluTrain$Queries)
FluTrend1 <- lm(log(ILI) ~ Queries, data = FluTrain)
summary(FluTrend1)
correlation <- cor(log(FluTrain$ILI), FluTrain$Queries)
correlation^2
FluTest = read.csv("./dataset/FluTest.csv")
PredTest1 = exp(predict(FluTrend1, newdata=FluTest))
idx <- which(FluTest$Week == "2012-03-11 - 2012-03-17")
FluTest[idx,]
FluTest

PredTest_ = exp(predict(FluTrend1, newdata=FluTest[idx,]))
PredTest_

(FluTest[idx,]$ILI - PredTest_) / FluTest[idx,]$ILI

SSE = sum(FluTrend1$residuals^2)
SSE = sum((PredTest1 - FluTest$ILI)^2)
RMSE = sqrt(SSE/nrow(FluTest))
RMSE

install.packages("zoo")
library("zoo")
# ILILag2 contains the ILI value from 2 weeks before the current observation
# return 2 observations before the current one; a positive value would have returned future observations. 
# The parameter na.pad=TRUE means to add missing values for the first two weeks of our dataset, 
# where we can't compute the data from 2 weeks earlier
ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)
FluTrain$ILILag2 = coredata(ILILag2)
summary(ILILag2)
plot(log(FluTrain$ILILag2), log(FluTrain$ILI))
FluTrend2 <- lm(log(ILI) ~ Queries + log(ILILag2), data = FluTrain)
summary(FluTrend2)
ILILag2 = lag(zoo(FluTest$ILI), -2, na.pad=TRUE)
FluTest$ILILag2 = coredata(ILILag2)
summary(FluTest$ILILag2)

n = nrow(FluTrain)
FluTest$ILILag2[1] = FluTrain$ILI[n-1]
FluTest$ILILag2[2] = FluTrain$ILI[n]
PredTest = exp(predict(FluTrend2, newdata=FluTest))
SSE = sum((PredTest - FluTest$ILI)^2)
RMSE = sqrt(SSE/nrow(FluTest))
RMSE

# ------------------------------------------------------------------------------------------------------------------------------------

# state = read.csv("./dataset/statedata.csv")
data(state)
statedata = cbind(data.frame(state.x77), state.abb, state.area, state.center,  state.division, state.name, state.region)

full_model <- lm(Life.Exp ~ Population + Income + Illiteracy + Murder + HS.Grad + Frost + Area, data = statedata)
summary(full_model)

small_model <- lm(Life.Exp ~ Population + Murder + HS.Grad + Frost, data = statedata)
summary(small_model)
sort(predict(small_model))
statedata[which.min(statedata$Life.Exp),]
statedata[which.max(statedata$Life.Exp),]

which.min(small_model$residuals)
which.max(small_model$residuals)

# ------------------------------------------------------------------------------------------------------------------------------------

data = read.csv("./dataset/elantra.csv")
train = subset(data, data$Year <= 2012)
test = subset(data, data$Year > 2012)
model = lm(ElantraSales ~ Unemployment + CPI_all + CPI_energy + Queries, data = train)
summary(model)

model_season = lm(ElantraSales ~ Month +  Unemployment + CPI_all + CPI_energy + Queries, data = train)
summary(model_season)

110.69 * (3 - 1)
110.69 * (5 - 1) 

train$Month_f <- as.factor(train$Month)
test$Month_f <- as.factor(test$Month)
model_season = lm(ElantraSales ~ Month_f +  Unemployment + CPI_all + CPI_energy + Queries, data = train)
summary(model_season)
cor(train$CPI_all, train$CPI_energy)
cor(train$Queries, train$CPI_all)

model_season = lm(ElantraSales ~ Month_f +  Unemployment + CPI_all + CPI_energy + Queries, data = train)
summary(model_season)

model_season_1 = lm(ElantraSales ~ Month_f +  Unemployment + CPI_all + CPI_energy, data = train)
summary(model_season_1)

prediction <- predict(model_season_1, newdata = test)
SSE = sum((test$ElantraSales - prediction) ^ 2)
SSE
mean(train$ElantraSales)
SST = sum((test$ElantraSales - mean(train$ElantraSales)) ^2)
1 - SSE/SST

max(abs(test$ElantraSales - prediction))

table(test$Year, test$Month, abs(test$ElantraSales - prediction))

which.max(abs(prediction - test$ElantraSales))
test[5,]
