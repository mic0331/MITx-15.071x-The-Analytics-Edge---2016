setwd("/media/mic0331/Data/Data Analysis/Courses/Edx/MITx: 15.071x The Analytics Edge")
dailykos <- read.csv("./dataset/dailykos.csv")
str(dailykos)

distance = dist(dailykos, method="euclidian")
cluster = hclust(distance, method="ward.D")
plot(cluster)
dailykosClusters = cutree(cluster, k=7)
hist_cluster1 <- subset(dailykos, dailykosClusters == 1)
hist_cluster2 <- subset(dailykos, dailykosClusters == 2)
hist_cluster3 <- subset(dailykos, dailykosClusters == 3)
hist_cluster4 <- subset(dailykos, dailykosClusters == 4)
hist_cluster5 <- subset(dailykos, dailykosClusters == 5)
hist_cluster6 <- subset(dailykos, dailykosClusters == 6)
hist_cluster7 <- subset(dailykos, dailykosClusters == 7)

tail(sort(colMeans(hist_cluster1)))
tail(sort(colMeans(hist_cluster2)))
tail(sort(colMeans(hist_cluster3)))
tail(sort(colMeans(hist_cluster4)))
tail(sort(colMeans(hist_cluster5)))
tail(sort(colMeans(hist_cluster6)))
tail(sort(colMeans(hist_cluster7)))

set.seed(1000)
k=7
KMC = kmeans(dailykos, centers = k)
str(KMC)
cluster1 <- subset(dailykos, KMC$cluster == 1)
cluster2 <- subset(dailykos, KMC$cluster == 2)
cluster3 <- subset(dailykos, KMC$cluster == 3)
cluster4 <- subset(dailykos, KMC$cluster == 4)
cluster5 <- subset(dailykos, KMC$cluster == 5)
cluster6 <- subset(dailykos, KMC$cluster == 6)
cluster7 <- subset(dailykos, KMC$cluster == 7)

tail(sort(colMeans(cluster1)))
tail(sort(colMeans(cluster2)))
tail(sort(colMeans(cluster3)))
tail(sort(colMeans(cluster4)))
tail(sort(colMeans(cluster5)))
tail(sort(colMeans(cluster6)))
tail(sort(colMeans(cluster7)))

table(KMC$cluster)
table(dailykosClusters)
table(dailykosClusters, KMC$cluster)

# ----------------------------------------------------------------------------------------------------------------------------------------

airlines <- read.csv("./dataset/AirlinesCluster.csv")
summary(airlines)
library(caret)
preproc = preProcess(airlines)
airlinesNorm = predict(preproc, airlines)
summary(airlinesNorm)
distance = dist(airlinesNorm, method="euclidian")
cluster = hclust(distance, method="ward.D")
plot(cluster)
clusters = cutree(cluster, k=5)
table(clusters)

lapply(split(airlines, clusters), colMeans)
set.seed(88)
k=5
KMC = kmeans(airlinesNorm, centers = k)
table(KMC$size)


airVec2=c(tapply(airlines$Balance, clusters, mean),
          tapply(airlines$QualMiles, KMC$cluster, mean),
          tapply(airlines$BonusMiles, KMC$cluster, mean),
          tapply(airlines$BonusTrans, KMC$cluster, mean),
          tapply(airlines$FlightMiles, KMC$cluster, mean),
          tapply(airlines$FlightTrans, KMC$cluster, mean),
          tapply(airlines$DaysSinceEnroll, clusters, mean))
dim(airVec2) = c(5, 7)
colnames(airVec2) = c("Balance", "QualMiles", "BonusMiles", "BonusTrans", "FlightMiles", "FlightTrans", "DaysEnroll")
airVec2

# --------------------------------------------------------------------------------------------------------------------------------------

stocks <- read.csv("./dataset/StocksCluster.csv")
table(stocks$PositiveDec)
6324/(5256+6324)
mean(stocks$PositiveDec)
summary(stocks)
library(caTools)
set.seed(144)
spl = sample.split(stocks$PositiveDec, SplitRatio = 0.7)
stocksTrain = subset(stocks, spl == TRUE)
stocksTest = subset(stocks, spl == FALSE)
StocksModel = glm(PositiveDec ~., data = stocksTrain, family = "binomial")
StocksModel.predict <- predict(StocksModel, type="response")
t <- table(stocksTrain$PositiveDec, StocksModel.predict >= .5)
N <- nrow(stocksTrain)
TN <- t[1] 
FP <- t[3] 
FN <- t[2] 
TP <- t[4]
Acc <- (TN+TP)/N
Acc

StocksModel.predict <- predict(StocksModel, type="response", newdata = stocksTest)
t <- table(stocksTest$PositiveDec, StocksModel.predict >= .5)
N <- nrow(stocksTest)
TN <- t[1] 
FP <- t[3] 
FN <- t[2] 
TP <- t[4]
Acc <- (TN+TP)/N
Acc
table(stocks$PositiveDec)
6324/(5256+6324)

limitedTrain = stocksTrain
limitedTrain$PositiveDec = NULL
limitedTest = stocksTest
limitedTest$PositiveDec = NULL

library(caret)
preproc = preProcess(limitedTrain)
normTrain = predict(preproc, limitedTrain)
normTest = predict(preproc, limitedTest)

mean(normTrain$ReturnJan)
mean(normTest$ReturnJan)

set.seed(144)
k = 3
km = kmeans(normTrain, centers = k)
str(km)

library(flexclust)
km.kcca = as.kcca(km, normTrain)
clusterTrain = predict(km.kcca)
clusterTest = predict(km.kcca, newdata=normTest)

sum(clusterTest == 2)

stocksTrain1 = subset(stocksTrain, clusterTrain == 1)
stocksTrain2 = subset(stocksTrain, clusterTrain == 2)
stocksTrain3 = subset(stocksTrain, clusterTrain == 3)

stocksTest1 = subset(stocksTest, clusterTest == 1)
stocksTest2 = subset(stocksTest, clusterTest == 2)
stocksTest3 = subset(stocksTest, clusterTest == 3)

mean(stocksTrain1$PositiveDec)
mean(stocksTrain2$PositiveDec)
mean(stocksTrain3$PositiveDec)

StocksModel1 = glm(PositiveDec ~., data = stocksTrain1, family = "binomial")
StocksModel2 = glm(PositiveDec ~., data = stocksTrain2, family = "binomial")
StocksModel3 = glm(PositiveDec ~., data = stocksTrain3, family = "binomial")

summary(StocksModel1)
summary(StocksModel2)
summary(StocksModel3)

PredictTest1 <- predict(StocksModel1, type="response", newdata = stocksTest1)
PredictTest2 <- predict(StocksModel2, type="response", newdata = stocksTest2)
PredictTest3 <- predict(StocksModel3, type="response", newdata = stocksTest3)
t <- table(stocksTest1$PositiveDec, PredictTest1 >= .5)
N <- nrow(stocksTest1)
TN <- t[1] 
FP <- t[3] 
FN <- t[2] 
TP <- t[4]
Acc <- (TN+TP)/N
Acc

t <- table(stocksTest2$PositiveDec, PredictTest2 >= .5)
N <- nrow(stocksTest2)
TN <- t[1] 
FP <- t[3] 
FN <- t[2] 
TP <- t[4]
Acc <- (TN+TP)/N
Acc

t <- table(stocksTest3$PositiveDec, PredictTest3 >= .5)
N <- nrow(stocksTest3)
TN <- t[1] 
FP <- t[3] 
FN <- t[2] 
TP <- t[4]
Acc <- (TN+TP)/N
Acc

AllPredictions = c(PredictTest1, PredictTest2, PredictTest3)
AllOutcomes = c(stocksTest1$PositiveDec, stocksTest2$PositiveDec, stocksTest3$PositiveDec)

t <- table(AllOutcomes, AllPredictions >= .5)
N <- length(AllOutcomes)
TN <- t[1] 
FP <- t[3] 
FN <- t[2] 
TP <- t[4]
Acc <- (TN+TP)/N
Acc

