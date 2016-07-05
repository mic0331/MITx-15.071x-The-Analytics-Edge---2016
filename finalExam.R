setwd("/media/mic0331/Data/Data Analysis/Courses/Edx/MITx: 15.071x The Analytics Edge")
fedFunds = read.csv('./dataset/federalFundsRate.csv', stringsAsFactors = F)
table (fedFunds$RaisedFedFunds)
294/(291+294)
table(fedFunds$Chairman)
fedFunds$Chairman <- as.factor(fedFunds$Chairman)
fedFunds$DemocraticPres <- as.factor(fedFunds$DemocraticPres)
fedFunds$RaisedFedFunds <- as.factor(fedFunds$RaisedFedFunds)
set.seed(201)
library(caTools)
spl = sample.split(fedFunds$RaisedFedFunds, 0.7)
training = subset(fedFunds, spl == TRUE)
testing = subset(fedFunds, spl == FALSE)
LogModel = glm(RaisedFedFunds ~ PreviousRate + Streak + Unemployment + HomeownershipRate + DemocraticPres + MonthsUntilElection, data=training, family="binomial")
summary(LogModel)
sample <- data.frame(PreviousRate=1.7,Streak=-3,Unemployment=5.1,HomeownershipRate=65.3,DemocraticPres=0,MonthsUntilElection=18 )
sample$DemocraticPres <- as.factor(sample$DemocraticPres)
pred <- predict(LogModel, newdata = sample, type = "response")
pred
summary(LogModel)
prediction <- predict(LogModel, newdata = testing, type="response")
table(testing$RaisedFedFunds, prediction > 0.5)

# Table outcome
table(testing$RaisedFedFunds)

60+57
60+57
87+88
(87-60)+(88-57)


library(ROCR)
ROCRpred = prediction(prediction, testing$RaisedFedFunds)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
ROCRauc <- as.numeric(performance(ROCRpred, "auc")@y.values)
ROCRauc


library(rpart)
library(rpart.plot)
library(caret)
set.seed(201)
numFolds = trainControl(method = "cv", number = 10)
cpGrid = expand.grid(.cp=seq(0.001,0.05,0.001))
train(RaisedFedFunds ~ PreviousRate + Streak + Unemployment + HomeownershipRate + DemocraticPres + MonthsUntilElection, data = training, method = "rpart", trControl=numFolds, tuneGrid=cpGrid)
tree <- rpart(RaisedFedFunds ~  PreviousRate + Streak + Unemployment + HomeownershipRate + DemocraticPres + MonthsUntilElection, data = training, method = "class", cp = .016)
prp(tree)
tree.predict <- predict(tree, newdata = sample)

tree.predict <- predict(tree, newdata = testing, type = "class")
t <- table(testing$RaisedFedFunds, tree.predict)
N <- nrow(testing)
TN <- t[1] 
FP <- t[3] 
FN <- t[2] 
TP <- t[4]
Acc <- (TN+TP)/N
Acc
prp(tree)

# -------------------------------------------------------------------

households = read.csv('./dataset/Households.csv')
subset(households, households$MorningPct == 100)
subset(households, households$AfternoonPct == 100)

subset(households, households$AvgSalesValue >= 150)
subset(households, households$AvgDiscount >= 25)

x<-(subset(households, households$NumVisits >= 300))
148/2500

library(caret)
preproc = preProcess(households)
HouseholdsNorm = predict(preproc, households)
max(HouseholdsNorm$NumVisits)
min(HouseholdsNorm$AfternoonPct)
set.seed(200)
distances <- dist(HouseholdsNorm, method = "euclidean")
ClusterShoppers <- hclust(distances, method = "ward.D")
plot(ClusterShoppers, labels = FALSE)
set.seed(200)
k=10
KMC = kmeans(HouseholdsNorm, centers = k)
str(KMC)
min(KMC$size)
max(KMC$size)

table(KMC$cluster, HouseholdsNorm$MorningPct)
table(KMC$cluster, HouseholdsNorm$AvgProdCount)
table(KMC$cluster, HouseholdsNorm$NumVisits)


set.seed(500)
k=5
KMC = kmeans(HouseholdsNorm, centers = k)
min(KMC$size)
max(KMC$size)

table(KMC$cluster, HouseholdsNorm$NumVisits)

# --------------------------------------------------------------------------------------------------------------------------------------

energy = read.csv('./dataset/energy.csv')
table(energy$GenTotalRenewable)
sort(tapply(energy$GenTotalRenewable, energy$STATE, mean))
id = subset(energy, energy$STATE == 'ID')

sort(tapply(energy$AllSourcesCO2, energy$presidential.results, mean, na.rm = TRUE))
rep = subset(energy, energy$presidential.results == 0)
dem = subset(energy, energy$presidential.results == 1)
sort(tapply(rep$AllSourcesCO2, rep$YEAR, mean, na.rm = TRUE))
sort(tapply(rep$AllSourcesCO2, rep$STATE, mean, na.rm = TRUE))

sort(tapply(dem$AllSourcesCO2, dem$YEAR, mean, na.rm = TRUE))
sort(tapply(dem$AllSourcesCO2, dem$STATE, mean, na.rm = TRUE))
cor(energy$AllSourcesCO2, energy$EsalesIndustrial, use="complete")
cor(energy$AllSourcesSO2, energy$EsalesIndustrial, use="complete")
cor(energy$AllSourcesNOx, energy$EsalesResidential, use="complete")

boxplot(EPriceTotal ~ STATE, data = energy)
sort(tapply(energy$EPriceTotal, energy$STATE, mean))
sort(tapply(energy$GenTotal, energy$STATE, mean))

set.seed(144)
spl = sample(1:nrow(energy), size = 0.7*nrow(energy))
train = energy[spl,]
test = energy[-spl,]
mod = glm(GenSolarBinary ~  GenHydro + GenSolar + CumlFinancial + CumlRegulatory + Total.salary + Import, data = train, , family="binomial")
summary(mod)

rep <- subset(test, test$presidential.results == 0)
dem <- subset(test, test$presidential.results == 1)
mod.predict <- predict(mod, newdata=test, type="response")
t <- table(test$GenSolarBinary, mod.predict >= .5)
N <- nrow(test)
TN <- t[1] 
FP <- t[3] 
FN <- t[2] 
TP <- t[4]
Acc <- (TN+TP)/N
Acc

mod.predict <- predict(mod, newdata=rep, type="response")
t <- table(rep$GenSolarBinary, mod.predict >= .5)
N <- nrow(rep)
TN <- t[1] 
FP <- t[3] 
FN <- t[2] 
TP <- t[4]
Acc <- (TN+TP)/N
Acc

mod.predict <- predict(mod, newdata=dem, type="response")
t <- table(dem$GenSolarBinary, mod.predict >= .5)
N <- nrow(dem)
TN <- t[1] 
FP <- t[3] 
FN <- t[2] 
TP <- t[4]
Acc <- (TN+TP)/N
Acc


library(caret)
train.limited = train[,-which(names(train) == "GenSolarBinary")]
test.limited = test[,-which(names(test) == "GenSolarBinary")]
preproc = preProcess(train.limited)







HouseholdsNorm = predict(preproc, households)
max(HouseholdsNorm$NumVisits)
min(HouseholdsNorm$AfternoonPct)
set.seed(200)
distances <- dist(HouseholdsNorm, method = "euclidean")
ClusterShoppers <- hclust(distances, method = "ward.D")
plot(ClusterShoppers, labels = FALSE)
set.seed(200)
k=10
KMC = kmeans(HouseholdsNorm, centers = k)
str(KMC)
min(KMC$size)
max(KMC$size)
