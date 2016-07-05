setwd("/media/mic0331/Data/Data Analysis/Courses/Edx/MITx: 15.071x The Analytics Edge")
quality <- read.csv('./dataset/quality.csv')
str(quality)
table(quality$PoorCare)
98/(98+33)
install.packages('caTools')
library('caTools')
set.seed(88)
split = sample.split(quality$PoorCare, SplitRatio = .75)
split
qualityTrain = subset(quality, split == TRUE)
qualityTest = subset(quality, split == FALSE)
#spl = sample(1:nrow(data), size=0.7 * nrow(data))
#train = data[spl,]
#test = data[-spl,]
nrow(qualityTrain)
nrow(qualityTest)
QualityLog = glm(PoorCare ~ OfficeVisits + Narcotics, data = qualityTrain, family = binomial)
summary(QualityLog)
predictTrain = predict(QualityLog, type = "response")
summary(predictTrain)
tapply(predictTrain, qualityTrain$PoorCare, mean)


QualityLog = glm(PoorCare ~ StartedOnCombination + ProviderCount, data = qualityTrain, family = binomial)
summary(QualityLog)
predictTrain = predict(QualityLog, type = "response")
summary(predictTrain)
tapply(predictTrain, qualityTrain$PoorCare, mean)

# ---------------------------------------------------------------------------------------------------------------------------------------

framingham = read.csv("./dataset/framingham.csv")
str(framingham)
set.seed(1000)
split = sample.split(framingham$TenYearCHD, SplitRatio = .65)
train = subset(framingham, split == TRUE)
test = subset(framingham, split == FALSE)
install.packages("ROCR")
library(ROCR)
framinghamLog = glm(TenYearCHD ~ ., data = train, family = binomial)
summary(framinghamLog)

predictTest = predict(framinghamLog, type = 'response', newdata = test)
table(test$TenYearCHD, predictTest > .5)
accuracy <- (1069+11)/(1069+6+187+11)
accuracy_baseline <- (1069+6)/(1069+6+187+11)
ROCRpred = prediction(predictTest, test$TenYearCHD)
as.numeric(performance(ROCRpred, "auc")@y.values)

11/(187+11)
1069/(1069+6)












