setwd("/media/mic0331/Data/Data Analysis/Courses/Edx/MITx: 15.071x The Analytics Edge")
stevens <- read.csv("./dataset/stevens.csv")
str(stevens)
library(caTools)
set.seed(3000)
spl = sample.split(stevens$Reverse, SplitRatio=.7)
Train = subset(stevens, spl == TRUE)
Test = subset(stevens, spl == FALSE)
install.packages("rpart")
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)
StevensTree <- rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method = "class", minbucket=25)
prp(StevensTree)
PredictCART = predict(StevensTree, newdata = Test, type="class")
table(Test$Reverse, PredictCART)
(41+71)/(41+36+22+71)
library(ROCR)
PredictROC = predict(StevensTree, newdata = Test)
PredictROC
pred = prediction(PredictROC[,2], Test$Reverse)
perf = performance(pred, "tpr", "fpr")
plot(perf)

as.numeric(performance(pred, "auc")@y.values)

StevensTree2 <- rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method = "class", minbucket=5)
prp(StevensTree2)

StevensTree3 <- rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method = "class", minbucket=100)
prp(StevensTree3)

install.packages("randomForest")
library(randomForest)
stevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, nodesize = 25, ntree = 200)
# avoid the warning message because the dependant variable is not a factor (class prediction)
Train$Reverse = as.factor(Train$Reverse)
Test$Reverse = as.factor(Test$Reverse)
stevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, nodesize = 25, ntree = 200)
PredictForest = predict(stevensForest, newdata = Test)
table(Test$Reverse, PredictForest)
(40+74)/(40+37+19+74)

set.seed(100)
stevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, nodesize = 25, ntree = 200)
PredictForest = predict(stevensForest, newdata = Test)
table(Test$Reverse, PredictForest)
(43+74)/(43+34+19+74)

set.seed(200)
stevensForest = randomForest(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, nodesize = 25, ntree = 200)
PredictForest = predict(stevensForest, newdata = Test)
table(Test$Reverse, PredictForest)
(44+76)/(44+33+17+76)

install.packages("caret")
install.packages("e1071")
install.packages("class")
install.packages("ggplot2")

library(caret)
library(e1071)

numFolds = trainControl(method = "cv", number = 10)
cpGrid = expand.grid(.cp=seq(0.01,0.5,0.01))
train(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method = "rpart", trControl=numFolds, tuneGrid=cpGrid)
StevensTreeCV = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst, data = Train, method = "class", cp=.18)
PredictCV = predict(StevensTreeCV, newdata = Test, type = "class")
table(Test$Reverse, PredictCV)
(59+64)/(59+18+29+64)
prp(StevensTreeCV)

# ---------------------------------------------------------------------------------------------------------------------------------------

Claims <- read.csv("./dataset/ClaimsData.csv")
str(Claims)
table(Claims$bucket2009)/nrow(Claims)
library(caTools)
set.seed(88)
spl = sample.split(Claims$bucket2009, SplitRatio = .6)
ClaimsTrain = subset(Claims, spl == TRUE)
ClaimsTest = subset(Claims, spl == FALSE)
mean(ClaimsTrain$age)
table(ClaimsTrain$diabetes)[2] / nrow(ClaimsTrain)

table(ClaimsTest$bucket2009, ClaimsTest$bucket2008)
# accuracy
sum(diag(table(ClaimsTest$bucket2009, ClaimsTest$bucket2008))) / nrow(ClaimsTest)
# penalty error
PenaltyMetrix = matrix(c(0, 1, 2, 3, 4, 2, 0, 1, 2, 3, 4, 2, 0, 1, 2, 6, 4, 2, 0, 1, 8, 6, 4, 2, 0), byrow = TRUE, nrow = 5)
PenaltyMetrix
as.matrix(table(ClaimsTest$bucket2009, ClaimsTest$bucket2008)) * PenaltyMetrix
sum(as.matrix(table(ClaimsTest$bucket2009, ClaimsTest$bucket2008)) * PenaltyMetrix) / nrow(ClaimsTest)

library(rpart)
library(rpart.plot)
ClaimsTree = rpart(bucket2009 ~age + arthritis + alzheimers + cancer + copd + depression + diabetes + heart.failure + ihd + kidney + osteoporosis + stroke + bucket2008 + reimbursement2008, data = ClaimsTrain, method = "class", cp=0.00005)
prp(ClaimsTree)
PredictTest = predict(ClaimsTree, newdata = ClaimsTest, type = "class")
table(ClaimsTest$bucket2009, PredictTest)
sum(diag(table(ClaimsTest$bucket2009, PredictTest))) / nrow(ClaimsTest)
sum(as.matrix(table(ClaimsTest$bucket2009, PredictTest)) * PenaltyMetrix) / nrow(ClaimsTest)

ClaimsTree = rpart(bucket2009 ~age + arthritis + alzheimers + cancer + copd + depression + diabetes + heart.failure + ihd + kidney + osteoporosis + stroke + bucket2008 + reimbursement2008, data = ClaimsTrain, method = "class", cp=0.00005, parms = list(loss=PenaltyMetrix))
PredictTest = predict(ClaimsTree, newdata = ClaimsTest, type = "class")
table(ClaimsTest$bucket2009, PredictTest)
sum(diag(table(ClaimsTest$bucket2009, PredictTest))) / nrow(ClaimsTest)
sum(as.matrix(table(ClaimsTest$bucket2009, PredictTest)) * PenaltyMetrix) / nrow(ClaimsTest)






