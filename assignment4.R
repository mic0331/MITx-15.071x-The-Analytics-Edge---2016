setwd("/media/mic0331/Data/Data Analysis/Courses/Edx/MITx: 15.071x The Analytics Edge")
gerber <- read.csv('./dataset/gerber.csv')
table(gerber$voting)
sum(gerber$voting) / nrow(gerber)
tapply(gerber$voting, gerber$civicduty, mean)
tapply(gerber$voting, gerber$hawthorne, mean)
tapply(gerber$voting, gerber$self, mean)
tapply(gerber$voting, gerber$neighbors, mean)

logModel <- glm(voting ~ civicduty + hawthorne + self + neighbors, family="binomial", data = gerber)
summary(logModel)
logModel.predict <- predict(logModel, type="response")
t <- table(gerber$voting, logModel.predict >= .3)
N <- nrow(gerber)
TN <- t[1] 
FP <- t[3] 
FN <- t[2] 
TP <- t[4]
Acc <- (TN+TP)/N
Acc
library(ROCR)
ROCRpred <- prediction(logModel.predict, gerber$voting)
as.numeric(performance(ROCRpred, "auc")@y.values)

t <- table(gerber$voting, logModel.predict >= .5)
N <- nrow(gerber)
TN <- t[1] 
FP <- t[3] 
FN <- t[2] 
TP <- t[4]
Acc <- (TN)/N
Acc
ROCRpred <- prediction(logModel.predict, gerber$voting)
as.numeric(performance(ROCRpred, "auc")@y.values)

sum(gerber$voting)/N

CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber)
prp(CARTmodel)
CARTmodel2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)
prp(CARTmodel2)
CARTmodel3 = rpart(voting ~ civicduty + hawthorne + self + neighbors + sex, data=gerber, cp=0.0)
prp(CARTmodel3)
CARTmodel4 = rpart(voting ~ control, data=gerber, cp=0.0)
prp(CARTmodel4, digits = 6)
abs(0.296638-.34)
CARTmodel5 = rpart(voting ~ control + sex, data=gerber, cp=0.0)
prp(CARTmodel5, digits = 6)

LogModelSex <- glm(voting ~ sex + control, family="binomial", data = gerber)
summary(LogModelSex)

Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(LogModelSex, newdata=Possibilities, type="response")
abs(0.2908065-0.290456)

LogModel2 = glm(voting ~ sex + control + sex:control, data=gerber, family="binomial")
summary(LogModel2)
predict(LogModel2, newdata=Possibilities, type="response")
abs(0.2904558-0.290456)
# ---------------------------------------------------------------------------------------------------------------------------------------
letters <- read.csv('./dataset/letters_ABPR.csv')
letters$isB = as.factor(letters$letter == "B")
set.seed(1000)
split = sample.split(letters$isB, SplitRatio = .5)
train = subset(letters, split == TRUE)
test = subset(letters, split == FALSE)
sum(test$isB==FALSE)/nrow(test)

CARTb = rpart(isB ~ . - letter, data=train, method="class")
CARTb.predict <- predict(CARTb, type="class", newdata = test)
t <- table(test$isB, CARTb.predict == TRUE)
N <- nrow(test)
TN <- t[1] 
FP <- t[3] 
FN <- t[2] 
TP <- t[4]
Acc <- (TN+TP)/N
Acc

library(randomForest)
set.seed(1000)
lettersForest = randomForest(isB ~ . - letter, data = train)
lettersForest.predict <- predict(lettersForest, type="class", newdata = test)
t <- table(test$isB, lettersForest.predict == TRUE)
N <- nrow(test)
TN <- t[1] 
FP <- t[3] 
FN <- t[2] 
TP <- t[4]
Acc <- (TN+TP)/N
Acc

letters$letter = as.factor( letters$letter )
set.seed(2000)
split = sample.split(letters$letter, SplitRatio = .5)
train = subset(letters, split == TRUE)
test = subset(letters, split == FALSE)
table(test$letter)
sum(test$letter=="P")/nrow(test)
CARTletters = rpart(letter ~ . - isB, data=train, method="class")
CARTletters.predict <- predict(CARTletters, type="class", newdata = test)

t <- table(test$letter, CARTletters.predict)
N <- nrow(test)
Acc <- sum(diag(t))/N
Acc

set.seed(1000)
forestletters = randomForest(letter ~ . - isB, data=train)
forestletters.predict <- predict(forestletters, type="class", newdata = test)

t <- table(test$letter, forestletters.predict)
N <- nrow(test)
Acc <- sum(diag(t))/N
Acc
# --------------------------------------------------------------------------------------------------------------------------------------
census <- read.csv('./dataset/census.csv')
set.seed(2000)
split = sample.split(census$over50k, SplitRatio = .6)
train = subset(census, split == TRUE)
test = subset(census, split == FALSE)
LogModel = glm(over50k ~ ., data = train, family="binomial")
summary(LogModel)
LogModel.predict <- predict(LogModel, newdata = test)
t <- table(test$over50k, LogModel.predict >= .5)
N <- nrow(test)
TN <- t[1] 
FP <- t[3] 
FN <- t[2] 
TP <- t[4]
Acc <- (TN+TP)/N
Acc
Baseline <- (TN+FP)/N
Baseline
library(ROCR)
ROCRpred = prediction(LogModel.predict, test$over50k)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
ROCRauc <- as.numeric(performance(ROCRpred, "auc")@y.values)
ROCRauc

tree <- rpart(over50k ~ ., data = train, method = "class")
prp(tree)
tree.predict <- predict(tree, newdata = test, type = "class")
t <- table(test$over50k, tree.predict)
N <- nrow(test)
TN <- t[1] 
FP <- t[3] 
FN <- t[2] 
TP <- t[4]
Acc <- (TN+TP)/N
Acc

tree.predict <- predict(tree, newdata = test)
ROCRpred = prediction(tree.predict[,2], test$over50k)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
ROCRauc <- as.numeric(performance(ROCRpred, "auc")@y.values)
ROCRauc

set.seed(1)
trainSmall = train[sample(nrow(train), 2000), ]
set.seed(1)
forest = randomForest(over50k ~ ., data=trainSmall)
forest.predict <- predict(forest, newdata = test)
t <- table(test$over50k, forest.predict)
N <- nrow(test)
TN <- t[1] 
FP <- t[3] 
FN <- t[2] 
TP <- t[4]
Acc <- (TN+TP)/N
Acc
vu = varUsed(forest, count=TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(forest$forest$xlevels[vusorted$ix]))
varImpPlot(forest)

set.seed(2)
numFolds = trainControl(method = "cv", number = 10)
cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002))
train(over50k ~ ., data = train, method = "rpart", trControl=numFolds, tuneGrid=cartGrid)
tree <- rpart(over50k ~ ., data = train, method = "class", cp = .002)
tree.predict <- predict(tree, newdata = test, type = "class")
t <- table(test$over50k, tree.predict)
N <- nrow(test)
TN <- t[1] 
FP <- t[3] 
FN <- t[2] 
TP <- t[4]
Acc <- (TN+TP)/N
Acc
prp(tree)

# --------------------------------------------------------------------------------------------------------------------------------------

data(state)
statedata = data.frame(state.x77)

linreg <- lm(Life.Exp ~., data = statedata)
summary(linreg)

linreg.pred <- predict(linreg)
linreg.SSE <- sum((statedata$Life.Exp - linreg.pred)^2)
linreg.SSE

linreg2 <- lm(Life.Exp ~ Population + Murder + Frost + HS.Grad, data = statedata)
summary(linreg2)

linreg2.pred <- predict(linreg2)
linreg2.SSE <- sum((statedata$Life.Exp - linreg2.pred)^2)
linreg2.SSE

tree <- rpart(Life.Exp ~ ., data = statedata)
prp(tree)
tree.pred <- predict(tree)
tree.SSE <- sum((statedata$Life.Exp - tree.pred)^2)

tree2 <- rpart(Life.Exp ~ ., data = statedata, minbucket = 5)
prp(tree2)
tree2.pred <- predict(tree2)
tree2.SSE <- sum((statedata$Life.Exp - tree2.pred)^2)

tree3 <- rpart(Life.Exp ~ Area, data = statedata, minbucket = 1)
prp(tree3)
tree3.pred <- predict(tree3)
tree3.SSE <- sum((statedata$Life.Exp - tree3.pred)^2)

set.seed(111)
numFolds = trainControl(method = "cv", number = 10)
cpGrid = expand.grid(.cp=seq(0.01,0.5,0.01))
train(Life.Exp ~ ., data = statedata, method = "rpart", trControl=numFolds, tuneGrid=cpGrid)

tree4 <- rpart(Life.Exp ~ ., data = statedata, cp = .11)
prp(tree4)
tree4.pred <- predict(tree4)
tree4.SSE <- sum((statedata$Life.Exp - tree4.pred)^2)

set.seed(111)
numFolds = trainControl(method = "cv", number = 10)
cpGrid = expand.grid(.cp=seq(0.01,0.5,0.01))
train(Life.Exp ~ Area, data = statedata, method = "rpart", trControl=numFolds, tuneGrid=cpGrid)
tree5 <- rpart(Life.Exp ~ Area, data = statedata, cp = .06)
prp(tree5)
tree5.pred <- predict(tree5)
tree5.SSE <- sum((statedata$Life.Exp - tree5.pred)^2)
