# KAGGLE COMPETITION - GETTING STARTED

# This script file is intended to help you get started on the Kaggle platform, and to show you how to make a submission to the competition.


# Let's start by reading the data into R
# Make sure you have downloaded these files from the Kaggle website, and have navigated to the directory where you saved the files on your computer
setwd("/media/mic0331/Data/Data Analysis/Courses/Edx/MITx: 15.071x The Analytics Edge/kaggle competition")
train = read.csv("train2016.csv")#,na.strings="")
test = read.csv("test2016.csv")#, na.strings="")
summary(train)
train$YOB[train$YOB > 2014] <- mean(train$YOB)

# Let's look at how much data is missing
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(train,2,pMiss) # columns
apply(train,1,pMiss) # rows
# % or rows with less than 5% missing data
sum(apply(train,1,pMiss) < 5)/nrow(train)
sum(apply(test,1,pMiss) < 5)/nrow(test)
library(mice)
md.pattern(train)

# We will just create a simple logistic regression model, to predict Party using all other variables in the dataset, except for the user ID:
SimpleMod = glm(Party ~ . -USER_ID, data=train, family=binomial)

# And then make predictions on the test set:

PredTest = predict(SimpleMod, newdata=test, type="response")
threshold = 0.5
PredTestLabels = as.factor(ifelse(PredTest<threshold, "Democrat", "Republican"))

# However, you can submit the file on Kaggle to see how well the model performs. You can make up to 5 submissions per day, so don't hesitate to just upload a solution to see how you did.
# Let's prepare a submission file for Kaggle (for more about this, see the "Evaluation" page on the competition site):

MySubmission = data.frame(USER_ID = test$USER_ID, Predictions = PredTestLabels)
write.csv(MySubmission, "SubmissionSimpleLog.csv", row.names=FALSE, quote = FALSE)

# You should upload the submission "SubmissionSimpleLog.csv" on the Kaggle website to use this as a submission to the competition
# This model was just designed to help you get started - to do well in the competition, you will need to build better models!

# ---------------------------------------------------------------------------------------------------------------------------------------
# Data preparation
str(train)
summary(train)

# Do we have any NA's?
apply(train,2,function(x) sum(is.na(x)))

# imputation
library(mice)
simple = train[c("YOB", "Gender", "Income", "HouseholdStatus", "EducationLevel")]
imputed = complete(mice(simple))
train$YOB = imputed$YOB
train$Gender = imputed$Gender
train$Income = imputed$Income
train$HouseholdStatus = imputed$HouseholdStatus
train$EducationLevel = imputed$EducationLevel

simple = test[c("YOB", "Gender", "Income", "HouseholdStatus", "EducationLevel")]
imputed = complete(mice(simple))
test$YOB = imputed$YOB
test$Gender = imputed$Gender
test$Income = imputed$Income
test$HouseholdStatus = imputed$HouseholdStatus
test$EducationLevel = imputed$EducationLevel

# remove row where a number or NA is higher than a threshold
train <- train[rowSums(is.na(train)) < ncol(train)/1.1, ] # 10%

#impute missing values of questions
# TRAIN >>
# http://fastml.com/impute-missing-values-with-amelia/
q <- train[, grepl('Q', names(train))]
#install.packages("Amelia")
library(Amelia)
ords = c(colnames(q))
ncpus = 8
m = ncpus * 10
a.out = amelia( q,ords = ords,  parallel = 'multicore', ncpus = ncpus)
str(a.out$imputations)
x <- as.data.frame(a.out$imputations)
write.amelia( a.out, file.stem = 'train_imp', quote = FALSE )
q <- read.csv("train_imp1.csv")
q$X <- NULL
train[, grepl('Q', names(train))] <- q

# TEST >>
# http://fastml.com/impute-missing-values-with-amelia/
q <- test[, grepl('Q', names(test))]
#install.packages("Amelia")
library(Amelia)
ords = c(colnames(q))
ncpus = 8
m = ncpus * 10
a.out = amelia( q,ords = ords,  parallel = 'multicore', ncpus = ncpus)
str(a.out$imputations)
x <- as.data.frame(a.out$imputations)
write.amelia( a.out, file.stem = 'test_imp', quote = FALSE )
q <- read.csv("test_imp1.csv")
q$X <- NULL
test[, grepl('Q', names(test))] <- q

# ---------------------------------------------------------------------------------------------------------------------------------------
# Random Forest
library(randomForest)

set.seed(144)
RandomForestMod = randomForest(Party ~ ., data = train, ntree=200, nodesize=25)
RandomForestMod.pred = predict(RandomForestMod, newdata=test)
MySubmission = data.frame(USER_ID = test$USER_ID, Predictions = RandomForestMod.pred)
write.csv(MySubmission, "./submission/RandomForestMod.csv", row.names=FALSE, quote = FALSE)

# ---------------------------------------------------------------------------------------------------------------------------------------
# Adaboost v1
install.packages("adabag")
library(adabag)

adaboostMod <- boosting(Party ~ ., data = train, boos=TRUE, mfinal=20,coeflearn='Breiman')
adaboostMod.pred = predict(adaboostMod, newdata=test)
MySubmission = data.frame(USER_ID = test$USER_ID, Predictions = adaboostMod.pred$class)
write.csv(MySubmission, "./submission/adaboostMod_1.csv", row.names=FALSE, quote = FALSE)
# ---------------------------------------------------------------------------------------------------------------------------------------
# Adaboost v2
install.packages("adabag")
library(adabag)

adaboostMod <- boosting(Party ~ ., data = train, boos=TRUE, mfinal=500,coeflearn='Freund', control=rpart.control(minsplit=30, cp=0.001))
adaboostMod.pred = predict(adaboostMod, newdata=test)
MySubmission = data.frame(USER_ID = test$USER_ID, Predictions = adaboostMod.pred$class)
write.csv(MySubmission, "./submission/adaboostMod_2.csv", row.names=FALSE, quote = FALSE)
# ---------------------------------------------------------------------------------------------------------------------------------------
# Adaboost v3 (best)
set.seed(123)
adaboostMod <- boosting(Party ~ ., data = train, boos=TRUE, mfinal=100,coeflearn='Freund', control=rpart.control(maxdepth=8))
adaboostMod.pred = predict(adaboostMod, newdata=test)
MySubmission = data.frame(USER_ID = test$USER_ID, Predictions = adaboostMod.pred$class)
write.csv(MySubmission, "./submission/adaboostMod_8.csv", row.names=FALSE, quote = FALSE)
# ---------------------------------------------------------------------------------------------------------------------------------------
# Adaboost v4 
set.seed(144)
adaboostMod <- boosting(Party ~ ., data = train, boos=TRUE, mfinal=10,coeflearn='Zhu', control=rpart.control(maxdepth=5))
adaboostMod.pred = predict(adaboostMod, newdata=test)
MySubmission = data.frame(USER_ID = test$USER_ID, Predictions = adaboostMod.pred$class)
write.csv(MySubmission, "./submission/adaboostMod_4.csv", row.names=FALSE, quote = FALSE)
# ---------------------------------------------------------------------------------------------------------------------------------------
# Adaboost v5
library(caTools)
set.seed(144)
spl = sample.split(train$Party, SplitRatio = 0.75)
train_ = subset(train, spl==TRUE)
test_ = subset(train, spl==FALSE)
set.seed(144)
adaboostMod <- boosting(Party ~ ., data = train_, boos=TRUE, mfinal=9,coeflearn='Zhu', control=rpart.control(maxdepth=6))
adaboostMod.pred = predict(adaboostMod, newdata=test_)

t <- table(test_$Party, adaboostMod.pred$class)
N <- nrow(test_)
TN <- t[1] 
FP <- t[3] 
FN <- t[2] 
TP <- t[4]
Acc7 <- (TN+TP)/N
Acc7

adaboostMod <- boosting(Party ~ ., data = train, boos=TRUE, mfinal=9,coeflearn='Zhu', control=rpart.control(maxdepth=6))
adaboostMod.pred = predict(adaboostMod, newdata=test)
MySubmission = data.frame(USER_ID = test$USER_ID, Predictions = adaboostMod.pred$class)
write.csv(MySubmission, "./submission/adaboostMod_5.csv", row.names=FALSE, quote = FALSE)
# ---------------------------------------------------------------------------------------------------------------------------------------
# CV tree (to submit)
set.seed(144)
spl = sample.split(train$Party, SplitRatio = 0.75)
train_ = subset(train, spl==TRUE)
test_ = subset(train, spl==FALSE)
library(caret)
library(e1071)
numFolds = trainControl( method = "cv", number = 10 )
cpGrid = expand.grid( .cp = seq(0.01,0.5,0.01)) 
train(Party ~ ., data = train_, method = "rpart", trControl = numFolds, tuneGrid = cpGrid )
treeCVMod = rpart(Party ~ ., data = train_, method="class", cp = 0.05)
treeCVMod.pred = predict(treeCVMod, newdata = test_, type = "class")
t <- table(test_$Party, treeCVMod.pred)
N <- nrow(test_)
TN <- t[1] 
FP <- t[3] 
FN <- t[2] 
TP <- t[4]
Acc8 <- (TN+TP)/N
Acc8
library(rpart.plot)
prp(treeCVMod)
# ---------------------------------------------------------------------------------------------------------------------------------------
install.packages("neuralnet")
require(neuralnet)
?neuralnet

#train
train_ = data.matrix(train)
maxs <- apply(train_, 2, max) 
mins <- apply(train_, 2, min)
scaled_train <- as.data.frame(scale(train_, center = mins, scale = maxs - mins))

#test

test_ = data.matrix(test)
test_[is.na(test_)] <- 0
maxs <- apply(test_, 2, max) 
mins <- apply(test_, 2, min)
scaled_test <- as.data.frame(scale(test_, center = mins, scale = maxs - mins))

n <- names(scaled_train)
f <- as.formula(paste("Party ~", paste(n[!n %in% "Party"], collapse = " + ")))
nn = neuralnet(f, data = scaled_train, hidden = 2, linear.output = FALSE)
# https://www.youtube.com/watch?v=lTMqXSSjCvk
plot(nn)
nn.predict <- compute(nn, scaled_test)

str(nn.predict)

threshold = .5
nn.PredTestLabels = as.factor(ifelse(nn.predict$net.result<threshold, "Democrat", "Republican"))
MySubmission = data.frame(USER_ID = test$USER_ID, Predictions = nn.PredTestLabels)
write.csv(MySubmission, "./submission/neuralnet_2.csv", row.names=FALSE, quote = FALSE)

# ---------------------------------------------------------------------------------------------------------------------------------------
# Adaboost v6
set.seed(144)
adaboostMod <- boosting(Party ~ ., data = train, boos=TRUE, mfinal=10,coeflearn='Zhu', control=rpart.control(maxdepth=5))
adaboostMod.pred = predict(adaboostMod, newdata=test)
MySubmission = data.frame(USER_ID = test$USER_ID, Predictions = adaboostMod.pred$class)
write.csv(MySubmission, "./submission/adaboostMod_6.csv", row.names=FALSE, quote = FALSE)
# ----------------------------------------------------------------------------------------------------------------------------------------
train = read.csv("train2016.csv",na.strings="")
test = read.csv("test2016.csv", na.strings="")

train$YOB[train$YOB == 2039] <- NA
train$YOB <- as.integer(train$YOB)
test$YOB <- as.integer(test$YOB)
library(mice)
simple = train[c("YOB", "Gender", "Income", "HouseholdStatus", "EducationLevel")]
imputed = complete(mice(simple))
train$YOB = imputed$YOB
train$Gender = imputed$Gender
train$Income = imputed$Income
train$HouseholdStatus = imputed$HouseholdStatus
train$EducationLevel = imputed$EducationLevel

library(mice)
simple = test[c("YOB", "Gender", "Income", "HouseholdStatus", "EducationLevel")]
imputed = complete(mice(simple))
test$YOB = imputed$YOB
test$Gender = imputed$Gender
test$Income = imputed$Income
test$HouseholdStatus = imputed$HouseholdStatus
test$EducationLevel = imputed$EducationLevel

install.packages('xgboost')
require(xgboost)
library(Matrix)

#model.matrix( ~ Species - 1, data=iris )
train$Party <- ifelse(train$Party == "Republican", 1, 0)
train_ = model.matrix( ~ . - 1, data=train)
test_ = model.matrix( ~ . - 1, data=test)

xgboostMod <- xgboost(data = train_, label = train$Party, max.depth = 2, eta = 1, nround = 2, nthread = 2, objective = "binary:logistic")
xgboostMod.pred <- predict(xgboostMod, test_)
threshold = .5
xgboostMod.PredTestLabels = as.factor(ifelse(xgboostMod.pred<threshold, "Democrat", "Republican"))
MySubmission = data.frame(USER_ID = test$USER_ID, Predictions = xgboostMod.PredTestLabels)
write.csv(MySubmission, "./submission/xgboost_2.csv", row.names=FALSE, quote = FALSE)




summary(train)
questions = read.csv('Questions.csv', sep=";")
questions$Question.ID <- sub("^", "Q", questions$Question.ID)
