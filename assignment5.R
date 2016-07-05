setwd("/media/mic0331/Data/Data Analysis/Courses/Edx/MITx: 15.071x The Analytics Edge")
wiki = read.csv("./dataset/wiki.csv", stringsAsFactors=FALSE)
wiki$Vandal = as.factor(wiki$Vandal)
table(wiki$Vandal)

library(tm)
corpusAdded = Corpus(VectorSource(wiki$Added))
corpusAdded = tm_map(corpusAdded, removeWords, stopwords("english"))
corpusAdded = tm_map(corpusAdded, stemDocument)
dtmAdded = DocumentTermMatrix(corpusAdded)
length(stopwords("english")) 
dtmAdded
dtmAdded = removeSparseTerms(dtmAdded, .997)
dtmAdded
wordsAdded = as.data.frame(as.matrix(dtmAdded))
colnames(wordsAdded) = paste("A", colnames(wordsAdded))

corpusRemoved = Corpus(VectorSource(wiki$Removed))
corpusRemoved = tm_map(corpusRemoved, removeWords, stopwords("english"))
corpusRemoved = tm_map(corpusRemoved, stemDocument)
dtmRemoved = DocumentTermMatrix(corpusRemoved)
dtmRemoved = removeSparseTerms(dtmRemoved, .997)
length(stopwords("english")) 
dtmRemoved
wordsRemoved = as.data.frame(as.matrix(dtmRemoved))
colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))
ncol(wordsRemoved) 

wikiWords = cbind(wordsAdded, wordsRemoved)
str(wikiWords)
wikiWords$Vendal = wiki$Vandal
set.seed(123)
library(caTools)
spl = sample.split(wikiWords$Vendal, SplitRatio=.7)
Train = subset(wikiWords, spl == TRUE)
Test = subset(wikiWords, spl == FALSE)
table(Test$Vendal)
618/nrow(Test)

library(rpart)
library(rpart.plot)
wikiCART = rpart(Vendal ~., data = Train, method = "class")
pred = predict(wikiCART, newdata = Test)
pred.prob = pred[,2]
table(Test$Vendal, pred.prob >= .5)
(618+12)/nrow(Test)

prp(wikiCART)

grepl("cat","dogs and cats",fixed=TRUE) # TRUE
grepl("cat","dogs and rats",fixed=TRUE) # FALSE

wikiWords2 = wikiWords
wikiWords2$HTTP = ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)
table(wikiWords2$HTTP)

wikiTrain2 = subset(wikiWords2, spl==TRUE)
wikiTest2 = subset(wikiWords2, spl==FALSE)

wikiCART2 = rpart(Vendal ~., data = wikiTrain2, method = "class")
pred2 = predict(wikiCART2, newdata = wikiTest2, type="class")
table(Test$Vendal, pred2)
(609+57)/nrow(wikiTest2)
wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))
mean(wikiWords2$NumWordsAdded)

wikiTrain3 = subset(wikiWords2, spl==TRUE)
wikiTest3 = subset(wikiWords2, spl==FALSE)
wikiCART3 = rpart(Vendal ~., data = wikiTrain3, method = "class")
pred3 = predict(wikiCART3, newdata = wikiTest3, type="class")
table(wikiTest3$Vendal, pred3)
(550+143)/nrow(wikiTest3)

wikiWords3 = wikiWords2
wikiWords3$Minor = wiki$Minor
wikiWords3$Loggedin = wiki$Loggedin

wikiTrain4 = subset(wikiWords3, spl==TRUE)
wikiTest4 = subset(wikiWords3, spl==FALSE)
wikiCART4 = rpart(Vendal ~., data = wikiTrain4, method = "class")
pred4 = predict(wikiCART4, newdata = wikiTest4, type="class")
table(wikiTest4$Vendal, pred4)
(539+292)/nrow(wikiTest4)

prp(wikiCART3)

# ---------------------------------------------------------------------------------------------------------------------------------------

trials = read.csv("./dataset/clinical_trial.csv", stringsAsFactors=FALSE)
max(nchar(trials$abstract))
table(nchar(trials$abstract) == 0)
sum(nchar(trials$abstract) == 0)
trials[which.min(nchar(trials$title)),]$title
library(tm)
corpusTitle = Corpus(VectorSource(trials$title))
corpusAbstract = Corpus(VectorSource(trials$abstract))

corpusTitle = tm_map(corpusTitle, tolower)
corpusAbstract = tm_map(corpusAbstract, tolower)

corpusTitle = tm_map(corpusTitle, PlainTextDocument)
corpusAbstract = tm_map(corpusAbstract, PlainTextDocument)

corpusTitle = tm_map(corpusTitle, removePunctuation)
corpusAbstract = tm_map(corpusAbstract, removePunctuation)

corpusTitle = tm_map(corpusTitle, removeWords, stopwords("english"))
corpusAbstract = tm_map(corpusAbstract, removeWords, stopwords("english"))

corpusTitle = tm_map(corpusTitle, stemDocument)
corpusAbstract = tm_map(corpusAbstract, stemDocument)

dtmTitle = DocumentTermMatrix(corpusTitle)
dtmAbstract = DocumentTermMatrix(corpusAbstract)

dtmTitle = removeSparseTerms(dtmTitle, 0.95)
dtmAbstract = removeSparseTerms(dtmAbstract, 0.95)

dtmTitle = as.data.frame(as.matrix(dtmTitle))
dtmAbstract = as.data.frame(as.matrix(dtmAbstract))

sort(colSums(dtmAbstract))


colnames(dtmTitle) = paste0("T", colnames(dtmTitle))
colnames(dtmAbstract) = paste0("A", colnames(dtmAbstract))

dtm = cbind(dtmTitle, dtmAbstract)
dtm$trial <- trials$trial
str(dtm)
set.seed(144)
library(caTools)
split = sample.split(dtm$trial, SplitRatio = .7)
train = subset(dtm, split == TRUE)
test = subset(dtm, split == FALSE)
table(train$trial)
730/nrow(train)

library(rpart)
library(rpart.plot)
trialCART = rpart(trial~., data=train, method="class")
prp(trialCART)

predTrain = predict(trialCART, data=train)
max(predTrain[,2])

predTrain.prob = predTrain[,2]
table(train$trial, predTrain.prob >= 0.5)
# What is the training set accuracy of the CART model?
(631+441)/(631+99+131+441)
# What is the training set sensitivity of the CART model?
441/(131+441)
# What is the training set specificity of the CART model?
631/(631+99)

predTest = predict(trialCART, newdata=test)
predTest.prob = predTest[,2]
table(test$trial, predTest.prob >= 0.5)
(261+162)/(261+52+83+162)

library(ROCR)
predROCR = prediction(predTest.prob, test$trial)
perfROCR = performance(predROCR, "tpr", "fpr")
plot(perfROCR, colorize=TRUE)
performance(predROCR, "auc")@y.values

# --------------------------------------------------------------------------------------------------------------------------------------

emails = read.csv("./dataset/emails.csv", stringsAsFactors=FALSE)
table(emails$spam)
summary(nchar(emails$text))
which.min(nchar(emails$text))

library(tm)
corpusText = Corpus(VectorSource(emails$text))
corpusText = tm_map(corpusText, tolower)
corpusText = tm_map(corpusText, PlainTextDocument)
corpusText = tm_map(corpusText, removePunctuation)
corpusText = tm_map(corpusText, removeWords, stopwords("english"))
corpusText = tm_map(corpusText, stemDocument)

dtmText = DocumentTermMatrix(corpusText)
dtmText = as.data.frame(as.matrix(dtmText))
str(dtmText)

spdtm = DocumentTermMatrix(corpusText)
spdtm = removeSparseTerms(spdtm, 0.95)
spdtm = as.data.frame(as.matrix(spdtm))
str(spdtm)

emailsSparse = spdtm
colnames(emailsSparse) = make.names(colnames(emailsSparse))
sort(colSums(emailsSparse))

emailsSparse$spam = emails$spam
table(colSums(emailsSparse[emailsSparse$spam==0,names(emailsSparse) !="spam"]) >= 5000)

table(colSums(emailsSparse[emailsSparse$spam==1,names(emailsSparse) !="spam"]) >= 1000)


emailsSparse$spam = as.factor(emailsSparse$spam)
library(caTools)
set.seed(123)
spl = sample.split(emailsSparse$spam, 0.7)
train = subset(emailsSparse, spl == TRUE)
test = subset(emailsSparse, spl == FALSE)

spamLog = glm(spam ~ ., data=train, family="binomial")
trainLogPred = predict(spamLog, data=train, type="response")

# How many of the training set predicted probabilities from spamLog are less than 0.00001?
table(trainLogPred < 0.00001)
# How many of the training set predicted probabilities from spamLog are more than 0.99999?
table(trainLogPred > 0.99999)
# How many of the training set predicted probabilities from spamLog are between 0.00001 and 0.99999?
table(trainLogPred >= 0.00001 & trainLogPred <= 0.99999)

summary(spamLog)

library(rpart)
library(rpart.plot)
spamCART = rpart(spam~., data=train, method="class")
prp(spamCART)

table(train$spam, trainLogPred > 0.5)
(3052+954)/(3052+954+4)

library(ROCR)
LogROCRpred = prediction(trainLogPred, train$spam)
as.numeric(performance(LogROCRpred, "auc")@y.values)

trainCARTpred = predict(spamCART, data=train)
trainCARTpred.prob = trainCARTpred[,2]
table(train$spam, trainCARTpred.prob >= 0.5)
(2885+894)/nrow(train)

trainpredROCR = prediction(trainCARTpred.prob, train$spam)
performance(trainpredROCR, "auc")@y.values

library(randomForest)
set.seed(123)
spamRF = randomForest(spam~., data=train)
trainRFPred = predict(spamRF, data=train, type="prob")[,2]
table(train$spam, trainRFPred>0.5)
(3013+914)/(3013+39+44+914)

trainRFROCR = prediction(trainRFPred, train$spam)
performance(trainRFROCR, "auc")@y.values

testLogPred = predict(spamLog, newdata=test, type="response")
table(test$spam, testLogPred > 0.5)
(1257+376)/nrow(test)

LogROCRpred_test = prediction(testLogPred, test$spam)
as.numeric(performance(LogROCRpred_test, "auc")@y.values)

testCARTpred = predict(spamCART, newdata=test)[,2]
table(test$spam, testCARTpred >= 0.5)
(1228+386)/nrow(test)

testpredROCR = prediction(testCARTpred, test$spam)
performance(testpredROCR, "auc")@y.values

testRFPred = predict(spamRF, newdata=test, type="prob")[,2]
table(test$spam, testRFPred>0.5)
(1290+385)/nrow(test)

testRFROCR = prediction(testRFPred, test$spam)
performance(testRFROCR, "auc")@y.values

wordCount = rowSums(as.matrix(dtm))
