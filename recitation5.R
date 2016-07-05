setwd("/media/mic0331/Data/Data Analysis/Courses/Edx/MITx: 15.071x The Analytics Edge")
emails = read.csv("./dataset/energy_bids.csv", stringsAsFactors=FALSE)
str(emails)
emails$email[1]
strwrap(emails$email[1])
emails$responsive[1]
strwrap(emails$email[2])
emails$responsive[2]
table(emails$responsive)
library(tm)
corpus = Corpus(VectorSource(emails$email))
strwrap(corpus[[1]]$content)
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords("english"))
corpus = tm_map(corpus, stemDocument)
strwrap(corpus[[1]]$content)

dtm = DocumentTermMatrix(corpus)
dtm
dtm = removeSparseTerms(dtm, .97)
dtm
labeledTerms = as.data.frame(as.matrix(dtm))
labeledTerms$responsive = emails$responsive
str(labeledTerms)

library(caTools)
set.seed(144)
spl = sample.split(labeledTerms$responsive, .7)
train = subset(labeledTerms, spl == TRUE)
test = subset(labeledTerms, spl == FALSE)

library(rpart)
library(rpart.plot)
emailCART = rpart(responsive ~., data = train, method = "class")
prp(emailCART)
pred = predict(emailCART, newdata = test)
pred[1:10,]
pred.prob = pred[,2]
table(test$responsive, pred.prob >= .5)
(195+25)/nrow(test)
#baseline
table(test$responsive)
215/nrow(test)

library(ROCR)
predROCR = prediction(pred.prob, test$responsive)
perfROCR = performance(predROCR, "tpr", 'fpr')
plot(perfROCR, colorize=TRUE)
performance(predROCR, "auc")@y.values






