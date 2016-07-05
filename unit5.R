setwd("/media/mic0331/Data/Data Analysis/Courses/Edx/MITx: 15.071x The Analytics Edge")
tweets = read.csv("./dataset/tweets.csv", stringsAsFactors=FALSE)
Sys.setlocale("LC_ALL", "C")
str(tweets)
tweets$Negative = as.factor(tweets$Avg <= -1)
table(tweets$Negative)
install.packages("tm")
library(tm)
install.packages("SnowballC")
library(SnowballC)
# create a corpus
corpus = Corpus(VectorSource(tweets$Tweet))
corpus
corpus[[1]]
# make all words in lower case
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus[[1]]$content
# remove punctuation
corpus = tm_map(corpus, removePunctuation)
corpus[[1]]$content
stopwords("english")[1:10]
length(stopwords("english"))
# remove stop words
corpus = tm_map(corpus, removeWords, c("apple", stopwords(("english"))))
corpus[[1]]$content
# steming
corpus = tm_map(corpus, stemDocument)
corpus[[1]]$content
frequencies = DocumentTermMatrix(corpus)
frequencies
# look at document 1000 -> 1005 and words 505 to 515
inspect(frequencies[1000:1005, 505:515])
# find the frequencies with minimum 20 times
findFreqTerms(frequencies, lowfreq = 20)
# 98% only keeps terms that appear at least 2% of the time in all tweets
sparse = removeSparseTerms(frequencies, .995)
sparse
tweetsSparse = as.data.frame(as.matrix(sparse))
colnames(tweetsSparse) = make.names(colnames(tweetsSparse))
tweetsSparse$Negative = tweets$Negative

library(caTools)
set.seed(123)
split = sample.split(tweetsSparse$Negative, SplitRatio = .7)
train = subset(tweetsSparse, split == TRUE)
test = subset(tweetsSparse, split == FALSE)

library(rpart)
library(rpart.plot)
tweetCART <- rpart(Negative ~ ., data = train, method = "class")
prp(tweetCART)
predictCART = predict(tweetCART, newdata = test, type = "class")
table(test$Negative, predictCART)
(294+18)/(294+6+37+18)
table(test$Negative)
300/355

library(randomForest)
set.seed(123)
tweetRF = randomForest(Negative ~ ., data = train)
predictRF = predict(tweetRF, newdata = test)
table(test$Negative, predictRF)
(293+21)/nrow(test)

tweetLog = glm(Negative ~ ., data = train, family = "binomial")
predictions = predict(tweetLog, newdata=test, type="response")
table(test$Negative, predictions >= .5)
(252+31)/nrow(test)

















