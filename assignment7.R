setwd("/media/mic0331/Data/Data Analysis/Courses/Edx/MITx: 15.071x The Analytics Edge")
library(ggplot2)
library(maps)
library(ggmap)

statesMap = map_data("state")
str(statesMap)
table(statesMap$group)
ggplot(statesMap, aes(x = long, y = lat, group = group)) + geom_polygon(fill = "white", color = "black")

polling = read.csv("./dataset/PollingImputed.csv")
Train = subset(polling, Year >= 2004 & Year <= 2008)
Test = subset(polling, Year == 2012)
mod2 = glm(Republican~SurveyUSA+DiffCount, data=Train, family="binomial")
TestPrediction = predict(mod2, newdata=Test, type="response")
TestPredictionBinary = as.numeric(TestPrediction > 0.5)
predictionDataFrame = data.frame(TestPrediction, TestPredictionBinary, Test$State)
table(predictionDataFrame$TestPredictionBinary)
mean(TestPrediction)

predictionDataFrame$region = tolower(predictionDataFrame$Test.State)
predictionMap = merge(statesMap, predictionDataFrame, by = "region")
predictionMap = predictionMap[order(predictionMap$order),]
ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) + geom_polygon(color = "black")

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) + 
  geom_polygon(color = "black") + 
  scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPrediction)) + 
  geom_polygon(color = "black") + 
  scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")

subset(predictionMap, predictionMap$Test.State == 'Florida')$TestPrediction

?geom_polygon

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) + 
  geom_polygon(color = "black", linetype = 3) + 
  scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) + 
  geom_polygon(color = "black", size = 3) + 
  scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")

ggplot(predictionMap, aes(x = long, y = lat, group = group, fill = TestPredictionBinary)) + 
  geom_polygon(color = "black", alpha = .3) + 
  scale_fill_gradient(low = "blue", high = "red", guide = "legend", breaks= c(0,1), labels = c("Democrat", "Republican"), name = "Prediction 2012")

# --------------------------------------------------------------------------------------------------------------------------------------

edges = read.csv("./dataset/edges.csv")
users = read.csv("./dataset/users.csv")
str(edges)
str(users)
(length(edges$V1) + length(edges$V2))/length(users$id)
table(users$locale, users$school)
table(users$school, users$gender)
install.packages("igraph")
library(igraph)
?graph.data.frame
g = graph.data.frame(edges, FALSE, users)
plot(g, vertex.size=5, vertex.label=NA)
degree(g)
x = degree(g)
sum(x >= 10)

V(g)$size = degree(g)/2+2
plot(g, vertex.label=NA)

max(V(g)$size)
min(V(g)$size)

V(g)$color = "black"
V(g)$color[V(g)$gender == "A"] = "red"
V(g)$color[V(g)$gender == "B"] = "gray"
plot(g, vertex.label = NA)

V(g)$color = "black"
V(g)$color[V(g)$school == "A"] = "red"
V(g)$color[V(g)$school == "AB"] = "green"
plot(g, vertex.label = NA)

V(g)$color = "black"
V(g)$color[V(g)$locale == "A"] = "red"
V(g)$color[V(g)$locale == "B"] = "green"
plot(g, vertex.label = NA)

?igraph.plotting
install.packages("rgl")
rglplot(g, vertex.label=NA)

# --------------------------------------------------------------------------------------------------------------------------------------

setwd("/media/mic0331/Data/Data Analysis/Courses/Edx/MITx: 15.071x The Analytics Edge")
tweets = read.csv("./dataset/tweets.csv", stringsAsFactors=FALSE)
library(tm)
library(SnowballC)
corpus = Corpus(VectorSource(tweets$Tweet))
corpus = tm_map(corpus, tolower)
corpus = tm_map(corpus, PlainTextDocument)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, c("apple", stopwords(("english"))))
frequencies = DocumentTermMatrix(corpus)
dtm <- DocumentTermMatrix(corpus)
allTweets <- as.data.frame(as.matrix(dtm))
colnames(allTweets) <- make.names(colnames(allTweets))
ncol(allTweets)
install.packages("wordcloud")
library(wordcloud)
?wordcloud
colnames(allTweets)
colSums(allTweets)
wordcloud(colnames(allTweets), colSums(allTweets))
wordcloud(colnames(allTweets),colSums(allTweets),scale=c(2,0.25),colors=brewer.pal(8, "Dark2"))

# Which word cloud is based only on the negative tweets (tweets with Avg value -1 or less)?
allTweets$Avg <- tweets$Avg
# Avg that is negative implies that the data contains negative words 
wordcloud(colnames(allTweets[allTweets$Avg<0,1:3779]),colSums(allTweets),scale=c(2,0.25),colors=brewer.pal(9, "YlOrRd"))

# Only one word cloud was created without modifying parameters min.freq or max.words. Which word cloud is this?
# Count the amount of words each plot has. Parameters min.freg and/or max.words limit the amount of words
# Which word clouds were created with parameter random.order set to FALSE?
wordcloud(colnames(allTweets),colSums(allTweets),scale=c(2,0.25),colors=brewer.pal(9, "YlOrRd")[c(-1,-2,-3,-4)],random.order=F)

install.packages("RColorBrewer")
library(RColorBrewer)
# Which color palette would be most appropriate for use in a word cloud for which we want to use color
# to indicate word frequency?
display.brewer.all()

# --------------------------------------------------------------------------------------------------------------------------------------



