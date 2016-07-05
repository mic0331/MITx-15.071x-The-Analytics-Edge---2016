setwd("/media/mic0331/Data/Data Analysis/Courses/Edx/MITx: 15.071x The Analytics Edge")
flower <- read.csv("./dataset/flower.csv", header=FALSE)
str(flower)
flowerMatrix = as.matrix(flower)
str(flowerMatrix)
flowerVector = as.vector(flowerMatrix)
str(flowerVector)
distance = dist(flowerVector, method="euclidian")
clusterIntensity = hclust(distance, method="ward.D")
plot(clusterIntensity)
rect.hclust(clusterIntensity, k=3, border="red")
flowerClusters = cutree(clusterIntensity, k=3)
flowerClusters
tapply(flowerVector, flowerClusters, mean)
dim(flowerClusters) = c(50,50)
image(flowerClusters, axes=FALSE)
image(flowerMatrix, axes=FALSE, col=grey(seq(0,1,length=256)))

healthy = read.csv("./dataset/healthy.csv", header = FALSE)
healthyMatrix = as.matrix(healthy)
str(healthyMatrix)
image(healthyMatrix, axes=FALSE, col = grey(seq(0,1,length=256)))
healthyVector = as.vector(healthyMatrix)
distance = dist(healthyVector, method = "euclidean")
str(healthyVector)
n=365636
n*(n-1)/2

k=5
set.seed(1)
KMC = kmeans(healthyVector, centers = k, iter.max = 1000)
str(KMC)
healthyclusters = KMC$cluster
KMC$centers[2]
dim(healthyclusters) = c(nrow(healthyMatrix), ncol(healthyMatrix))
image(healthyclusters, axes=FALSE, col=rainbow(k))

# scree plots to find the number of k-means cluster
KMC2 = kmeans(healthyVector, centers = 2, iter.max = 1000)
KMC3 = kmeans(healthyVector, centers = 3, iter.max = 1000)
KMC4 = kmeans(healthyVector, centers = 4, iter.max = 1000)
KMC5 = kmeans(healthyVector, centers = 5, iter.max = 1000)
KMC6 = kmeans(healthyVector, centers = 6, iter.max = 1000)
KMC7 = kmeans(healthyVector, centers = 7, iter.max = 1000)
KMC8 = kmeans(healthyVector, centers = 8, iter.max = 1000)
KMC9 = kmeans(healthyVector, centers = 9, iter.max = 1000)
KMC10 = kmeans(healthyVector, centers = 10, iter.max = 1000)

KMC2$withinss
NumClusters = seq(2,10,1)
SumWithinss = c(sum(KMC2$withinss), sum(KMC3$withinss), sum(KMC4$withinss), sum(KMC5$withinss), sum(KMC6$withinss), sum(KMC7$withinss), sum(KMC8$withinss), sum(KMC9$withinss), sum(KMC10$withinss))
# or !!!!
SumWithinss = sapply(2:10, function(x) sum(kmeans(healthyVector, centers=x, iter.max=1000)$withinss))
plot(NumClusters, SumWithinss, type="b")
#  Beyond 5, increasing the number of clusters does not really reduce the within-cluster sum of squares too much.

tumor = read.csv("./dataset/tumor.csv", header = FALSE)
tumorMetrix = as.matrix(tumor)
tumorVector = as.vector(tumorMetrix)
install.packages("flexclust")
library(flexclust)
KMC.kcca = as.kcca(KMC, healthyVector)
tumorClusters = predict(KMC.kcca, newdata = tumorVector)
dim(tumorClusters) = c(nrow(tumorMetrix), ncol(tumorMetrix))
image(tumorClusters, axes=FALSE, col=rainbow((k)))





