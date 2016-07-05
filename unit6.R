setwd("/media/mic0331/Data/Data Analysis/Courses/Edx/MITx: 15.071x The Analytics Edge")
movies = read.table("./dataset/movieLens.txt", header=FALSE, sep='|', quote="\"")
str(movies)
colnames(movies) = c("ID", "Title", "ReleaseDate", "VideoReleaseDate", "IMDB", "Unkown", "Action", "Advanture", "Animation", "Childrens", "Comedy", "Crime", "Documentary", "Drama", "Fantasy", "FilmNoir", "Horror", "Musical", "Mystery", "Romance", "SciFi", "Thriller", "War", "Western")
movies$ID = NULL
movies$ReleaseDate = NULL
movies$VideoReleaseDate = NULL
movies$IMDB = NULL
movies = unique(movies)
str(movies)
table(movies$Comedy)
table(movies$Western)
table(movies$Romance & movies$Drama)

distances = dist(movies[2:20], method = "euclidean")
clusterMovies = hclust(distances, method="ward.D")
plot(clusterMovies)
clusterGroups = cutree(clusterMovies, k = 10)
tapply(movies$Action, clusterGroups, mean)
tapply(movies$Romance, clusterGroups, mean)
subset(movies, Title=="Men in Black (1997)")
clusterGroups[257]
cluster2 = subset(movies, clusterGroups == 2)
cluster2$Title[1:10]

colMeans(subset(movies[2:20], clusterGroups == 1))
spl = split(movies[2:20], clusterGroups)
spl[[1]]
subset(movies[2:20], clusterGroups == 1)
lapply(spl, colMeans)

clusterGroups = cutree(clusterMovies, k = 2)
colMeans(subset(movies[2:20], clusterGroups == 2))


