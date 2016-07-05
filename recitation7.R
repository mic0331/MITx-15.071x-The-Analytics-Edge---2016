setwd("/media/mic0331/Data/Data Analysis/Courses/Edx/MITx: 15.071x The Analytics Edge")
library(ggplot2)
intl = read.csv("./dataset/intl.csv")
str(intl)
ggplot(intl, aes(x=Region, y=PercentOfIntl)) + geom_bar(stat="identity") + geom_text(aes(label=PercentOfIntl))
intl = transform(intl, Region = reorder(Region, -PercentOfIntl))
str(intl)
intl$PercentOfIntl = intl$PercentOfIntl * 100
ggplot(intl, aes(x=Region, y=PercentOfIntl)) +
  geom_bar(stat="identity", fill="dark blue") +
  geom_text(aes(label=PercentOfIntl), vjust=-.4) +
  ylab("Percent of Internation Students") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

library(ggmap)
intall = read.csv("./dataset/intlall.csv", stringsAsFactors = FALSE)
head(intall)
intall[is.na(intall)] = 0
head(intall)
world_map = map_data("world")
str(world_map)
world_map = merge(world_map, intall, by.x="region", by.y="Citizenship")
str(world_map)
ggplot(world_map, aes(x=long, y=lat, group=group)) +
  geom_polygon(fill="white", color="black") +
  coord_map("mercator")

world_map = world_map[order(world_map$group, world_map$order),]
ggplot(world_map, aes(x=long, y=lat, group=group)) +
  geom_polygon(fill="white", color="black") +
  coord_map("mercator")

table(intall$Citizenship)
intall$Citizenship[intall$Citizenship=="China (People's Republic Of)"] = 'China'

world_map = merge(map_data("world"), intall, by.x="region", by.y="Citizenship")
world_map = world_map[order(world_map$group, world_map$order),]
world_map = world_map[order(world_map$group, world_map$order),]
ggplot(world_map, aes(x=long, y=lat, group=group)) +
  geom_polygon(aes(fill=Total), color="black") +
  coord_map("mercator", xlim=c(-180,180), ylim=c(-60, 90))


ggplot(world_map, aes(x=long, y=lat, group=group)) +
  geom_polygon(aes(fill=Total), color="black") +
  coord_map("ortho", orientation = c(20,30,0), xlim=c(-180,180), ylim=c(-60, 90))


install.packages("reshape2")
library(reshape2)

households = read.csv("./dataset/households.csv")
str(households)
households[,1:2]
head(melt(households, id="Year"))
ggplot(melt(households, id="Year"), aes(x=Year, y=value, color=variable)) +
  geom_line(size=2) +
  geom_point(size=5) +
  ylab("Percentage of Houholds")


