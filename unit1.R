sd(c(5,8,12))
which.min(c(4,1,6))

Sys.setlocale("LC_ALL", "C")
abs(-65)
?sqrt
SquareRoot2 = sqrt(2)
HoursYear <- 365*24
ls()

# vectors
c(2, 3, 5, 8, 13)
Country = c("Brazil", "China", "India", "Switzerland", "USA")
LifeExpectancy = c(74,76,65,83,79)
Country[1]
LifeExpectancy[3]
seq(0,100,2)
# dataframe
CountryData = data.frame(Country, LifeExpectancy)
CountryData
CountryData$Population=c(199000,1390000,1240000,7997,318000)
CountryData
Country = c("Australia", "Greece")
LifeExpectancy = c(82,81)
Population = c(23050,11125)
NewcountryData = data.frame(Country, LifeExpectancy, Population)
NewcountryData
AllcountryData = rbind(CountryData, NewcountryData)
AllcountryData
# Loading data files
getwd()
setwd("/media/mic0331/Data/Data Analysis/Courses/Coursera/MITx: 15.071x The Analytics Edge")
WHO = read.csv("./dataset/WHO.csv")
str(WHO)
summary(WHO)
WHO_Europe = subset(WHO, Region == "Europe")
str(WHO_Europe)
write.csv(WHO_Europe, './dataset/WHO_EUROPE.csv')
ls()
rm(WHO_Europe)
# Data Analysis
WHO$Under15
mean(WHO$Under15)
sd(WHO$Under15)
summary(WHO$Under15)
which.min(WHO$Under15)
WHO$Country[86]
which.max(WHO$Under15)
WHO$Country[124]
plot(WHO$GNI, WHO$FertilityRate)
Outliers = subset(WHO, GNI > 10000 & FertilityRate > 2.5)
nrow(Outliers)
Outliers[c("Country", "GNI", "FertilityRate")]

mean(WHO$Over60)
WHO$Country[which.min(WHO$Over60)]
WHO$Country[which.max(WHO$LiteracyRate)]
# Plots
hist(WHO$CellularSubscribers)
boxplot(WHO$LifeExpectancy ~ WHO$Region)
boxplot(WHO$LifeExpectancy ~ WHO$Region, xlab="", ylab="Life Expentancy", main="Life Expectancy of Countries by Region")
table(WHO$Region)
tapply(WHO$Over60, WHO$Region, mean)
tapply(WHO$LiteracyRate, WHO$Region, min)
tapply(WHO$LiteracyRate, WHO$Region, min, na.rm=TRUE)

str(WHO)
tapply(WHO$ChildMortality, WHO$Region, mean)


