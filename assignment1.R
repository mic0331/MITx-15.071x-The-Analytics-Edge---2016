setwd("/media/mic0331/Data/Data Analysis/Courses/Edx/MITx: 15.071x The Analytics Edge")
mvt = read.csv("./dataset/mvtWeek1.csv")
nrow(mvt)
str(mvt)
max(mvt$ID)
min(mvt$Beat)
summary(mvt$Arrest)

alley = subset(mvt, LocationDescription=='ALLEY')
summary(alley)
summary(mvt$Date[1])
DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))
summary(DateConvert)
mvt$Month = months(DateConvert)
mvt$Weekday = weekdays(DateConvert)
mvt$Date = DateConvert
table(mvt$Weekday)
max(table(mvt$Weekday))
tapply(mvt$Arrest, mvt$Month, sum, na.rm=TRUE)
max(tapply(mvt$Arrest, mvt$Month, sum, na.rm=TRUE))
table(mvt$Arrest,mvt$Month)

hist(mvt$Date, breaks = 100)
boxplot(mvt$Date ~ mvt$Arrest)

table(mvt$Arrest,mvt$Year)
2152/(18517+2152)
1212/(13068+1212)
550/(13542+550)

sort(table(mvt$LocationDescription))
Top5 <- mvt[mvt$LocationDescription %in% c("STREET", "PARKING LOT/GARAGE(NON.RESID.)", "ALLEY", "DRIVEWAY - RESIDENTIAL", "GAS STATION", ""),]

Top5$LocationDescription = factor(Top5$LocationDescription)
str(Top5)

table(Top5$LocationDescription, Top5$Weekday)

# ------------------------------------------------------------------------------------------------------------

IBM = read.csv("./dataset/IBMStock.csv")
GE = read.csv("./dataset/GEStock.csv")
ProcterGamble = read.csv("./dataset/ProcterGambleStock.csv")
CocaCola = read.csv("./dataset/CocaColaStock.csv")
Boeing = read.csv("./dataset/BoeingStock.csv")

IBM$Date = as.Date(IBM$Date, "%m/%d/%y")
GE$Date = as.Date(GE$Date, "%m/%d/%y")
CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")
ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")
Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")

mean(IBM$StockPrice)
min(GE$StockPrice)
max(CocaCola$StockPrice)
median(Boeing$StockPrice)
sd(ProcterGamble$StockPrice)

plot(CocaCola$Date, CocaCola$StockPrice, type="l", col="red")
lines(ProcterGamble$Date, ProcterGamble$StockPrice, col="blue")
abline(v=as.Date(c("2000-03-01")), lwd=2)
abline(v=as.Date(c("1983-01-01")), lwd=2)

plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type="l", col="red", ylim=c(0,210))
lines(ProcterGamble$Date[301:432], ProcterGamble$StockPrice[301:432], col="blue")
lines(IBM$Date[301:432], IBM$StockPrice[301:432], col="green")
lines(GE$Date[301:432], GE$StockPrice[301:432], col="purple")
lines(Boeing$Date[301:432], Boeing$StockPrice[301:432], col="orange")
abline(v=as.Date(c("2000-03-01")), lwd=2)
abline(v=as.Date(c("1997-10-01")), lwd=2)
abline(v=as.Date(c("1997-09-01")), lwd=2)
abline(v=as.Date(c("2004-01-01")), lwd=2)
abline(v=as.Date(c("2005-12-31")), lwd=2)
mean(IBM$StockPrice)
tapply(IBM$StockPrice, months(IBM$Date), mean)
tapply(Boeing$StockPrice, months(Boeing$Date), mean)
tapply(CocaCola$StockPrice, months(CocaCola$Date), mean)
tapply(GE$StockPrice, months(GE$Date), mean)
tapply(ProcterGamble$StockPrice, months(ProcterGamble$Date), mean)

# ------------------------------------------------------------------------------------------------------------

CPS = read.csv("./dataset/CPSData.csv")
summary(CPS$Industry)
sort(table(CPS$State)) 
min(sort(table(CPS$State)) )
max(sort(table(CPS$State)) )

table(CPS$Citizenship)
(116639 + 7073) / (116639 + 7073 + 7590)

table(CPS$Hispanic,CPS$Race)
str(CPS)
colnames(CPS)[colSums(is.na(CPS)) > 0]
table(CPS$Region, is.na(CPS$Married))
table(CPS$Sex, is.na(CPS$Married))
table(CPS$Age, is.na(CPS$Married))
table(CPS$Citizenship, is.na(CPS$Married))

table(CPS$Region, is.na(CPS$MetroAreaCode))
mean(c(TRUE, FALSE, TRUE, TRUE))

tapply(is.na(CPS$MetroAreaCode), CPS$State, mean)
sort(tapply(is.na(CPS$MetroAreaCode), CPS$State, mean))

MetroAreaMap = read.csv("./dataset/MetroAreaCodes.csv")
CountryMap = read.csv("./dataset/CountryCodes.csv")

CPS = merge(CPS, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)
str(CPS)
summary(CPS)

table(is.na(CPS$MetroArea))
sort(table(CPS$MetroArea))
sort(tapply(CPS$Race == "Asian", CPS$MetroArea, mean))
sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean, na.rm=TRUE))
str(CPS)
CPS = merge(CPS, CountryMap, by.x="CountryOfBirthCode", by.y="Code", all.x=TRUE)
str(CPS)
table(is.na(CPS$Country))

sort(table(CPS$Country))

tapply(CPS$MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA", CPS$Country != "United States", mean, na.rm=TRUE)
table(CPS$MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA", CPS$Country != "United States")
1668/(1668+3736)

sort(tapply(CPS$Country == "India", CPS$MetroArea, sum, na.rm=TRUE))

sort(tapply(CPS$Country == "Brazil", CPS$MetroArea, sum, na.rm=TRUE))

sort(tapply(CPS$Country == "Somalia", CPS$MetroArea, sum, na.rm=TRUE))

# ------------------------------------------------------------------------------------------------------------

poll = read.csv("./dataset/AnonymityPoll.csv")
summary(poll$Smartphone)
str(poll)
487+472+43
table(poll$State, poll$Region)
table(!poll$Internet.Use & !poll$Smartphone)
table(poll$Internet.Use & poll$Smartphone)
table(poll$Internet.Use & !poll$Smartphone)
table(!poll$Internet.Use & poll$Smartphone)

table(is.na(poll$Internet.Use))
table(is.na(poll$Smartphone))

limited <- subset(poll, Internet.Use | Smartphone)
colnames(limited)[colSums(is.na(limited)) > 0]

mean(limited$Info.On.Internet)
table(limited$Info.On.Internet)

table(limited$Worry.About.Info)
386/(404+386)
table(limited$Anonymity.Possible)
278/(475+278)
table(limited$Tried.Masking.Identity)
128/(656+128)
table(limited$Privacy.Laws.Effective)
186/(541+186)
hist(limited$Age)
plot(limited$Age, limited$Info.On.Internet)
max(table(limited$Age, limited$Info.On.Internet))

jitter(c(1, 2, 3))
plot(jitter(limited$Age), jitter(limited$Info.On.Internet))

tapply(limited$Info.On.Internet, limited$Smartphone, mean)
tapply(limited$Tried.Masking.Identity, limited$Smartphone, table)
tapply(limited$Tried.Masking.Identity, limited$Smartphone, summary)
