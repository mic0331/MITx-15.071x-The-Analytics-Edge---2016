setwd("/media/mic0331/Data/Data Analysis/Courses/Edx/MITx: 15.071x The Analytics Edge")
songs <- read.csv('./dataset/songs.csv')
str(songs)
songs_2010 <- subset(songs, year == 2010)
songs_mj <- subset(songs, artistname == "Michael Jackson")
songs_mj_top10 <- subset(songs, artistname == "Michael Jackson" & Top10 == 1)
unique(songs$timesignature)

table(songs$timesignature)
which.max(songs$tempo)
songs[6206,]$songtitle

SongsTrain <- subset(songs, year <= 2009)
SongsTest <- subset(songs, year == 2010)

nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]
SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]

model1 <- glm(Top10 ~ ., data=SongsTrain, family=binomial)
summary(model1)

cor(SongsTrain$loudness, SongsTrain$energy)
SongsLog2 = glm(Top10 ~ . - loudness, data=SongsTrain, family=binomial)
summary(SongsLog2)
SongsLog3 = glm(Top10 ~ . - energy, data=SongsTrain, family=binomial)
summary(SongsLog3)
pred <- predict(SongsLog3, type="response", newdata = SongsTest)
table(SongsTest$Top10, pred >= .45)

(309+19)/nrow(SongsTest)
(314)/nrow(SongsTest)

19/(19+40)
309/(309+5)

# ---------------------------------------------------------------------------------------------------------------------------------------

parole <- read.csv('./dataset/parole.csv')
str(parole)
table(parole$violator)
summary(parole)
str(parole)
parole$state <- as.factor(parole$state)
parole$crime <- as.factor(parole$crime)
summary(parole$crime)

set.seed(144)
library(caTools)
split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split == TRUE)
test = subset(parole, split == FALSE)
mod <- glm(violator ~ ., family="binomial", data = train)
summary(mod)

# is male, of white race, aged 50 years at prison release, from the state of Maryland, served 3 months, had a maximum sentence of 12 months, did not commit multiple offenses, and committed a larceny
odds <- exp(-4.2411574 + 0.3869904 -0.0001756 - 50*0.0001756 -3 * 0.1238867 + 12 * 0.0802954 + 0.6837143)
odds
1 / (1 + odds)

odds <- as.numeric(exp(coefficients(mod)[c("(Intercept)")]+coefficients(mod)[c("male")]+coefficients(mod)[c("race")] + coefficients(mod)[c("time.served")]*3 + coefficients(mod)[c("max.sentence")]*12 + coefficients(mod)[c("multiple.offenses")]*0 + coefficients(mod)[c("crime2")]))
p = odds/(1+odds)
odds
p

prediction <- predict(mod, type="response", newdata = test)
summary(prediction)

table(test$violator, prediction >= .5)
12/(11+12)
167/(167+12)
(167+12) / nrow(test)
table(test$violator)
179/(179+23)

library(ROCR)
ROCRpred <- prediction(prediction, test$violator)
as.numeric(performance(ROCRpred, "auc")@y.values)
# ---------------------------------------------------------------------------------------------------------------------------------------
loans <- read.csv('./dataset/loans.csv')
str(loans)
summary(loans)
table(loans$not.fully.paid)
1533/(8045+1533)
summary(loans)

install.packages("mice")
library(mice)
set.seed(144)
vars.for.imputation = setdiff(names(loans), "not.fully.paid")
imputed = complete(mice(loans[vars.for.imputation]))
loans[vars.for.imputation] = imputed

set.seed(144)
library('caTools')
split = sample.split(loans$not.fully.paid, SplitRatio = .7)
train = subset(loans, split == TRUE)
test = subset(loans, split == FALSE)
modelLog = glm(not.fully.paid ~ ., data=train, family=binomial)
summary(modelLog)
logA=9.260+(-9.406e-03*700)
logB=9.260+(-9.406e-03*710)
ans1 <- logA - logB
ans1
ans2 <- exp(logA)/exp(logB)
ans2

loansPred <- predict(modelLog, newdata = test, type = "response")
test$predicted.risk <- loansPred
t <- table(test$not.fully.paid, loansPred > 0.5)
Ntest <- nrow(test)
TN <- t[1] 
FP <- t[3] 
FN <- t[2] 
TP <- t[4] 
Acc <- (TN+TP)/Ntest
Acc
Baseline <- (TN+FP)/Ntest
Baseline

library(ROCR)
ROCRpred = prediction(loansPred, test$not.fully.paid)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))
ROCRauc <- as.numeric(performance(ROCRpred, "auc")@y.values)
ROCRauc


model1 <-glm(not.fully.paid ~ int.rate, data = train, family = "binomial")
summary(model1)

smartPred <- predict(model1, newdata = test, type = "response")
summary(smartPred)
table(test$not.fully.paid, smartPred > 0.5)

ROCRsmartpred = prediction(smartPred, test$not.fully.paid)
ROCRsmartperf = performance(ROCRsmartpred, "tpr", "fpr")
plot(ROCRsmartperf, colorize=TRUE)
ROCRsmartauc = performance(ROCRsmartpred, "auc")@y.values
ROCRsmartauc

10*exp(0.06*3)
test$profit = exp(test$int.rate*3) - 1
test$profit[test$not.fully.paid == 1] = -1
ans <- max(test$profit)*10
ans

highInterest  <- subset(test, test$int.rate  >= 0.15)
mean(highInterest$profit)
sum(highInterest$not.fully.paid == 1)/nrow(highInterest)

cutoff = sort(highInterest$predicted.risk, decreasing=FALSE)[100]
selectedLoans = subset(highInterest, predicted.risk <= cutoff) 
dim(selectedLoans)

sum(selectedLoans$profit)
table(selectedLoans$not.fully.paid)
sum(selectedLoans$not.fully.paid == 1)

# --------------------------------------------------------------------------------------------------------------------------------------

baseball <- read.csv('./dataset/baseball.csv')
sum(table( baseball$Year))
length(table(baseball$Year))
baseball <- subset(baseball, baseball$Playoffs == 1)
table(baseball$Year,baseball$Playoffs)
table(table(baseball$Year))

PlayoffTable = table(baseball$Year)
names(PlayoffTable)

PlayoffTable[c("1990", "2001")]
baseball$NumCompetitors = PlayoffTable[as.character(baseball$Year)]

table(baseball$Playoffs, baseball$NumCompetitors == 8)
baseball$WorldSeries = as.numeric(baseball$RankPlayoffs == 1)
table(baseball$WorldSeries)

modelLog <- glm(WorldSeries ~ Year + RA + RankSeason + NumCompetitors, data = baseball, family=binomial)
summary(modelLog)

cor(baseball$Year, baseball$RA)
cor(baseball$Year, baseball$RankSeason)
cor(baseball$Year, baseball$NumCompetitors)
cor(baseball$RA, baseball$RankSeason)
cor(baseball$RA, baseball$NumCompetitors)
cor(baseball$RankSeason, baseball$NumCompetitors)
