### Set-Up
setwd("~/UVa/1st Semester - Fall 2020/STAT 6021 - Linear Models for Data Science/Homeworks/Project 2")

set.seed(52)
data<-read.csv("bioChemists.csv", header=TRUE)
data.scram<-data[sample(1:nrow(data)),]

library(MASS)

### Factorization
data$fem<-factor(data$fem)
data$mar<-factor(data$mar)

data$kids<-ifelse(data$kid5>0, "TRUE", "FALSE")

data$kids<-factor(data$kids)

levels(data$fem)

plot(art~phd, data=data)

hist(data$kid5)

identify(data$art)

regnull<-lm(art~1, data=data.scram)
regmin<-lm(art~fem+mar+ment, data=data.scram)
regfull<-lm(art~., data=data.scram)

step(regmin, scope=list(lower=regmin, upper=regfull), direction="forward")

preds<-cbind(data$fem, data$mar, data$kid5, data$phd, data$ment)
round(cor(preds), 3)

library(faraway)
vif(result)

minphd<-lm(art~fem+mar+kid5+ment, data=data)

basekid5<-lm(art~kid5, data=data)
plot(basekid5$fitted.values, basekid5$residuals)
acf(basekid5$residuals)
qqnorm(basekid5$residuals)

### Autocorrelation Fit

attach(data)

res<-lm(art~ment)

ar.1<-arima(res$residuals, order=c(1,0,0), include.mean=FALSE)

shift<-ar.1$coef

y<-cbind(as.ts(art), lag(art))
yprime<-y[,2] - shift*y[,1]
x<-cbind(as.ts(ment), lag(ment))
xprime<-x[,2] - shift*x[,1]

res.prime<-lm(yprime~xprime)
summary(res.prime)