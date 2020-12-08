#load data in and necessary libraries
library(leaps)
library(faraway)
library(MASS)
library(pscl)
data <- as.data.frame(bioChemists)

#attach
attach(data)

#summary stats
summary(data)

#factor
fem<-factor(fem)
mar<-factor(mar)
#check that factoring worked
is.factor(fem)
is.factor(mar)
#check levels
levels(fem)
levels(mar)

#checking reference
contrasts(fem) #man is reference class
contrasts(mar) #single is reference class

#full inital regression
artedit<-art + 0.01
init_reg<-lm(artedit~fem+mar+kid5+phd+ment, data=data)
summary(init_reg)
anova(init_reg)

#check correlation among quantitative variables
preds<-cbind(fem,mar,kid5,phd,ment)
round(cor(preds), 3) #mar and kid5 strongest correlation

#residual plot of inital regression model
plot(init_reg$fitted.values,init_reg$residuals,main="Residual plot")
abline(h=0,col="red")

#boxcox
boxcox(init_reg, lambda = seq(0.2,0.4, 1/10))

#acf and pacf
acf(init_reg$residuals)
pacf(init_reg$residuals)

#normality assumptions
qqnorm(init_reg$residuals)
qqline(init_regt$residuals, col="red")




###identified issue of ordering of number articles. scramble and rerun reg###
set.seed(444)

data.scram<-data[sample(1:nrow(data)),]
data.scram$fem<-factor(data.scram$fem)
data.scram$mar<-factor(data.scram$mar)
data.scram$arteditscram<-data.scram$art + 0.01

#redo regression model for all variables
scram_reg<-lm(arteditscram~fem+mar+kid5+phd+ment, data=data.scram)
summary(scram_reg)
anova(scram_reg)

#rerun assumptions
boxcox(scram_reg,lambda = seq(0.2,0.4, 1/10))

plot(scram_reg$fitted.values,scram_reg$residuals,main="Residual plot - scrambled")
abline(h=0,col="red")

acf(scram_reg$residuals, main = "ACF Plot for Scrambled data")
pacf(scram_reg$residuals, main = "PACF Plot for Scrambled data")

qqnorm(scram_reg$residuals)
qqline(scram_regt$residuals, col="red")


#identified transformation on response variable
data.scram$art.third<-(data.scram$arteditscram)^(3/10)

trans_reg <- lm(art.third~fem+mar+kid5+phd+ment, data=data.scram)
summary(trans_reg)
anova(trans_reg)

#assumptions
boxcox(trans_reg)

plot(trans_reg$fitted.values,trans_reg$residuals,main="Residual plot - transformed")
abline(h=0,col="red")

acf(trans_reg$residuals, main = "ACF Plot for Transformed data")
pacf(trans_reg$residuals, main = "PACF Plot for Transformed data")

qqnorm(trans_reg$residuals)
qqline(trans_reg$residuals, col="red")



