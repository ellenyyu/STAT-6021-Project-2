### Set-Up
setwd("~/UVa/1st Semester - Fall 2020/STAT 6021 - Linear Models for Data Science/Homeworks/Project 2")

set.seed(52)
data<-read.csv("bioChemists.csv", header=TRUE)
data.scram<-data[sample(1:nrow(data)),]
data.scram$fem<-factor(data.scram$fem)
data.scram$mar<-factor(data.scram$mar)

# We create the initial regression

data.scram$art.plus<-data.scram$art + 0.01

reg<-lm(art.plus~fem+mar+kid5+phd+ment, data=data.scram)

# Checking data assumptions

boxcox(reg)

data.scram$art.third<-(data.scram$art.plus)^(1/3)

reg.third<-lm(art.third~fem+mar+kid5+phd+ment, data=data.scram)

plot(reg.third$fitted.values, reg.third$residuals)

acf(reg.third$residuals)

qqnorm(reg.third$residuals)
qqline(reg.third$residuals)

# reg.pois<-glm(art.third~fem+mar+kid5+phd+ment, family="poison", data=data.scram)

# Scatterplots
plot(art.third~kid5, data=data.scram)
boxplot(art~fem, data=data.scram)
boxplot(art~mar, data=data.scram)
plot(art.third~phd, data=data.scram)
plot(art.third~ment, data=data.scram)

# Looking for best model
regnull<-lm(art.third~1, data=data.scram)
step(regnull, scope=list(lower=regnull, upper=reg.third), direction="forward")

# Marital status - kids affect?
kid.contr<-lm(art.third~kid5+mar, data=data.scram)
kid.test<-lm(art.third~kid5*mar, data=data.scram)

anova(kid.contr, kid.test)

# No single people w/ kids, oh well

# Marital status - gender affect?
gen.contr<-lm(art.third~fem+mar, data=data.scram)
summary(gen.contr)
gen.test<-lm(art.third~fem*mar, data=data.scram)
summary(gen.test)

anova(gen.contr, gen.test)

# Multicollinearity checks
preds<-cbind(data.scram$fem,
             data.scram$mar,
             data.scram$kid5,
             data.scram$phd,
             data.scram$ment)
round(cor(preds), 4)

vif(full.contr)