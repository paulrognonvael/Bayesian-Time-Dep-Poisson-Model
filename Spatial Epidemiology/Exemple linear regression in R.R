rm(list=ls())
#Simulated regression model.#
#Simulated data
n<-20
alpha.true<-0
beta.true<-1
set.seed(123)
X<-rnorm(n)
Y<-beta.true*X+rnorm(n)

plot(Y~X,pch=16)

#Linera regression
prova<-summary(lm(Y~X))
prova
anova(aov(prova))
