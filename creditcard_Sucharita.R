#Classify whether application accepted or not using Logistic regression
library(readr)
library(mlogit)
library(ROCR)
card<-read.csv("F://ExcelR//Assignment//Logistic regression//creditcard.csv")
View(card)
attach(card)
card<-na.omit(card)
str(card)
summary(card)
card1<-subset(card, select = -c(X)) # drop first column
View(card1)
attach(card1)
fit1<-glm(card~., family= "binomial", data = card1)
summary(fit1) # all  variables have p value<0.05, so all are significant

card1$reports=as.factor(card1$reports)
card1$majorcards=as.factor(card1$majorcards)
card1$card=as.factor(card1$card)
card1$owner=as.factor(card1$owner)
card1$selfemp=as.factor(card1$selfemp)
str(card1)
fit2<-glm(card~., family = "binomial", data = card1)
summary(fit2) # all  variables have p value<0.05, so all are significant

fit3<-glm(card~age+income+expenditure+dependents+active+majorcards, family="binomial", data=card1)
summary(fit3) # AIC= 157.77, only dependents is ignificant variable

fit4<-glm(card~dependents, family="binomial", data=card1)
summary(fit4) # AIC= 1406.9, very high

fit5<-glm(card~income+expenditure+dependents+active+majorcards, family="binomial", data=card1)
summary(fit5)# AIC= 157.51, let us consider this model, as it has the least AIC

table(card1$card)
prob <- predict(fit5,type=c("response"),card1)
prob
confusion<-table(prob>0.5,card1$card)
probo <- prob>0.5
table(probo)
confusion

Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy # 0.98
Error <- 1-Accuracy
Error # 0.015

sensitivity<-(1002/sum(1002+21)) # TPR = TP/TP+FN
sensitivity # 0.9794

specificity<-(0/(0+296)) # = TN/TN+FP
specificity # 0 

precision<-(1002/(1002+296))
precision # 0.77

a<-(sensitivity * precision)
b<-(sensitivity+precision)
F1<-(2*a/b)
F1 # 0.86

rocrpred<-prediction(prob,card1$card)
rocrperf<-performance(rocrpred,'tpr','fpr')
plot(rocrperf,colorize=T)
