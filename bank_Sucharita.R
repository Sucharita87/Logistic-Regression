#Output variable -> y
#y -> Whether the client has subscribed a term deposit or not (Binomial ("yes" or "no"))
library(readr)
library(ROCR)
bank<-read.csv("F://ExcelR//Assignment//Logistic regression//bank-full.csv")
View(bank)
bank<-na.omit(bank)
View(bank)
summary(bank)
str(bank)
bank$day=as.factor(bank$day)
bank$campaign=as.factor(bank$campaign)
bank$pdays=as.factor(bank$pdays)
bank$previous=as.factor(bank$previous)
str(bank)
attach(bank)


fit1<-glm(y~ age+job+marital+education+balance+housing+loan, family = "binomial", data = bank)
summary(fit1) # AIC= 30941

fit2<-glm(y~ age+job+marital+education, family = "binomial", data = bank)
summary(fit2) # AIC= 31658

fit3<-glm(y~ job+loan+duration+poutcome, data= bank, family = "binomial")
summary(fit3) # AIC= 24017

fit4<-glm(y~ age+job+balance+housing+loan, family = "binomial", data = bank)
summary(fit4) # AIC= 31118

fit5<-glm(y~ age+job+marital+education+housing+balance+loan+duration+campaign+poutcome, data= bank, family = "binomial")
summary(fit5) #AIC= 23144

fit6<-glm(y~ age+job+loan+duration+poutcome, data= bank, family = "binomial")
summary(fit6) #AIC= 24019

fit8<-glm(y~ ., data= bank, family = "binomial")
summary(fit8)

#USING fit5

exp(coef(fit5))
table(bank$y)
prob <- predict(fit5,type=c("response"),bank)
prob
confusion<-table(prob>0.5,bank$y)
probo <- prob>0.5
table(probo)
confusion

Accuracy<-sum(diag(confusion)/sum(confusion))
Accuracy # 0.90
Error <- 1-Accuracy
Error # 0.09

sensitivity<-(1679/sum(1679+888)) # TPR = TP/TP+FN
sensitivity # 0.65

specificity<-(39034/(39034+3610)) # = TN/TN+FP
specificity # 0.915

precision<-(1679/(1679+3610))
precision # 0.317

a<-(sensitivity * precision)
b<-(sensitivity+precision)
F1<-(2*a/b)
F1 # 0.427

rocrpred<-prediction(prob,bank$y)
rocrperf<-performance(rocrpred,'tpr','fpr')
plot(rocrperf,colorize=T)



