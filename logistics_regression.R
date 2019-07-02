rm(list=ls())
getwd()

# loading data in R 
t<-read.csv("1Telecom churn.csv")
str(t)

# removing some column
t<-t[,-c(1,4)]

# transforming data
t$international.plan<-ifelse(t$international.plan=="yes",0,1)
t$voice.mail.plan<-ifelse(t$voice.mail.plan=="yes",0,1)

# checking for missing value and omiting them
sapply(t, function(x) sum(is.na(x)))
t <- t[complete.cases(t), ]

# checking multi-collinearity and removing 1 column
library(corrplot)
a<-cor(t)
corrplot(a,method = "number")
t<-t[,-5]
colnames(t)

# univariate analysis
bx<-boxplot(t$total.day.calls)
quantile(t$total.day.calls,seq(0,1,0.01))
bx$stats
t$total.day.calls<-ifelse(t$total.day.calls>=152,152,t$total.day.calls)
t$total.day.calls<-ifelse(t$total.day.calls<=47,47,t$total.day.calls)

bx1<-boxplot(t$total.day.charge)
quantile(t$total.day.charge,seq(0,1,0.01))
bx1$stats
t$total.day.charge<-ifelse(t$total.day.charge>=55.2,55.2,t$total.day.charge)
t$total.day.charge<-ifelse(t$total.day.charge<=5.97,5.97,t$total.day.charge)

bx2<-boxplot(t$total.eve.calls)
quantile(t$total.eve.calls,seq(0,1,0.01))
bx2$stats
t$total.eve.calls<-ifelse(t$total.eve.calls>=154,154,t$total.eve.calls)
t$total.eve.calls<-ifelse(t$total.eve.calls<=48,48,t$total.eve.calls)

bx3<-boxplot(t$total.eve.charge)
quantile(t$total.eve.charge,seq(0,1,0.01))
bx3$stats
t$total.eve.charge<-ifelse(t$total.eve.charge>=28.650,28.650,t$total.eve.charge)
t$total.eve.charge<-ifelse(t$total.eve.charge<=5.470,5.470,t$total.eve.charge)

bx5<-boxplot(t$total.night.charge)
quantile(t$total.night.charge,seq(0,1,0.01))
bx5$stats
t$total.night.charge<-ifelse(t$total.night.charge>=15.06,15.06,t$total.night.charge)
t$total.night.charge<-ifelse(t$total.night.charge<=2.96,2.96,t$total.night.charge)

bx6<-boxplot(t$total.intl.calls)
quantile(t$total.intl.calls,seq(0,1,0.01))
bx6$stats
t$total.intl.calls<-ifelse(t$total.intl.calls>=10,10,t$total.intl.calls)

bx7<-boxplot(t$total.intl.charge)
quantile(t$total.intl.charge,seq(0,1,0.01))
bx7$stats
t$total.intl.charge<-ifelse(t$total.intl.charge>=4.67,4.67,t$total.intl.charge)
t$total.intl.charge<-ifelse(t$total.intl.charge<=0.89,0.89,t$total.intl.charge)

#lets divide the data into test and train
set.seed(222)
df<-sample(1:nrow(t),0.7*nrow(t))
t_train=t[df,]
t_test=t[-df,]

#checking the multi-collinearity
library(car)
mod<- lm(churn ~ ., data=t_train)
t = vif(mod)
sort(t, decreasing = T)

#Since all the variable are below the threshold of 5, we can proceed with the model
mod1 <- glm(churn ~. , family="binomial"(link="logit"), data=t_train)
summary(mod1)

# using step function 
stpmod = step(mod1, direction = "both")
formula(stpmod)
summary(stpmod)

#checking the probability for each observation by creating a variable names score
mod2 <- glm(churn ~ international.plan + voice.mail.plan + total.day.charge + total.eve.charge + total.night.charge + total.intl.calls + total.intl.charge + customer.service.calls, family="binomial"(link = "logit"), data=t_train)
summary(mod2)
t_train$score=predict(mod2,newdata=t_train,type = "response")
head(t_train$score)
tail(t_train$score)

#Lets try to analyse the confusion matrix and model accuracy
library(lattice)
library(ggplot2)
library(caret)
library(e1071)
prediction<-ifelse(t_train$score>=0.5,1,0)
confusionMatrix(prediction,t_train$churn)
# accuracy = 86.63% , Recall = 97.49% and precision = 88.20%

# Concordance Test #
library(InformationValue)
library(caret)
concor <- Concordance(t_train$churn,t_train$score)
concor

#lets check the AUC and ROC
##AUC
library(InformationValue)
plotROC(actuals = t_train$churn,predictedScores = as.numeric(fitted(mod2)))
ks_plot(actuals = t_train$churn,predictedScores = as.numeric(fitted(mod2)))
ks_stat(actuals = t_train$churn,predictedScores = as.numeric(fitted(mod2)))
t_test$score2= predict(mod2, t_test, type="response")
View(t_test)

