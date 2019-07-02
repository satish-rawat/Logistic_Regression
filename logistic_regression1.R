rm(list=ls())
getwd()
# without outlier treatment
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
# accuracy = 86.67% , Recall = 97.64% and precision = 88.17%

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