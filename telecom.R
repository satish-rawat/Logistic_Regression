# loading library

library(randomForestSRC)
library('ltm')
library('dplyr')
library('VIM')
library('car')
library('lattice')
library('ggplot2')
library('caret')
library('e1071')
library('InformationValue')

# loading data in R

telecom<-read.csv("telecom.csv")
View(telecom)
str(telecom)
summary(telecom)
dim(telecom)

# missing value pattern

mice::md.pattern(telecom,plot = TRUE, rotate.names = TRUE)
mice_plot <- aggr(telecom, col=c('orange','green'), numbers=TRUE, sortVars=TRUE, labels=names(telecom), 
                  cex.axis=1, gap=5, ylab=c("Missing data","Pattern"))

# DATA PRE PROCESSING, TRANSFORMATION AND CLEANING
# Data Cleaning
# removing missing values row because missing value percentage is very low

removed_na_telecom <- na.omit(telecom)
dim(removed_na_telecom)

# 17 rows are omitted out 3333
# Data Transformation creating Dummy Variable

removed_na_telecom$international.plan.yes <- ifelse(removed_na_telecom$international.plan == "yes",1,0)
removed_na_telecom$voice.mail.plan.yes <- ifelse(removed_na_telecom$voice.mail.plan == "yes",1,0)

frequency_count_service_calls <- table(removed_na_telecom$customer.service.calls)
frequency_count_churn <- table(removed_na_telecom$churn)
removed_na_telecom$area.code <- as.factor(removed_na_telecom$area.code)
frequency_count_area.code <- table(removed_na_telecom$area.code)
frequency_count_service_calls
frequency_count_churn
frequency_count_area.code
str(removed_na_telecom)

# Data Cleaning for modelling
# removing some column as they are not helpful in modelling

final_telecom<-removed_na_telecom[,-c(1,4,5,6)]
str(final_telecom)

# Data Exploration and Visualization

removed_na_telecom %>%
  dplyr::select(c(account.length,number.vmail.messages,total.day.calls,total.day.charge,total.eve.calls,
                  total.eve.charge,total.night.calls,total.night.charge,total.intl.calls,total.intl.charge)) %>%
  reshape2::melt(id.vars = "account.length") %>%
  ggplot(aes(x = value, colour = variable)) +
  geom_histogram() + 
  facet_wrap(~variable, scales = "free", ncol = 3) +
  labs(x = "Variable Value") +
  theme_minimal()

removed_na_telecom %>%
  dplyr::select(c(account.length,total.day.calls,total.eve.calls,total.night.calls,total.day.calls,churn)) %>%
  melt(id.vars = "churn") %>%
  ggplot(aes(x = value,  fill = churn)) +
  geom_histogram(stat ='bin') + 
  facet_wrap(~variable, scales = "free", ncol = 2) +
  labs(x = "Variable Value") +
  theme_minimal()

ggplot(removed_na_telecom, aes(x=account.length, y =total.night.charge, color = churn))+
  geom_smooth() + theme_minimal() + labs(x = "Account Length", y = "Total Night Charge", 
                                         title = "Churning Account Length Wise and Night Call Charge Wise", color = "Churn")

ggplot(removed_na_telecom, aes(y = account.length,x = as.factor(customer.service.calls), color = churn)) + 
  geom_point(position = "jitter") + scale_x_discrete(position = "bottom") + theme_minimal() + 
  labs(x = "Customer Service Call Count", y = "Account Length", color = "Churn", 
       title ="Churning Account Length Wise and Customer Service Calls Wise")

ggplot(removed_na_telecom,aes(y = account.length,x = as.factor(area.code), color = churn))+geom_point(position = 'jitter')+
  theme_minimal() + labs(x = "Area Code", y = "Account Length", title = "Churning Area Code Wise And Account Length Wise", fill = "Churn")

ggplot(removed_na_telecom, aes(x=as.factor(area.code), fill = churn )) + geom_bar(position = "dodge") + 
  geom_text(stat = "count",  aes(label=..count..), vjust =1.5, position = position_dodge(0.9))+ 
  labs(x="Area Code", y = "Count", fill = "Churn", title = "Churning by Area Code")  + theme_minimal()

ggplot(telecom, aes(x=as.factor(customer.service.calls), fill = churn )) + geom_bar(position = "fill") + 
  geom_text(stat = "count",  aes(label=..count..), vjust =1.5, position = position_fill(0.9))+ 
  labs(x="Number of Service Calls", y = "Count", fill = "Churn", title = "Churning Customer Service Call Wise")  + theme_minimal()
di
ggplot(removed_na_telecom, aes(x=international.plan, fill = churn)) + geom_bar(position = "fill") +
  geom_text(stat = "count", aes (label = ..count..), vjust =1.5,position = position_fill(0.9)) + 
  labs(y = "Count", x = "International Plan", fill = "Churn") + theme_minimal()

ggplot(removed_na_telecom, aes(x=voice.mail.plan, fill = churn)) + geom_bar(position = "fill") +
  geom_text(stat = "count", aes (label = ..count..), vjust =1.5,position = position_fill(0.9)) + 
  labs(y = "Count", x = "Voice Mail Plan", fill = "Churn") + theme_minimal()

ggplot(removed_na_telecom, aes(x=area.code, fill = international.plan)) + geom_bar(position = "fill") +
  geom_text(stat = "count", aes (label = ..count..), vjust =1.5,position = position_fill(0.9)) + 
  labs(y = "Count", x = "Area Code", fill = "International Plan", title = "International Plan Area Code Wise") + theme_minimal()

ggplot(removed_na_telecom, aes(x=area.code, fill = voice.mail.plan)) + geom_bar(position = "fill") +
  geom_text(stat = "count", aes (label = ..count..), vjust =1.5,position = position_fill(0.9)) + 
  labs(y = "Count", x = "Area Code", fill = "Voice Mail Plan", title = "Voice Mail Plan Area Code Wise") + theme_minimal()

ggplot(removed_na_telecom,aes(x = as.factor(international.plan),y = total.intl.charge, color = area.code))+geom_point(position = 'jitter')+
  theme_minimal() + labs(x = "International Plan", y = "Total International Charge",  color = "Area Code")

ggplot(removed_na_telecom,aes(x = as.factor(voice.mail.plan),y = account.length, color = churn))+geom_point(position = 'jitter')+
  theme_minimal() + labs(x = "Voice Mail Plan", y = "Account  Length",  color = "Churn")


# calculating Point Biserial Correlation

numerical_variable_df <- select_if(removed_na_telecom, is.numeric)
colnames(numerical_variable_df)
for (i in numerical_variable_df){
    value = ltm::biserial.cor(x = i , y = removed_na_telecom$churn, use = c("all.obs", "complete.obs"), level = 1)
    print(paste("Point Biserial Correlation coefficient Values are : ", value))
}  


#lets divide the data into test and train

set.seed(222)
df<-sample(1:nrow(final_telecom),0.7*nrow(final_telecom))
t_train_log <-final_telecom[df,]
t_test_log <-final_telecom[-df,]
t_train_rf <- final_telecom[df,]
t_test_rf <- final_telecom[-df,]
dim(t_train_log)
dim(t_test_log)
table(t_train_log$churn)
table(t_test_log$churn)
dim(t_train_rf)
dim(t_test_rf)
table(t_train_rf$churn)
table(t_test_rf$churn)

# parameters for comparsion between Random Forest and Logistic Regression are
# Accuracy, Sensitivity (Recall or True positive rate), Specificity (True negative rate), Precision (Positive predictive value) and AUC
# Logistic Regression 

model1 <- glm(churn ~. , family=binomial(link="logit"), data=t_train_log)
summary(model1)
vif(model1)
step(model1, direction = "both")

model2 <- glm(churn ~ total.day.charge + total.eve.charge + total.night.charge + 
                total.intl.calls + total.intl.charge + customer.service.calls + 
                international.plan.yes + voice.mail.plan.yes, family = binomial(link = "logit"), 
              data = t_train_log)
summary(model2)
vif(model2)

# final model

t_train_log$score <- predict(model2, newdata=t_train_log, type = "response")

# visualising score to check a good cut off

ggplot(t_train_log,aes(x = churn , y = score, color = churn)) + geom_point(position = 'jitter')+
  theme_minimal() + labs(x = "CHURN", y = "SCORE",  color = "CHURN")

# Lets try to analyse the confusion matrix for different cut-off value

for (i in seq(0.1,0.4,0.05)){
  prediction<-ifelse(t_train_log$score>=i,TRUE,FALSE)
  conf <- caret::confusionMatrix(as.factor(prediction),as.factor(t_train_log$churn),positive ="TRUE")
  print(paste("THE CUT OFF VALUE is :",i))
  print(conf)
}

prediction<-ifelse(t_train_log$score>=0.15,TRUE,FALSE)
conf <- caret::confusionMatrix(as.factor(prediction),as.factor(t_train_log$churn),positive ="TRUE")
conf
# at cut-off= 0.15 both sensitivity and specifivity are optimal 71% and 77% respecively

# Concordance Test #

concor <- Concordance(t_train_log$churn,t_train_log$score)
concor

##AUC

log_reg_auc <- plotROC(actuals = t_train_log$churn,predictedScores = as.numeric(fitted(model2)))

# Random Forest Method 
# Dataset is imbalance using balanced random forest
# we are using misclassification error rate for assessing balanced random forest model
# This means imposing penalty on model for missclassification of a class
# This approach is taken because we interested in predicting how many customers will churn out 

# baseline model random forest model

x <- t_train_rf[,-13]
y <- t_train_rf[,13]
t_train_rf$churn <- as.factor(t_train_rf$churn) 
t_test_rf$churn <- as.factor(t_test_rf$churn) 
formula <- formula (churn ~.)

tuned_mdelrf1 <- randomForest::tuneRF(x,as.factor(y),mtryStart = 3,stepFactor = 1.5,ntreetry = 3000,
                                      improve = 0.1,trace = TRUE,plot = TRUE,doBest = FALSE)
model_rf <- rfsrc(formula = formula, data = t_train_rf, ntree = 3000, mtry = 6, splitrule = "auc")

# balanced random forest without optimize


modelrf_brf1 <- imbalanced(formula = formula , data = t_train_rf , ntree = 3000, method = "brf",
                           perf.type = "misclass")

# balanced random forest with optimize

modelrf_brf2 <- imbalanced(formula = formula , data = t_train_rf , ntree = 3000, method = "brf",optimize = TRUE,
                            perf.type = "misclass")

# compare 4 random Forest model based on confusion metrics

model_rf
modelrf_brf1
modelrf_brf2
varimp_rf <- vimp(model_rf)$importance
varimp_brf1 <- vimp(model_brf1)$importance
varimp_brf2 <- vimp(model_brf2)$importance

varimp_rf
varimp_brf1
varimp_brf2

# testing on test dataset
# logistic Regression

t_test_log$score2= predict(model2, t_test_log, type="response")
prediction_test<-ifelse(t_test_log$score2>=0.15,TRUE,FALSE)
caret::confusionMatrix(as.factor(prediction_test),as.factor(t_test_log$churn),positive ="TRUE")

t_test_prediction<-ROCR::prediction(predict(model2,newdata=t_test_log, type="response"), t_test_log$churn)
ROCR::performance(t_test_prediction, measure="tpr", x.measure="fpr")

# AUC Value on Test Dataset
ROCR::performance(t_test_prediction,measure="auc")
ROCR::plot(ROCR::performance(t_test_prediction, measure="tpr", x.measure="fpr"))

# random Forest

t_test_rf_pred <- predict(model_rf, t_test_rf, do.trace = TRUE)
t_test_brf1_pred <- predict(modelrf_brf1, t_test_rf, do.trace = TRUE)
t_test_brf2_pred <- predict(modelrf_brf2, t_test_rf, do.trace = TRUE)

t_test_rf_pred
t_test_brf1_pred
t_test_brf2_pred
