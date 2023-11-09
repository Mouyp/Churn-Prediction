# [4.7] Support Vector Machine (SVM) -------------------------

library(e1071)
#based on the other models, the following variables are chosen
# (LogIncome + PenaltyFee + Relation_length + Electricity_usage + Gas_usage)

# Fitting

# - - - -
SVM_model1 <- svm(Churn ~ LogIncome + PenaltyFee0 + PenaltyFee1 + Relation_length + 
                    Electricity_usage + Gas_usage , 
                  data = estimate_data,
                  type = 'C-classification', 
                  probability = TRUE,
                  kernel = 'linear') 

print(summary(SVM_model1))

# - - - -

SVM_model2 <- svm(Churn ~ LogIncome + PenaltyFee + Relation_length + 
                    Electricity_usage + Gas_usage , 
                  data = estimate_data,
                  type = 'C-classification', 
                  probability = TRUE,
                  kernel = 'polynomial') 

print(summary(SVM_model2))



# - - - -

SVM_model3 <- svm(Churn ~ LogIncome + PenaltyFee + Relation_length + 
                    Electricity_usage + Gas_usage , 
                  data = estimate_data,
                  type = 'C-classification', 
                  probability = TRUE,
                  kernel = 'radial') 

print(summary(SVM_model3))




# - - - -

SVM_model4 <- svm(Churn ~ LogIncome + PenaltyFee + Relation_length + 
                    Electricity_usage + Gas_usage , 
                  data = estimate_data,
                  type = 'C-classification', 
                  probability = TRUE,
                  kernel = 'sigmoid') 

print(summary(SVM_model4))



# Prediction - - - -

SVM_model1_pred <- predict(SVM_model1, 
                           newdata= validate_data, 
                           type = 'prob',
                           probability=TRUE) 

SVM_model1_pred <- attr(SVM_model1_pred,"probabilities")[,2]
SVM_model1_pred_value <- ifelse(SVM_model1_pred>0.5,1,0)


# - - - -

SVM_model2_pred <- predict(SVM_model2, 
                           newdata= validate_data, 
                           type = 'prob',
                           probability=TRUE) 

SVM_model2_pred <- attr(SVM_model2_pred,"probabilities")[,2]
SVM_model2_pred_value <- ifelse(SVM_model2_pred>0.5,1,0)



# - - - -


SVM_model3_pred <- predict(SVM_model3, 
                           newdata= validate_data, 
                           type = 'prob',
                           probability=TRUE) 

SVM_model3_pred <- attr(SVM_model3_pred,"probabilities")[,2]
SVM_model3_pred_value <- ifelse(SVM_model3_pred>0.5,1,0)



# - - - -

SVM_model4_pred <- predict(SVM_model4, 
                           newdata= validate_data, 
                           type = 'prob',
                           probability=TRUE) 

SVM_model4_pred <- attr(SVM_model4_pred,"probabilities")[,2]
SVM_model4_pred_value <- ifelse(SVM_model4_pred>0.5,1,0)

# Evaluation - - - -

# 1) Hit Rate - - - -
hitrate_SVM1 <- table(validate_data$Churn, SVM_model1_pred_value,
                      dnn = c('Observed', 'Predicted'))
hitrate_SVM1

hitrate_SVM1_model <- (hitrate_SVM1[1,1] + hitrate_SVM1[2,2])/sum(hitrate_SVM1)
hitrate_SVM1_model

# 2) Top Decile - - - -
decile_SVM1 <- ntile(SVM_model1_pred, 10)
decile_SVM1_model <- table(validate_data$Churn, decile_SVM1,
                           dnn= c("Observed", "Decile"))
decile_SVM1_model

#Calculate the TDL
(decile_SVM1_model[2,10] / (decile_SVM1_model[1,10]+ decile_SVM1_model[2,10])) / churn_rate

# 3) GINI - - - -
pred_SVM_model1 <- prediction(SVM_model1_pred, validate_data$Churn) # use prob value (not class)
perf_SVM_model1 <- performance(pred_SVM_model1 ,"tpr","fpr")

plot(perf_SVM_model1,xlab="Cumulative % of observations",
     ylab="Cumulative % of positive cases",
     xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
abline(0,1, col="red")

auc_model_SVM1 <- performance(pred_SVM_model1,"auc")
as.numeric(auc_model_SVM1@y.values)*2-1

# - - - -

# 1) Hit Rate - - - -
hitrate_SVM2 <- table(validate_data$Churn, SVM_model2_pred_value,
                      dnn = c('Observed', 'Predicted'))
hitrate_SVM2

hitrate_SVM2_model <- (hitrate_SVM2[1,1] + hitrate_SVM2[2,2])/sum(hitrate_SVM2)
hitrate_SVM2_model

# 2) Top Decile - - - -
decile_SVM2 <- ntile(SVM_model2_pred, 10)
decile_SVM2_model <- table(validate_data$Churn, decile_SVM2,
                           dnn= c("Observed", "Decile"))
decile_SVM2_model

#Calculate the TDL
(decile_SVM2_model[2,10] / (decile_SVM2_model[1,10]+ decile_SVM2_model[2,10])) / churn_rate

# 3) GINI - - - -
pred_SVM_model2 <- prediction(SVM_model2_pred, validate_data$Churn) # use prob value (not class)
perf_SVM_model2 <- performance(pred_SVM_model2 ,"tpr","fpr")

plot(perf_SVM_model2,xlab="Cumulative % of observations",
     ylab="Cumulative % of positive cases",
     xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
abline(0,1, col="red")

auc_model_SVM2 <- performance(pred_SVM_model2,"auc")
as.numeric(auc_model_SVM2@y.values)*2-1

# - - - -

# 1) Hit Rate - - - -
hitrate_SVM3 <- table(validate_data$Churn, SVM_model3_pred_value,
                      dnn = c('Observed', 'Predicted'))
hitrate_SVM3

hitrate_SVM3_model <- (hitrate_SVM3[1,1] + hitrate_SVM3[2,2])/sum(hitrate_SVM3)
hitrate_SVM3_model

# 2) Top Decile - - - -
decile_SVM3 <- ntile(SVM_model3_pred, 10)
decile_SVM3_model <- table(validate_data$Churn, decile_SVM3,
                           dnn= c("Observed", "Decile"))
decile_SVM3_model

#Calculate the TDL
(decile_SVM3_model[2,10] / (decile_SVM3_model[1,10]+ decile_SVM3_model[2,10])) / churn_rate

# 3) GINI - - - -
pred_SVM_model3 <- prediction(SVM_model3_pred, validate_data$Churn) # use prob value (not class)
perf_SVM_model3 <- performance(pred_SVM_model3 ,"tpr","fpr")

plot(perf_SVM_model3,xlab="Cumulative % of observations",
     ylab="Cumulative % of positive cases",
     xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
abline(0,1, col="red")

auc_model_SVM3 <- performance(pred_SVM_model3,"auc")
as.numeric(auc_model_SVM3@y.values)*2-1


# - - - -

# 1) Hit Rate - - - -
hitrate_SVM4 <- table(validate_data$Churn, SVM_model4_pred_value,
                      dnn = c('Observed', 'Predicted'))
hitrate_SVM4

hitrate_SVM4_model <- (hitrate_SVM4[1,1] + hitrate_SVM4[2,2])/sum(hitrate_SVM4)
hitrate_SVM4_model

# 2) Top Decile - - - -
decile_SVM4 <- ntile(SVM_model4_pred, 10)
decile_SVM4_model <- table(validate_data$Churn, decile_SVM4,
                           dnn= c("Observed", "Decile"))
decile_SVM4_model

#Calculate the TDL
(decile_SVM4_model[2,10] / (decile_SVM4_model[1,10]+ decile_SVM4_model[2,10])) / churn_rate

# 3) GINI - - - -
pred_SVM_model4 <- prediction(SVM_model4_pred, validate_data$Churn) # use prob value (not class)
perf_SVM_model4 <- performance(pred_SVM_model4 ,"tpr","fpr")

plot(perf_SVM_model4,xlab="Cumulative % of observations",
     ylab="Cumulative % of positive cases",
     xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
abline(0,1, col="red")

auc_model_SVM4 <- performance(pred_SVM_model4,"auc")
as.numeric(auc_model_SVM4@y.values)*2-1
