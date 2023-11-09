# Setup Train Control

train_control <- trainControl(
  method = "cv", 
  number = 5
)



# Create 'Churn Rate' used in Evaluation Part (churn rate = 0.4901)

finalize_data$Churn_numeric <- as.numeric(as.character(finalize_data$Churn))
churn_rate <- mean(finalize_data$Churn_numeric)
churn_rate


# [4.1] Based Model (Logistic Regression) ---------------------------------------------

# Fitting - - - -
logistic_model <- train(Churn ~ PenaltyFee + Electricity_usage + Relation_length + Gas_usage + LogIncome, 
                        method = 'glm',
                        family = 'binomial',
                        trControl = train_control,
                        data = estimate_data)

summary(logistic_model)

# Prediction - - - -

logistic_pred = predict(logistic_model,
                        type = 'prob',
                        newdata = validate_data)

logistic_pred_positive <- logistic_pred[, "1"]
logistic_pred_value <- if_else(logistic_pred_positive > 0.5, 1, 0)


# Evaluation - - - -

# 1) Hit Ratio (71.74 %)

hitrate_logistic <- table(validate_data$Churn, logistic_pred_value,
                          dnn = c('Observed', 'Predicted'))

hitrate_logistic

# Cal hit rate

hitrate_logistic_model <- (hitrate_logistic[1,1] + 
                             hitrate_logistic[2,2])/sum(hitrate_logistic) 

hitrate_logistic_model


# 2) Top Decile Lift (0.60)

decile_logistic <- ntile(logistic_pred[,2], 10)

decile_logistic_model <- table(validate_data$Churn, decile_logistic, 
                               dnn= c("Observed", "Decile"))
decile_logistic_model

#Calculate the TDL ( base 1)

(decile_logistic_model[2,10] / (decile_logistic_model[1,10]+ decile_logistic_model[2,10]))/churn_rate


# 3) GINI coefficient (0.6228)

pred_logis_model <- prediction(logistic_pred[,2], validate_data$Churn) 
perf_logis_model <- performance(pred_logis_model ,"tpr","fpr") 

plot(perf_logis_model,xlab = "Cumulative % of observations",
     ylab= "Cumulative % of Churn",
     xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
abline(0,1, col="red")


auc_logis_model <- performance(pred_logis_model,"auc")

gini_logistic <- as.numeric(auc_logis_model@y.values)*2-1
gini_logistic

