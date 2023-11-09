# [4.6] Random Forest -------------------------

# Fitiing
RandomForest_model <- randomForest(Churn ~ ., 
                                   data= estimate_data, 
                                   importance=TRUE)

# Prediction
RandomForest_pred <- predict(RandomForest_model, 
                             newdata= validate_data, 
                             type ="prob")

RandomForest_pred_value <- ifelse(RandomForest_pred > 0.5,1,0)
RandomForest_pred_value <- RandomForest_pred_value[,2]

varImpPlot(RandomForest_model)


# 1) Hit Rate - - - -

hitrate_RF <- table(validate_data$Churn, RandomForest_pred_value,
                    dnn = c('Observed', 'Predicted'))

hitrate_RF

hitrate_RF_model <- (hitrate_RF[1,1] +
                       hitrate_RF[2,2])/sum(hitrate_RF)

hitrate_RF_model


# 2) Top Decile - - - - 
decile_RF <- ntile(RandomForest_pred[,2], 10)

decile_RF_model <- table(validate_data$Churn, decile_RF,
                         dnn= c("Observed", "Decile"))
decile_RF_model

#Calculate the TDL

(decile_RF_model[2,10] / (decile_RF_model[1,10]+ decile_RF_model[2,10])) / churn_rate


# 3) GINI  - - - -

pred_model_RF <- prediction(RandomForest_pred[,2], validate_data$Churn) # use prob value (not class)
perf_model_RF <- performance(pred_model_RF,"tpr","fpr")

plot(perf_model_RF,xlab="Cumulative % of observations",
     ylab="Cumulative % of positive cases",
     xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
abline(0,1, col="red")


auc_model_RF <- performance(pred_model_RF,"auc")
as.numeric(auc_model_RF@y.values)*2-1


# Extra: Try Some extra setting for RF
Random_forest1 <- randomForest(as.factor(survived) ~ ., 
                               data=Titanic,
                               ntree=500, mtry=3, 
                               nodesize=1, 
                               maxnodes=100, 
                               importance=TRUE)

estimate_data$Churn

