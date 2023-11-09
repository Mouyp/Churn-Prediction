# [4.4] Bagging ---------------------------------------------------------

trainControl <- trainControl(method = "cv", number = 5)

# Fitting

Bagging_model <- train(Churn ~ ., 
                       data = estimate_data,
                       method = "treebag", 
                       nbagg = 100, 
                       trControl = trainControl)

print(Bagging_model)


# VarImportance

importance <- varImp(Bagging_model, scale = FALSE)
plot(importance, col = "blue", main = "Variable Importance")




# Prediction
bagging_pred <- predict(Bagging_model, type = 'prob', newdata = validate_data)

bagging_pred_value <- ifelse(bagging_pred[,2] > 0.5,1,0)

bagging_pred_varimp <- varImp(object =Bagging_model)
sorted_varimp <- bagging_pred_varimp$importance[order(bagging_pred_varimp$importance$Overall, decreasing = TRUE),]


# Evaluation

# 1) Hit Rate - - - -

hitrate_bagging <- table(validate_data$Churn, bagging_pred_value,
                         dnn = c('Observed', 'Predicted'))

hitrate_bagging

hitrate_bagging_model <- (hitrate_bagging[1,1] +
                            hitrate_bagging[2,2])/sum(hitrate_bagging)

hitrate_bagging_model


# 2) Top Decile - - - - 
decile_bagging <- ntile(bagging_pred[,2], 10)

decile_bagging_model <- table(validate_data$Churn, decile_bagging,
                              dnn= c("Observed", "Decile"))
decile_bagging_model

#Calculate the TDL

(decile_bagging_model[2,10] / (decile_bagging_model[1,10]+ decile_bagging_model[2,10])) / churn_rate


# 3) GINI  - - - -

pred_model_bagging <- prediction(bagging_pred[,2], validate_data$Churn) # use prob value (not class)
perf_model_bagging <- performance(pred_model_bagging,"tpr","fpr")

plot(perf_model_bagging,xlab="Cumulative % of observations",
     ylab="Cumulative % of positive cases",
     xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
abline(0,1, col="red")


auc_model_bagging <- performance(pred_model_bagging,"auc")
as.numeric(auc_model_bagging@y.values)*2-1



# [4.5] Boosting ---------------------------------------------------------
require(gbm)
require(MASS)
install.packages('xgboost')
install.packages('Matrix')
library(Matrix)
library(xgboost)
library(readr)
library(stringr)
library(caret)

# Data cleaning for XGBOOST -- (Convert all col into numeric)

finalized_data <- finalize_data %>% dplyr::select(-Churn_numeric)
head(finalized_data)

finalized_numeric <- finalized_data %>% dplyr:: select(-Gender, -Start_channel, -Email_list, -PenaltyFee)

Gender_dummy <- model.matrix(Churn ~ Gender -1, data = finalized_data)
Start_channel_dummy <- model.matrix(Churn ~ Start_channel -1, data = finalized_data)
Email_dummy <- model.matrix(Churn ~ Email_list -1, data = finalized_data)
PenaltyFee_dummy <- model.matrix(Churn ~ PenaltyFee -1, data = finalized_data)

finalized_ohe <- cbind(finalized_numeric,Gender_dummy,Start_channel_dummy,
                       Email_dummy,PenaltyFee_dummy )

head(finalized_ohe)                               


set.seed(42)
split_XGdata <- train_test_split(finalized_ohe) 

estimate_XGdata <- split_XGdata[[1]] 
validate_XGdata <- split_XGdata[[2]]

# Check ( total sample should be 20,000 )
nrow(estimate_XGdata); nrow(validate_XGdata)
head(estimate_XGdata);head(validate_XGdata)

X_vars <- estimate_XGdata %>% dplyr:: select(-Churn)
Y_var <- estimate_XGdata$Churn
Y_var <- as.numeric(as.character(Y_var))

# Fitting - -
set.seed(123) 

params <- list(
  objective = "binary:logistic",
  eta = 0.1,
  max_depth = 6,
  subsample = 0.8,
  colsample_bytree = 0.8,
  eval_metric = "logloss" 
)

nrounds <- 100

data_matrix <- xgb.DMatrix(data = as.matrix(X_vars), 
                           label = Y_var)

cv_results <- xgb.cv(
  params = params,
  data = data_matrix,
  nrounds = nrounds,
  nfold = 5,
  showsd = TRUE,
  stratified = TRUE,
  early_stopping_rounds = 10,
  maximize = FALSE
)

final_model <- xgb.train(
  params = params,
  data = data_matrix,
  nrounds = cv_results$best_iteration
)


# Prediction - -

X_test <- validate_XGdata %>% dplyr:: select(-Churn)
Y_test <- validate_XGdata$Churn


XGB_pred <- predict(final_model , data.matrix(X_test))
length(XGB_pred)

XGB_pred_value <- ifelse(XGB_pred>0.5,1,0)
head(XGB_pred_value)


# Evaluation - -

# 1) Hit Rate - - - -

hitrate_XGB <- table(validate_XGdata$Churn, XGB_pred_value,
                     dnn = c('Observed', 'Predicted'))

hitrate_XGB

hitrate_XGB_model <- (hitrate_XGB[1,1] +
                        hitrate_XGB[2,2])/sum(hitrate_XGB)

hitrate_XGB_model


# 2) Top Decile - - - - 
decile_XGB <- ntile(XGB_pred, 10)

decile_XGB_model <- table(validate_XGdata$Churn, decile_XGB,
                          dnn= c("Observed", "Decile"))
decile_XGB_model

#Calculate the TDL

(decile_XGB_model[2,10] / (decile_XGB_model[1,10]+ decile_XGB_model[2,10])) / churn_rate


# 3) GINI  - - - -

pred_model_XGB <- prediction(XGB_pred, validate_XGdata$Churn) 
perf_model_XGB <- performance(pred_model_XGB,"tpr","fpr")

plot(perf_model_XGB,xlab="Cumulative % of observations",
     ylab="Cumulative % of positive cases",
     xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
abline(0,1, col="red")


auc_model_XGB <- performance(pred_model_XGB,"auc")
as.numeric(auc_model_XGB@y.values)*2-1

# Visualization
importance_matrix <- xgb.importance(feature_names = colnames(data.matrix(as.matrix(X_vars))), 
                                    model = final_model)

print(importance_matrix)
xgb.plot.importance(importance_matrix, col = 'blue')

print(xtable(importance_matrix), type = "html")

