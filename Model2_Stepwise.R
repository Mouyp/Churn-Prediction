# [4.2] Stepwise Regression ---------------------------------------------


Logistic_regression_full <- glm(Churn ~ ., 
                                data = estimate_data, 
                                family = binomial)

Logistic_regression_null <- glm(Churn ~ 0, 
                                data = estimate_data, 
                                family = binomial)

# AIC: Both - - - -

# Fitting
stepwise_model <- stepAIC(Logistic_regression_full, direction = "both", trace = FALSE)
print(stepwise_model)

# Prediction

stepwise_genAIC_pred <- predict(stepwise_model, type = 'response', newdata = validate_data)
stepwise_genAIC_pred_value <- ifelse(stepwise_genAIC_pred > 0.5, 1, 0)


summary_model <- summary(stepwise_model)
# Calculate Wald statistic
wald_stats <- summary_model$coefficients[, "Estimate"]^2 / summary_model$coefficients[, "Std. Error"]^2

# Create a data frame of variable importance
importance_df <- data.frame(
  Variable = rownames(summary_model$coefficients),
  WaldStatistic = wald_stats,
  Coefficient = summary_model$coefficients[, "Estimate"],
  StdError = summary_model$coefficients[, "Std. Error"],
  pValue = summary_model$coefficients[, "Pr(>|z|)"]
)

importance_df <- importance_df[order(-importance_df$WaldStatistic), ]
print(importance_df)



# 1) Hit Ratio 

hitrate_stepwise_genAIC <- table(validate_data$Churn, stepwise_genAIC_pred_value,
                                 dnn = c('Observed', 'Predicted'))

hitrate_stepwise_genAIC

# Cal hit rate

hitrate_stepwise_genAIC_model <- (hitrate_stepwise_genAIC[1,1] + 
                                    hitrate_stepwise_genAIC[2,2])/sum(hitrate_stepwise_genAIC) 

hitrate_stepwise_genAIC_model


# 2) Top Decile

decile_stepwise <- ntile(stepwise_genAIC_pred, 10)

decile_stepwise_model <- table(validate_data$Churn, decile_stepwise,
                               dnn= c("Observed", "Decile"))
decile_stepwise_model

#Calculate the TDL

(decile_stepwise_model[2,10] / (decile_stepwise_model[1,10]+ decile_stepwise_model[2,10])) /churn_rate

# 3) GINI

validate_data$Churn_numeric <- as.numeric(as.character(validate_data$Churn)) 

pred_model_CART <- prediction(stepwise_genAIC_pred, validate_data$Churn_numeric) 
perf_model_CART <- performance(pred_model_CART,"tpr","fpr")

plot(perf_model_CART,xlab="Cumulative % of observations",
     ylab="Cumulative % of positive cases",
     xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
abline(0,1, col="red")

auc_model_CART <- performance(pred_model_CART,"auc")
as.numeric(auc_model_CART@y.values)*2-1


# 2: AIC: Forward - - - -
Logistic_regression_forward_AIC <- stepAIC(Logistic_regression_null, 
                                           direction = "forward",
                                           scope = list(lower= Logistic_regression_null, 
                                                        upper=Logistic_regression_full), 
                                           trace = TRUE)

summary(Logistic_regression_forward_AIC)

# 3: BIC: Forward - - - -
Logistic_regression_forward_BIC <- stepAIC(Logistic_regression_null, 
                                           direction = "forward",
                                           scope = list(lower= Logistic_regression_null, 
                                                        upper=Logistic_regression_full),
                                           k = log(nrow(estimate_data)),
                                           trace = TRUE)

summary(Logistic_regression_forward_BIC)

# 4: AIC: Backward - - - -
Logistic_regression_backward_AIC <- stepAIC(Logistic_regression_full, 
                                            direction= "backward", trace = TRUE)

summary(Logistic_regression_backward_AIC)


# 5: BIC: Backward - - - -
Logistic_regression_backward_BIC <- stepAIC(Logistic_regression_full, 
                                            direction="backward", trace = TRUE, 
                                            k = log(nrow(estimate_data)))

summary(Logistic_regression_backward_BIC)


# 6: BIC: Both
Logistic_regression_both_BIC <- stepAIC(Logistic_regression_full, 
                                        direction="both", trace = TRUE, 
                                        k = log(nrow(estimate_data)))

summary(Logistic_regression_both_BIC)
