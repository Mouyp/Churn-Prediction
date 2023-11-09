# [4.3] Decision Tree ---------------------------------------------------------


install.packages('rpart.plot')
library(rpart.plot)

# 4.4.1 CART Tree (GINI) --
## Create 'Tree' with all variables

Cart_tree_model <- rpart(Churn ~ .,
                         data = estimate_data)

Cart_tree_model_plot <- as.party(Cart_tree_model)
plot(Cart_tree_model_plot, type="simple", gp = gpar(fontsize = 10))

varImp(Cart_tree_model)

# Try 'Setting' for Tree

Cart_Control <- rpart.control(minsplit = 100, 
                              minbucket = 50, 
                              cp = 0.01, 
                              maxdepth = 3)


Cart_tree_model_control <- rpart(Churn ~ .,
                                 data = estimate_data,
                                 control = Cart_Control,
                                 method = "class") 


rpart.plot(Cart_tree_model_control, type=4, extra=101)


## Get 'Prediction' from CART

Cart_pred <- predict(Cart_tree_model, 
                     newdata = validate_data, 
                     type = "prob")

Cart_pred_value <- ifelse(Cart_pred[ ,2] > 0.5, 1, 0)

## Calculate Evaluation Metric

# 1) Hit Rate 

actual_churn <- validate_data$Churn


hitrate_Cart <- table(validate_data$Churn, Cart_pred_value,
                      dnn = c('Observed', 'Predicted'))

hitrate_Cart

# Cal hit rate

hitrate_Cart_model <- (hitrate_Cart[1,1] + 
                         hitrate_Cart[2,2])/sum(hitrate_Cart) 

hitrate_Cart_model

# 2) Top Decile Lift

decile_Cart <- ntile(Cart_pred[,2], 10)

decile_Cart_model <- table(actual_churn, decile_Cart, 
                           dnn = c("Churn", "Decile"))

decile_Cart_model

#Calculate the TDL

(decile_Cart_model[2,10] / (decile_Cart_model[1,10]+ decile_Cart_model[2,10])) / churn_rate


# 3) GINI

validate_data$Churn_numeric <- as.numeric(as.character(validate_data$Churn)) 


pred_model_CART <- prediction(Cart_pred[,2], validate_data$Churn_numeric) 
perf_model_CART <- performance(pred_model_CART,"tpr","fpr")

plot(perf_model_CART,xlab="Cumulative % of observations",
     ylab="Cumulative % of positive cases",
     xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
abline(0,1, col="red")

auc_model_CART <- performance(pred_model_CART,"auc")
as.numeric(auc_model_CART@y.values)*2-1


# 4.4.2 CHAID Tree -- (predictors = factor/ ordered factor only)

# Set up for CHAID

# install.packages("partykit")
# install.packages("CHAID", repos="http://R-Forge.R-project.org")
require(dplyr)
require(ggplot2)
theme_set(theme_bw()) # set theme
require(CHAID)
require(purrr)
require(caret)

# Create only dataframe with factor variables (no int with few levels that can make it as factor)

CHAID_df <- clean_data %>%
  dplyr::select(Gender, Start_channel, Email_list,Home_label,Churn, PenaltyFee)


# Split 
set.seed(1234)
split_data_CHAID <- train_test_split(CHAID_df) 

estimate_data_CHAID <- split_data_CHAID[[1]] 
validate_data_CHAID <- split_data_CHAID[[2]]

head(estimate_data_CHAID)
head(validate_data_CHAID)

# Fit Model (Full model)

CHAID_model <- chaid(Churn ~ ., data = estimate_data_CHAID)
print(CHAID_model)

# Try different visual
plot(CHAID_model)

plot(CHAID_model, type = "simple", gp = gpar(fontsize = 6))

plot(
  CHAID_model,
  gp = gpar(
    col = "blue",
    lty = "solid",
    lwd = 3,
    fontsize = 10
  )
)


# First result of tree = Penalty Fee : Test for significance
# p-value < 2.2e-16 (very significant)

chisq.test(validate_data_CHAID$Churn, validate_data_CHAID$PenaltyFee)

# Get Prediciton

CHAID_pred <- predict(CHAID_model, type = 'prob', newdata = validate_data_CHAID)

CHAID_pred_value <- ifelse(CHAID_pred[,2] > 0.5, 1, 0)

# VarImp
plot(sort(varimp(CHAID_model), decreasing = TRUE))
sort(varimp(CHAID_model), decreasing = TRUE)

# Evaluation

# 1) Hit Rate

hitrate_CHAID <- table(validate_data$Churn, CHAID_pred_value,
                       dnn = c('Observed', 'Predicted'))

hitrate_CHAID

# Cal hit rate

hitrate_CHAID_model <- (hitrate_CHAID[1,1] + 
                          hitrate_CHAID[2,2])/sum(hitrate_CHAID) 

hitrate_CHAID_model


# 2) Top Decile
decile_CHAID <- ntile(CHAID_pred, 10)

decile_CHAID_model <- table(validate_data_CHAID$Churn, decile_CHAID,
                            dnn= c("Observed", "Decile"))
decile_CHAID_model

#Calculate the TDL
(decile_CHAID_model[2,10] / (decile_CHAID_model[1,10]+ decile_CHAID_model[2,10])) / churn_rate


# Now: add 'numerical' variable by cutting them into group as factor

# Type1: cut_interval -> equal range of group

CHAID_new_df <- clean_data %>% 
  mutate_if(is.numeric, funs(cut_number(., n=5)))

str(CHAID_new_df)

set.seed(1234)
split_data_CHAID_new_df <- train_test_split(CHAID_new_df) 

estimate_data_CHAID_new_df <- split_data_CHAID_new_df[[1]] 
validate_data_CHAID_new_df <- split_data_CHAID_new_df[[2]]

# Fit model 

CHAID_model2 <- chaid(Churn ~ ., data = estimate_data_CHAID_new_df)
print(CHAID_model2)


plot(CHAID_model2)
varimp(CHAID_model2)

CHAID_pred2 <- predict(CHAID_model2, type = 'prob',
                       newdata = validate_data_CHAID_new_df)

CHAID_pred2_value <- ifelse(CHAID_pred2[,2] > 0.5, 1, 0)

plot(sort(varimp(CHAID_model2), decreasing = TRUE))
sort(varimp(CHAID_model2), decreasing = TRUE)


# Evaluation

# 1) Hit Rate

hitrate_CHAID2 <- table(validate_data$Churn, CHAID_pred2_value,
                        dnn = c('Observed', 'Predicted'))

hitrate_CHAID2

# Cal hit rate

hitrate_CHAID2_model <- (hitrate_CHAID2[1,1] + 
                           hitrate_CHAID2[2,2])/sum(hitrate_CHAID2) 

hitrate_CHAID2_model




# 2) Top Decile
decile_CHAID2 <- ntile(CHAID_pred2[,2], 10)

decile_CHAID_model2 <- table(validate_data_CHAID_new_df$Churn, decile_CHAID2,
                             dnn= c("Observed", "Decile"))
decile_CHAID_model2

#Calculate the TDL
(decile_CHAID_model2[2,10] / (decile_CHAID_model2[1,10]+ decile_CHAID_model2[2,10])) / churn_rate

# 3) GINI

pred_model_CHAID <- prediction(CHAID_pred2[,2], validate_data$Churn_numeric) 
perf_model_CHAID <- performance(pred_model_CHAID,"tpr","fpr")

plot(perf_model_CHAID,xlab="Cumulative % of observations",
     ylab="Cumulative % of positive cases",
     xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
abline(0,1, col="red")

auc_model_CHAID <- performance(pred_model_CHAID,"auc")
as.numeric(auc_model_CHAID@y.values)*2-1

# Additional:

# Type2: cut_number -> equal member in each group

# Type3: cut_width -> make group of specific width

# Extra: try 'control' CHAID (8 ctrl options)



# 4.4.3 Entrophy C50 --
install.packages('C50') 
library('C50')
graphics.off()

# Model1: Simple Tree Model - -

estimate_data_C50 <- estimate_data %>% dplyr::select(-Churn)


# Fitting 
entrophC50_model <- C5.0(x = estimate_data_C50, 
                         y = estimate_data$Churn) 


summary(entrophC50_model) 
plot(entrophC50_model, main = 'EntrophyC50')

# Prediction
entrophC50_pred <- predict(entrophC50_model, 
                           type = 'prob', newdata = validate_data) 

entrophC50_pred_value <- ifelse(entrophC50_pred[,2] > 0.5,1,0)

# Evaluation

# 1) Hit Rate 

actual_churn <- validate_data$Churn

hitrate_C50 <- table(validate_data$Churn, entrophC50_pred_value,
                     dnn = c('Observed', 'Predicted'))

hitrate_C50

# Cal hit rate

hitrate_C50 <- (hitrate_C50[1,1] + hitrate_C50[2,2])/sum(hitrate_C50) 

hitrate_C50


# 2) Top Decile
decile_C50 <- ntile(entrophC50_pred[,2], 10)

decile_C50 <- table(validate_data$Churn, decile_C50,
                    dnn= c("Observed", "Decile"))
decile_C50

#Calculate the TDL
(decile_C50[2,10] / (decile_C50[1,10]+ decile_C50[2,10])) / churn_rate

# 3) GINI

pred_model_C50 <- prediction(entrophC50_pred[,2], validate_data$Churn_numeric) 
perf_model_C50 <- performance(pred_model_C50,"tpr","fpr")

plot(perf_model_C50,xlab="Cumulative % of observations",
     ylab="Cumulative % of positive cases",
     xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
abline(0,1, col="red")

auc_model_C50 <- performance(pred_model_C50,"auc")
as.numeric(auc_model_C50@y.values)*2-1


# Model2: Rule-based Model - -
C50_rulebased_model <- C5.0(x = estimate_data_C50, 
                            y = estimate_data$Churn, rules = TRUE)

C50_rulebased_model


# Prediction
C50_rulebased_pred <- predict(C50_rulebased_model, 
                              type = 'prob', newdata = validate_data) 

C50_rulebased_pred_value <- ifelse(C50_rulebased_pred[,2] > 0.5, 1,0)

# Evaluation
# 1) Hit Rate 

actual_churn <- validate_data$Churn

hitrate_C50_rule <- table(validate_data$Churn, C50_rulebased_pred_value,
                          dnn = c('Observed', 'Predicted'))

hitrate_C50_rule

# Cal hit rate

hitrate_C50_rule <- (hitrate_C50_rule[1,1] + hitrate_C50_rule[2,2])/sum(hitrate_C50_rule) 

hitrate_C50_rule

# 2) Top Decile
decile_C50_rule <- ntile(C50_rulebased_pred[,2], 10)

decile_C50_rule <- table(validate_data$Churn, decile_C50_rule,
                         dnn= c("Observed", "Decile"))
decile_C50_rule

#Calculate the TDL
(decile_C50_rule[2,10] / (decile_C50_rule[1,10]+ decile_C50_rule[2,10])) / churn_rate

# 3) GINI

pred_model_C50_rule <- prediction(C50_rulebased_pred[,2], validate_data$Churn_numeric) 
perf_model_C50_rule <- performance(pred_model_C50_rule,"tpr","fpr")

plot(perf_model_C50_rule,xlab="Cumulative % of observations",
     ylab="Cumulative % of positive cases",
     xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
abline(0,1, col="red")

auc_model_C50_rule <- performance(pred_model_C50_rule,"auc")
as.numeric(auc_model_C50_rule@y.values)*2-1


# Model3: Boosted Tree Model- -

# Fitting
C50_boost_model <- C5.0(x = estimate_data_C50, 
                        y = estimate_data$Churn, trials = 10) 



plot(C50_boost_model)
summaryC50 <- summary(C50_boost_model)



# Prediction

C50_boost_pred <- predict(C50_boost_model, 
                          type = 'prob', newdata = validate_data) 

C50_boost_pred_value <- ifelse(C50_boost_pred[,2] > 0.5,1,0)

# Evaluation
# 1) Hit Rate 

hitrate_C50_boost <- table(validate_data$Churn, C50_boost_pred_value,
                           dnn = c('Observed', 'Predicted'))

hitrate_C50_boost

# Cal hit rate

hitrate_C50_boost <- (hitrate_C50_boost[1,1] + hitrate_C50_boost[2,2])/sum(hitrate_C50_boost) 

hitrate_C50_boost


# 2) Top Decile
decile_C50_boost <- ntile(C50_boost_pred[,2], 10)

decile_C50_boost <- table(validate_data$Churn, decile_C50_boost,
                          dnn= c("Observed", "Decile"))
decile_C50_boost

#Calculate the TDL
(decile_C50_boost[2,10] / (decile_C50_boost[1,10]+ decile_C50_boost[2,10])) / churn_rate

# 3) GINI

C50_boost_pred_numeric <- as.numeric(as.character(C50_boost_pred)) 

pred_model_C50_boost <- prediction(C50_boost_pred[,2] , validate_data$Churn_numeric) 
perf_model_C50_boost <- performance(pred_model_C50_boost,"tpr","fpr")

plot(perf_model_C50_boost,xlab="Cumulative % of observations",
     ylab="Cumulative % of positive cases",
     xlim=c(0,1),ylim=c(0,1),xaxs="i",yaxs="i")
abline(0,1, col="red")

auc_model_C50_boost <- performance(pred_model_C50_boost,"auc")
as.numeric(auc_model_C50_boost@y.values)*2-1

