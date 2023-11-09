# Package

install.packages("rmarkdown")
library(rmarkdown)

library(dplyr)
library(tidyverse)
library(tidyr)
library(corrplot)
library(gmodels) 
library(caret) 
library(ggplot2) 
library(gbm) 
library(ipred) 
library(neuralnet) 
library(ROCR) 
library(rpart) 
library(randomForest)
require(CHAID) 
require(purrr) 
library(MASS)
library(pROC)
library(ModelMetrics)

# Data

energy_data <- read.csv('energychurn_data.csv')

print('Overview of Data')
str(energy_data)