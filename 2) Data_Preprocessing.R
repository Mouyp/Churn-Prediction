# Step 2: Clean Data

# There are a total of 20000 observations with 14 variables in the dataset
nrow(energy_data)
ncol(energy_data)

# [2.1] Data Type / Format --------------------------------

#Clean Data Format

# Convert Gender to a factor 
energy_data$Gender <- factor(energy_data$Gender, levels = c(0, 1),
                             labels = c(0,1))

# Convert Email_list to a factor 
energy_data$Email_list <- factor(energy_data$Email_list, levels = c(0, 1),
                                 labels = c(0,1))

# Convert Churn to a factor 
energy_data$Churn <- factor(energy_data$Churn, levels = c(0, 1),
                            labels = c(0,1))

# Convert Start_channel to a factor
energy_data$Start_channel <- factor(energy_data$Start_channel, levels = c("Online", "Phone"), 
                                    labels = c(0,1))

# Convert Home_label to a factor 
energy_data$Home_label <- factor(energy_data$Home_label, levels = c("A", "B", "C", "D", "E", "F", "G"), 
                                 labels = c(1,2,3,4,5,6,7))

# Convert Province to a factor with unique integer labels
energy_data$Province <- factor(energy_data$Province, levels = unique(energy_data$Province),
                               labels = 1:length(unique(energy_data$Province)))

# Create 'PenaltyFee' and convert it to a factor

energy_data$PenaltyFee <- ifelse(energy_data$Contract_length > 0, 1, 0)
energy_data$PenaltyFee <- factor(energy_data$PenaltyFee, levels = c(0, 1),
                                 labels = c(0,1))


str(energy_data)
head(energy_data,10)

# [2.2] Check missing [NAs] -------------------------------

# There's no missing values

sum(is.na(energy_data))


# [2.3] Adjustment for outliers (based on visualization in 2.4) ------------------------------------------

# 1) Age

energy_data$Age <- ifelse(energy_data$Age >= 85, 85, energy_data$Age)
energy_data$LogAge <- log(energy_data$Age)

boxplot(energy_data$Age, main = "Boxplot for Age after Adjustment")
boxplot(energy_data$LogAge, main = "Boxplot for Log(Age) after Adjustment")



# 2) Income

energy_data$Income <- ifelse(energy_data$Income > 50000, energy_data$Income / 12, energy_data$Income)
energy_data$LogIncome <- log(energy_data$Income + 1) # Adding 1 to avoid log(0)

boxplot(energy_data$LogIncome, main = "Boxplot for Log(Income)", xlab = "Log(Income)")


# 3) Electricity & Gas Usage
energy_data$Gas_usage <- ifelse(energy_data$Gas_usage > 2900, 2900, energy_data$Gas_usage)
energy_data$Electricity_usage <- ifelse(energy_data$Electricity_usage > 6000, 6000, energy_data$Electricity_usage)

boxplot(energy_data$Gas_usage, main = "Boxplot for Gas Usage after Capping", ylab = "Gas Usage")
boxplot(energy_data$Electricity_usage, main = "Boxplot for Electricity Usage after Capping", ylab = "Electricity Usage")


clean_data <- dplyr::select(energy_data, -Age, -Income, -Customer_ID, -Contract_length)
head(clean_data,10)

# To make dataset suit with ML model, we will do some data engineering as follows:
# 1) One-hot encoding for categorical variables (with many levels)


# One-hot encoding 'Home_label' and 'Province' using model.matrix()

province_dummies <- model.matrix(~ Province - 1, data = clean_data)
homelabel_dummies <- model.matrix(~ Home_label -1, data = clean_data)

clean_data_encode <- cbind(clean_data, province_dummies, homelabel_dummies)
clean_data_encode <- clean_data_encode %>% dplyr::select(-Province, -Home_label)

head(clean_data_encode)



# 2) Standardize numeric variable to be on the same scale

numeric_vars <- c("LogIncome", "LogAge", "Electricity_usage", "Gas_usage",
                  "Relation_length", "Home_age") 

numeric_data <- clean_data_encode[, numeric_vars]
categorical_data <- clean_data_encode[, !(names(clean_data_encode) %in% numeric_vars)]

preProcValues <- preProcess(numeric_data, 
                            method = c("center", "scale"))
numeric_data_scaled <- predict(preProcValues, numeric_data)

clean_data_scaled_encode <- cbind(numeric_data_scaled, categorical_data)


# See Final 'Clean_data'
head(clean_data_scaled_encode)
summary(clean_data_scaled_encode)
str(clean_data_scaled_encode)


finalize_data <- clean_data_scaled_encode
nrow(finalize_data)
str(finalize_data)
