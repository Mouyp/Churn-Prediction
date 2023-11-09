# Part3: Visualization for Cleaning (part2) and exploratory analysis

## For numeric variables: use IQR & boxplot

# Function to find outliers

get_outliers <- function(x) {
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  IQR <- Q3 - Q1
  return((x < (Q1 - 1.5 * IQR)) | (x > (Q3 + 1.5 * IQR)))
}

# Apply function
numeric_cols <- sapply(energy_data, is.numeric)
outliers <- energy_data[, numeric_cols] %>% sapply(get_outliers)

numeric_column <- energy_data[,numeric_cols]

# 1) sum outliers in each column
sum_outliers <- colSums(outliers)
print(sum_outliers)


# 2) Inspect rows with outliers
rows_with_outliers <- energy_data[rowSums(outliers) > 0, ]
head(rows_with_outliers)


# Numeric: Visualization of outliers (Boxplots)

# Boxplot
boxplot(numeric_column)

# See Numerical variables related with 'Churn'

# 1) Income
ggplot(energy_data, aes(x = Income, fill = factor(Churn))) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("#00BFC4", "#F8766D")) +
  labs(title = "Conversion by Income",
       x = "Income",
       y = "density") +
  theme_minimal() +
  theme(legend.title = element_blank())


# 2) Age

ggplot(energy_data, aes(x = Age, fill = factor(Churn))) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("#00BFC4", "#F8766D")) +
  labs(title = "Conversion by Age",
       x = "Age",
       y = "density") +
  theme_minimal() +
  theme(legend.title = element_blank())


# 3) Relation_length

ggplot(energy_data, aes(x = Relation_length, fill = factor(Churn))) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("#00BFC4", "#F8766D")) +
  labs(title = "Conversion by Relation_length",
       x = "Relation_length",
       y = "density") +
  theme_minimal() +
  theme(legend.title = element_blank())


# 4) Contract_length -> no_outliers

ggplot(energy_data, aes(x = Contract_length, fill = factor(Churn))) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("#00BFC4", "#F8766D")) +
  labs(title = "Conversion by Contract_length",
       x = "Contract_length",
       y = "density") +
  theme_minimal() +
  theme(legend.title = element_blank())

# 5) Home_age

ggplot(energy_data, aes(x = Home_age, fill = factor(Churn))) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("#00BFC4", "#F8766D")) +
  labs(title = "Conversion by Home_age",
       x = "Home_age",
       y = "density") +
  theme_minimal() +
  theme(legend.title = element_blank())



# 6) Electricity_usage

ggplot(energy_data, aes(x = Electricity_usage, fill = factor(Churn))) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("#00BFC4", "#F8766D")) +
  labs(title = "Conversion by Electricity_usage",
       x = "Electricity_usage",
       y = "density") +
  theme_minimal() +
  theme(legend.title = element_blank())


# 7) Gas_usage

ggplot(energy_data, aes(x = Gas_usage, fill = factor(Churn))) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("#00BFC4", "#F8766D")) +
  labs(title = "Conversion by Gas_usage",
       x = "Gas_usage",
       y = "density") +
  theme_minimal() +
  theme(legend.title = element_blank())

# For categorical variables - see variables that underrepresented
# There are 5 main variables to inspect & their relationship with churn

# Select Categorical Column
cat_cols <- sapply(energy_data, is.factor)
cat_cols <- energy_data[,cat_cols]
cat_cols <- subset(cat_cols, select = -Province)
head(cat_cols,10)

# For categorical variables - see variables that underrepresented


# 1) Province_ID + Province_ID & Churn

# Explore 'univariate' (bar plot)
ggplot(energy_data, aes(x = Province_ID)) +
  geom_bar(fill = "blue") +
  theme_minimal() +
  labs(title = "Barplot of Categorical Data",
       x = "Province_ID",
       y = "Count")


# Explore relationship btw 'variable' & 'churn'
churn_prov <- table(energy_data$Churn, energy_data$Province_ID)
churnprov_tabel <- prop.table(churn_prov)
print(churnprov_tabel)

ggplot(energy_data, aes(x = Province, fill = Churn)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("#00BFC4", "#F8766D")) +
  labs(x = "Province", y = "Proportion", fill = "Churn Status") +
  theme_minimal()


# 2) Gender

# Explore 'univariate' (bar plot)
ggplot(energy_data, aes(x = Gender)) +
  geom_bar(fill = "blue") +
  theme_minimal() +
  labs(title = "Barplot of Gender",
       x = "Gender",
       y = "Count")

# Explore relationship btw 'variable' & 'churn'
Gender_prov <- table(energy_data$Churn, energy_data$Gender)
Genderprov_tabel <- prop.table(Gender_prov)
print(Genderprov_tabel)

ggplot(energy_data, aes(x = Gender, fill = as.factor(Churn))) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("#00BFC4", "#F8766D")) 



# 3) Start_channel

# Explore 'univariate' (bar plot)
ggplot(energy_data, aes(x = Start_channel)) +
  geom_bar(fill = "blue") +
  theme_minimal() +
  labs(title = "Barplot of Start_channel",
       x = "Start_channel",
       y = "Count")

# Explore relationship btw 'variable' & 'churn'
Start_channel_prov <- table(energy_data$Churn, energy_data$Start_channel)
Start_channelprov_tabel <- prop.table(Start_channel_prov)
print(Start_channelprov_tabel)

ggplot(energy_data, aes(x = Start_channel, fill = as.factor(Churn))) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("#00BFC4", "#F8766D")) 

# 4) Email_list

# Explore 'univariate' (bar plot)
ggplot(energy_data, aes(x = Email_list)) +
  geom_bar(fill = "blue") +
  theme_minimal() +
  labs(title = "Barplot of Email_list",
       x = "Email_list",
       y = "Count")

# Explore relationship btw 'variable' & 'churn'
Email_list_prov <- table(energy_data$Churn, energy_data$Email_list)
Email_listprov_tabel <- prop.table(Email_list_prov)
print(Email_listprov_tabel)

ggplot(energy_data, aes(x = Email_list, fill = as.factor(Churn))) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("#00BFC4", "#F8766D")) 

# 5) Home_label

# Explore 'univariate' (bar plot)
ggplot(energy_data, aes(x = Home_label)) +
  geom_bar(fill = "blue") +
  theme_minimal() +
  labs(title = "Barplot of Home_label",
       x = "Home_label",
       y = "Count")

# Explore relationship btw 'variable' & 'churn'
Home_label_prov <- table(energy_data$Churn, energy_data$Home_label)
Home_labelprov_tabel <- prop.table(Home_label_prov)
print(Home_labelprov_tabel)

ggplot(energy_data, aes(x = Home_label, fill = as.factor(Churn))) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("#00BFC4", "#F8766D")) 


