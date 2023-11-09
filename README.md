# Churn-Prediction

The project aims to develop a 'predictive model' for customer churn for an energy supplier.

Churn prediction is crucial as it can help the company identify at-risk customers and implement strategies to improve customer retention, ultimately leading to better customer satisfaction and increased profitability.

The task is to go through the model development process, starting from defining the business problem to the model implementation recommendation.

## Outline
Part 1: Defining the business problem and research design

- Management Dilemma

- Management Question

- Research Question

- Sub - Research Questions


Part 2: Data preprocessing

- Data Cleaning: Address missing values, outliers, and inconsistencies

- Data Transformation: Standardization or normalization of variables, encoding categorical variables, and engineering new features if necessary.


Part 3: Exploratory data analysis (EDA)

- Summary Statistics

- Univariate Analysis

- Multivariate analysis

- Initial Hypothesis Testing


Part 4: Modeling

- Model Specification

- Model Estimation

- Model Validation


Part 5: Results, Conclusions, and Managerial Implications

- Key Findings

- Recommendations

## Data Description
The data consists of 20,000 customer records, with an even split between churned and retained customers. The dataset includes 14 variables potentially influencing churns, such as demographic data, contract details, energy usage, and other customer-related information.

Customer_ID: a unique customer identification number.

Gender: a dummy variable indicating if the customer who signed the contract is male (0) or female (1).

Age: the age of the customer in years.

Income: the monthly income of the customer’s household in dollars.

Relation_length: the amount of months the customer has been with the firm.

Contract_length: the amount of months the customer still has a contract with the firm. (0) means the customer has a flexible contract, i.e., (s)he can leave anytime without paying a fine. If the contract is more than zero months, the customer can still leave, but has to pay a fine when leaving.

Start_channel: indicating if the contract was filled out by the customer on the firm’s website (“Online”) or by calling up the firm (“Phone”).

Email_list: indicating if the customer’s email address is known by the firm (1=yes, 0=no).

Home_age: the age of the home of the customer in years.

Home_label: energy label of the home of the customer, ranging from A (good) to G (bad).

Electricity_usage: the yearly electricity usage in kWh.

Gas_usage: the yearly gas usage in cubic meters.

Province: the province where the customer is living.

Churn: a dummy variable indicating if the customer has churned (1) or not (0).
