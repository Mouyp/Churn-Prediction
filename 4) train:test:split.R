
# Create Train-Test-Split Function and dataset

train_test_split <- function(data, train_size = 0.75) {
  set.seed(42)
  n <- nrow(data) # total sample
  id <- sample (1:n, size = n * train_size) 
  train_data <- data[id, ]
  test_data <- data[-id, ] 
  return(list(train_data, test_data))
}

# Split Data (75% estimated sample, 25% validation sample) ----

set.seed(42)
split_data <- train_test_split(finalize_data) 

estimate_data <- split_data[[1]] 
validate_data <- split_data[[2]]

# Check ( total sample should be 20,000 )
nrow(estimate_data); nrow(validate_data)
head(estimate_data);head(validate_data)
