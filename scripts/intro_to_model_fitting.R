## This script was developed by Michael F Meyer (michael.f.meyer@wsu.edu)
## for the Reproducible Research Techniques with R workshop. 
## The goal of this script is to serve as a tutorial for creating 
## training and test datasets to diagnose model performance. 
## The script can be broken up into the following sections: 
## 1. General workflow of creating a training and test dataset
## 2. Bootstrapping a training and test dataset
## 3. Big Challenge!!!

# Load the packages and data
library(tidyverse)

bike_data <- read.csv("../data/day.csv",
                      header = TRUE)

# 1. General workflow of creating a training and test dataset -------------

# Create a training dataset
training_bike_data <- bike_data %>%
  rownames_to_column() %>%
  rename("index" = "rowname") %>%
  mutate(feeling_temperature = atemp*50) %>%
  select( index, feeling_temperature, registered, workingday) %>%
  sample_frac(., size = 0.75, weight = as.factor(workingday))

# Create a test dataset
test_bike_data <- bike_data %>%
  rownames_to_column() %>%
  rename("index" = "rowname") %>%
  mutate(feeling_temperature = atemp*50) %>%
  select( index, feeling_temperature, registered, workingday) %>%
  filter(!(index %in% training_bike_data$index))

# Build a linear model on the training dataset
training_bike_model <- lm(registered ~ feeling_temperature, data = training_bike_data)
summary(test_bike_model)

# Predict values for the test dataset
test_bike_data$predicted_registered <- predict.lm(training_bike_model, 
                                                  newdata = test_bike_data)

# Calculate RMSE for predictions vs. observations
rmse <- test_bike_data %>%
  mutate(square_residual = (predicted_registered - registered)^2) %>%
  summarize(rmse = sqrt(sum(square_residual)/nrow(test_bike_data)))


# 2. Bootstrapping a training and test dataset ----------------------------

# Iterate through 1000 subsamplings, where each subsampling 
# uses a new training and test dataset and then calculates 
# RMSE from the resulting test dataset

nreps <- 1000
rmse_repo <- rep(NA, nreps)

for(i in 1:nreps){
  training_bike_data <- bike_data %>%
    rownames_to_column() %>%
    rename("index" = "rowname") %>%
    mutate(feeling_temperature = atemp*50) %>%
    select( index, feeling_temperature, registered, workingday) %>%
    sample_frac(., size = 0.75, weight = as.factor(workingday))
  
  test_bike_data <- bike_data %>%
    rownames_to_column() %>%
    rename("index" = "rowname") %>%
    mutate(feeling_temperature = atemp*50) %>%
    select( index, feeling_temperature, registered, workingday) %>%
    filter(!(index %in% training_bike_data$index))
  
  training_bike_model <- lm(registered ~ feeling_temperature, data = training_bike_data)
  summary(training_bike_model)
  
  test_bike_data$predicted_registered <- predict.lm(training_bike_model, 
                                                    newdata = test_bike_data)
  
  rmse_repo[i] <- test_bike_data %>%
    mutate(square_residual = (predicted_registered - registered)^2) %>%
    summarize(rmse = sqrt(sum(square_residual)/nrow(test_bike_data)))
}

# Assess the distribution of values
hist(unlist(rmse_repo))

# Create a dataset for the linear model using all data
linear_bike_data <- bike_data %>%
  rownames_to_column() %>%
  rename("index" = "rowname") %>%
  mutate(feeling_temperature = atemp*50) %>%
  select( index, feeling_temperature, registered, workingday) 

# Build the model
whole_linear_model <- lm(registered ~ feeling_temperature,
                         data = linear_bike_data)

# Predict values
linear_bike_data$predicted_registered <- predict(whole_linear_model)

# Calculate RMSE
rmse_whole_data <- linear_bike_data %>%
  mutate(square_residual = (predicted_registered - registered)^2) %>%
  summarize(rmse = sqrt(sum(square_residual)/nrow(linear_bike_data)))

unlist(rmse_whole_data)

# Plot the distribution and our observed RMSE with the whole data
ggplot() +
  geom_histogram(data = data.frame(rmse = unlist(rmse_repo)), aes(x = rmse)) +
  geom_vline(data = data.frame(rmse = unlist(rmse_whole_data)),
             aes(xintercept = rmse))



# 3. Big Challenge!!! -----------------------------------------------------

## Big Challenge: Using everything that we've learned, redo this analysis
## using logistic regression of workday ~ registered and assess using 
## model accuracy, where accurracy is defined as the number of true positives 
# and true negatives divided by the total number of runs. 

nreps <- 1000
accuracy_repo <- rep(NA, nreps)

for(i in 1:nreps){
  training_bike_data <- bike_data %>%
    rownames_to_column() %>%
    rename("index" = "rowname") %>%
    mutate(feeling_temperature = atemp*50) %>%
    select( index, feeling_temperature, registered, workingday) %>%
    sample_frac(., size = 0.75, weight = as.factor(workingday))
  
  test_bike_data <- bike_data %>%
    rownames_to_column() %>%
    rename("index" = "rowname") %>%
    mutate(feeling_temperature = atemp*50) %>%
    select( index, feeling_temperature, registered, workingday) %>%
    filter(!(index %in% training_bike_data$index))
  
  training_bike_model <- glm(workingday ~ registered, 
                            data = training_bike_data, 
                            family = binomial())
  summary(training_bike_model)
  
  test_bike_data$predicted_registered <- predict(training_bike_model,
                                                 newdata = test_bike_data,
                                                 type = "response")
  
  accuracy_repo[i] <- test_bike_data %>%
    mutate(predicted_registered = ifelse(predicted_registered >= 0.75,
                                         1, 0),
           categorical_result = ifelse(workingday == 1 & predicted_registered == 1,
                                       "TRUE_RESULT", NA),
           categorical_result = ifelse(workingday == 0 & predicted_registered == 0,
                                       "TRUE_RESULT", categorical_result),
           categorical_result = ifelse(workingday == 1 & predicted_registered == 0,
                                       "FALSE_RESULT", categorical_result),
           categorical_result = ifelse(workingday == 0 & predicted_registered == 1,
                                       "FALSE_RESULT", categorical_result)) %>%
    group_by(categorical_result) %>%
    count() %>%
    spread(categorical_result, n) %>%
    summarize(accuracy = TRUE_RESULT / sum(TRUE_RESULT + FALSE_RESULT))
}

hist(unlist(accuracy_repo))

logistic_bike_data <- bike_data %>%
  rownames_to_column() %>%
  rename("index" = "rowname") %>%
  mutate(feeling_temperature = atemp*50) %>%
  select( index, feeling_temperature, registered, workingday) 

whole_logistic_model <- lm(workingday ~ registered,
                         data = logistic_bike_data)

logistic_bike_data$predicted_registered <- predict(whole_logistic_model)

accuracy_whole <- logistic_bike_data %>%
  mutate(predicted_registered = ifelse(predicted_registered >= 0.75,
                                       1, 0),
         categorical_result = ifelse(workingday == 1 & predicted_registered == 1,
                                     "TRUE_RESULT", NA),
         categorical_result = ifelse(workingday == 0 & predicted_registered == 0,
                                     "TRUE_RESULT", categorical_result),
         categorical_result = ifelse(workingday == 1 & predicted_registered == 0,
                                     "FALSE_RESULT", categorical_result),
         categorical_result = ifelse(workingday == 0 & predicted_registered == 1,
                                     "FALSE_RESULT", categorical_result)) %>%
  group_by(categorical_result) %>%
  count() %>%
  spread(categorical_result, n) %>%
  summarize(accuracy = TRUE_RESULT / sum(TRUE_RESULT + FALSE_RESULT))

unlist(accuracy_whole)

ggplot() +
  geom_histogram(data = data.frame(accuracy = unlist(accuracy_repo)), 
                 aes(x = accuracy)) +
  geom_vline(data = data.frame(accuracy = unlist(accuracy_whole)),
             aes(xintercept = accuracy))
