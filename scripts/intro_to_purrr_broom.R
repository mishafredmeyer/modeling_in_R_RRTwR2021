## This script was developed by Michael F Meyer (michael.f.meyer@wsu.edu)
## for the Reproducible Research Techniques with R workshop. 
## The goal of this script is to serve as a tutorial for using
## purrr and broom packages for running multiple models, but a 
## nested list structure. This tutorial was originally designed 
## by Nicholas Potter during the Fall 2020 Reproducible Research 
## Techniques with R workshop.
## The script can be broken up into the following sections: 
## 1. A primer on the broom package
## 2. Using purrr to build multiple models

# Load the packages and data
library(tidyverse)
library(broom)

bike_data <- read.csv("../data/day.csv",
                      header = TRUE)

# 1. A primer on the broom package ----------------------------------------

# Format some data
bike_data_cleaned <- bike_data %>%
  mutate(weather_type = factor(weathersit,
                               levels = c(1,2,3,4),
                               labels = c("Clear", "Cloudy", "Rainy", "Snowy")))

# Build the linear model
bike_model <- lm(cnt ~ season + workingday + weather_type + temp, data = bike_data_cleaned)

summary(bike_model)

# Extracting coefficients with the tidy function
tidy(bike_model)

# Extracting model performance with glance
glance(bike_model)

# Extract residuals
augment(bike_model)

# Plotting estimates from the model
ggplot(tidy(bike_model)) +
  geom_point(aes(x = term, y = estimate)) +
  geom_errorbar(aes(x = term,
                    ymin = estimate - std.error,
                    ymax = estimate + std.error))



# 2. Using purrr to build multiple models ---------------------------------

# Format the data
summary(bike_data_cleaned$temp)

bike_data_grouped <- bike_data_cleaned %>%
  mutate(temp_bin = cut(temp, 
                        breaks = c(0, seq(0.2, 0.9, by = 0.1)),
                        labels = seq(0.2, 0.9, by = 0.1),
                        right = FALSE))
bike_data_grouped %>%
  select(temp, temp_bin) %>%
  head()

# Create a nested dataframe
bike_data_purrr <- bike_data_grouped %>%
  nest(data = -temp_bin) %>%
  mutate(model = map(.x = data, 
                     .f = ~ lm(cnt ~ temp, data = .x)),
         tidy_model = map(.x = model, 
                          .f = ~ tidy(.x))) %>%
  unnest(tidy_model) %>%
  arrange(temp_bin)

# Plot the estimates with standard error
ggplot(data = bike_data_purrr %>% filter(term == "temp")) +
  geom_point(aes(x = temp_bin, y = estimate)) +
  geom_errorbar(aes(x = temp_bin,
                    ymin = estimate - std.error,
                    ymax = estimate + std.error)) +
  geom_hline(aes(yintercept = 0), linetype = "dotted", size = 2)

# Plotting regression segments 
ggplot(data = bike_data_grouped) +
  geom_point(aes(x = temp, y = cnt)) +
  geom_smooth(aes(x = temp, y = cnt, group = temp_bin), method = "lm")
