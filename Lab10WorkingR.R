####################################################################################################
# Lab 10

library(tidyverse)
####################################################################################################
# Task 1: Basic Simulation

# Use rbinom() to generate data
data_simulation_1 <- tibble(x=rbinom(10000, size = 1004, prob = 0.39)) /1004

# Plot a histogram of the data
ggplot(data = data_simulation_1, aes(x = x)) +
  geom_histogram(
    aes(y = ..density..),  
  ) +
  geom_density(
    color = "purple"
  ) +
  ggtitle("Data Simulation (Sample Size = 1004)")
  
# Shape is unimodal and centered around x=0.39 
# mean(data_simulation_1$x)


# What is the range of the middle 95%?

lower_bound <- quantile(data_simulation_1$x, 0.025) 
upper_bound <- quantile(data_simulation_1$x, 0.975) 
middle_range <- upper_bound - lower_bound

# Range of middle 95% is ~0.06

margin_of_error = middle_range/2

# Margin of error is ~0.03

# This margin of error of ~3% is better than Gallup's margin of error of 4%


###########################################################

# Task 1 Part 2: Repeat Simulation for Double Sample Size 

data_simulation_2 <- tibble(x=rbinom(10000, size = 1004*2, prob = 0.39)) /(1004*2)

# Plot a histogram of the data
ggplot(data = data_simulation_2, aes(x = x)) +
  geom_histogram(
    aes(y = ..density..),  
  ) +
  geom_density(
    color = "purple"
  ) + 
  ggtitle("Data Simulation (Sample Size = 2008)")


# Shape is unimodal and centered around x=0.78 
# mean(data_simulation_2$x)


# What is the range of the middle 95%?

lower_bound_2 <- quantile(data_simulation_2$x, 0.025) 
upper_bound_2 <- quantile(data_simulation_2$x, 0.975) 
middle_range_2 <- upper_bound_2 - lower_bound_2

# Range is ~0.06

margin_of_error_2 = middle_range_2/2

# Margin of error is ~2%

# This margin of error of ~2% is about equal to Gallup's margin of error of 2%



####################################################################################################
# Task 2: Resampling

# Create a df containing data from Gallup Survey

gallup_data <- tibble(
  response = c(rep(1, 319), rep(2, 592), rep(0, 20))
)

# Perform Resampling
R = 10000
for( i in 1:R){
# Take a resample
resample <- sample(x = gallup_data$response,
                  size = 1004,
                  replace = T) 
  
  resample_data <- tibble(resample) 
  
# Get Proportion
  proportion_of_1s <- mean(gallup_data$response == 1)
}



