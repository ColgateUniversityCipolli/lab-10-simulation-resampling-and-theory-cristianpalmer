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

# What do we notice about the shape?
# Shape is unimodal and centered around x=0.39 
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
  response = c(rep(1, 392), rep(2, 592), rep(0, 20))
)

# Perform Resampling
R = 10000
resamples <- tibble(p.hat = numeric(R))

for( i in 1:R){
  # Take a resample
  resample <- sample(x = gallup_data$response,
                     size = 1004,
                     replace = T) 
  resamples$p.hat[i] <- mean(resample==1)
}

# Plot a histogram of the resampling data
ggplot(data = resamples, aes(x = p.hat)) +
  geom_histogram(
    aes(y = ..density..),  
  ) +
  geom_density(
    color = "purple"
  ) + 
  ggtitle("Resampling Data")

# What do we notice about the shape?
# Shape is unimodal and centered around x=0.39

# What is the range of the middle 95%?
lower_boundR <- quantile(resamples$p.hat, 0.025) 
upper_boundR <- quantile(resamples$p.hat, 0.975) 
middle_rangeR <- upper_boundR - lower_boundR
# Range of middle 95% is ~0.06

# What is the margin of error?
margin_of_error_R = middle_rangeR/2
# Margin of error is ~3%
# This margin of error of ~3% is better than Gallup's margin of error of 4%

####################################################################################################
# Task 3: Simulation over n and p

###########################################################

# Simulation over n

n <- seq(from = 100, to = 3000, by = 10)
margin_errors <- numeric(length(n))

for (i in 1:length(n)) {
  current_n <- n[i]
  # Generate 10000 simulations for current sample size
  simulations <- rbinom(10000, size = current_n, prob = 0.39) / current_n
  
  # Calculate percentiles
  lower_bound <- quantile(simulations, 0.025)
  upper_bound <- quantile(simulations, 0.975)
  
  # Store half the range (margin of error)
  margin_errors[i] <- (upper_bound - lower_bound) / 2
}

# Create a dataframe with results
simulation_results_n <- tibble(
  n = n,
  margin_of_error = margin_errors
)

###########################################################
# Simulation over p
p <- seq(from = 0.01, to = 0.99, by = 0.01)
margin_errors <- numeric(length(p))

for (i in 1:length(p)) {
  current_p <- p[i]
  # Generate 10000 simulations for current probability
  simulations <- rbinom(10000, size = 1004, prob = current_p) / 1004
  
  # Calculate percentiles
  lower_bound <- quantile(simulations, 0.025)
  upper_bound <- quantile(simulations, 0.975)
  
  # Store half the range (margin of error)
  margin_errors[i] <- (upper_bound - lower_bound) / 2
}

# Create a dataframe with results
simulation_results_p <- tibble(
  p = p,
  margin_of_error = margin_errors
)

###########################################################

# Create a geom_raster() plot

