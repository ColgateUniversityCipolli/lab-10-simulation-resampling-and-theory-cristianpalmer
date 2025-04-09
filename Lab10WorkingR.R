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

# Simulation over n and p
n <- seq(from = 100, to = 3000, by = 10)
p <- seq(from = 0.01, to = 0.99, by = 0.01)

# Create initial tibble
results_tibble <- tibble(
  n = integer(),
  p = numeric(),
  margin_error = numeric()
)

# Run simulations and save results
for (i in 1:length(n)) {
  for (j in 1:length(p)) {
    
    current_n <- n[i]
    current_p <- p[j]
    
    # Generate 10000 simulations for current sample size
    simulations <- rbinom(10000, size = current_n, prob = current_p) / current_n
    
    # Calculate percentiles
    lower_bound <- quantile(simulations, 0.025)
    upper_bound <- quantile(simulations, 0.975)
    
    # Calculate margin of error
    current_margin_error <- (upper_bound - lower_bound) / 2
    
    # Add row to the tibble
    results_tibble <- results_tibble %>%
      add_row(
        n = current_n,
        p = current_p,
        margin_error = current_margin_error
      )
  }
}

###########################################################
# Create a geom_raster() plot

raster_simulation <- ggplot(results_tibble, aes(x = p, y = n, fill = margin_error)) +
  geom_raster(interpolate = TRUE) +
  scale_fill_viridis_c("Margin of Error", limits=c(0, 0.11))+
  scale_y_continuous(expand = c(0, 0), breaks = seq(500, 3000, 500)) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 1, 0.1)) +
  labs(
    title = "Margin of Error for Different Sample Sizes and Probabilities",
    subtitle = "Based on 10,000 binomial simulations per parameter combination",
    x = "Probability (p)",
    y = "Sample Size (n)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    axis.text = element_text(size = 9)
  )

####################################################################################################
# Task 4: Actual Margin of Error Calculation
n <- seq(from = 100, to = 3000, by = 10)
p <- seq(from = 0.01, to = 0.99, by = 0.01)
z <- qnorm(0.975)  # z-score for 95% confidence

# Create initial tibble
results_tibble_math <- tibble(
  n = integer(),
  p = numeric(),
  margin_error = numeric()  # Using consistent column name
)

# Run calculations and save results
for (i in 1:length(n)) {
  for (j in 1:length(p)) {
    
    current_n <- n[i]
    current_p <- p[j]
    
    # Calculate Wilson margin of error
    numerator <- z * sqrt((current_n * current_p * (1-current_p)) + (z^2)/4)
    denominator <- current_n + (z^2)
    Wilson_MOE <- numerator / denominator
    
    # Add row to the tibble
    results_tibble_math <- results_tibble_math %>%
      add_row(
        n = current_n,
        p = current_p,
        margin_error = Wilson_MOE  
      )
  }
}

###########################################################
# Create a geom_raster() plot using the Wilson margin of error calculations

raster_math <- ggplot(results_tibble_math, aes(x = p, y = n, fill = margin_error)) +
  geom_raster(interpolate = TRUE) +
  scale_fill_viridis_c("Margin of Error", limits=c(0, 0.11))+
  scale_y_continuous(expand = c(0, 0), breaks = seq(500, 3000, 500)) +
  scale_x_continuous(expand = c(0, 0), breaks = seq(0, 1, 0.1)) +
  labs(
    title = "Wilson Margin of Error for Different Sample Sizes and Proportions",
    subtitle = "95% Confidence Level",
    x = "Proportion (p)",
    y = "Sample Size (n)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    axis.text = element_text(size = 9)
  )

# Combine plots
library(patchwork)
combined_plot <- (raster_simulation + raster_math)
combined_plot

# Checking work

(mean(results_tibble$margin_error))
(mean(results_tibble_math$margin_error))