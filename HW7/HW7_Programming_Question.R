# Sample data
FVC_values <- c(3.4, 3.6, 3.8, 3.3, 3.4, 3.5, 3.7, 3.6, 3.7)
n <- length(FVC_values)  # Sample size
mu0 <- 3.4  # Hypothesized mean

# Calculate sample mean and standard deviation
sample_mean <- mean(FVC_values)
sample_sd <- sd(FVC_values)

# Calculate t-statistic
t_statistic <- (sample_mean - mu0) / (sample_sd / sqrt(n))

# Perform one-sample t-test
t_test <- t.test(FVC_values, mu = mu0, alternative = "greater")

# Output results
list(
  
  t_statistic = t_statistic,
  p_value = t_test$p.value,
  conclusion = ifelse(t_test$p.value < 0.05, "Reject H0", "Do not reject H0")
)