# Set seed for reproducibility
set.seed(707)

# Number of observations and simulations
n <- 100  # Sample size
num_simulations <- 200  # Number of simulations

# Initialize storage for estimates and p-values
beta0_estimates <- numeric(num_simulations)
beta1_estimates <- numeric(num_simulations)
p_values <- numeric(num_simulations)

# Simulation loop
for (i in 1:num_simulations) {
  # Generate data
  X <- rnorm(n, mean = 0, sd = 1)
  epsilon <- rnorm(n, mean = 0, sd = 1)
  Y <- 0.5 * X + 0.1 + epsilon
  
  # Estimate beta0 and beta1
  beta1_hat <- sum((X - mean(X)) * (Y - mean(Y))) / sum((X - mean(X))^2)
  beta0_hat <- mean(Y) - beta1_hat * mean(X)
  
  # Store estimates
  beta0_estimates[i] <- beta0_hat
  beta1_estimates[i] <- beta1_hat
  
  # Hypothesis testing for beta1
  beta1_var <- 1 / sum((X - mean(X))^2)  # Variance of beta1
  test_stat <- beta1_hat / sqrt(beta1_var)  # Test statistic
  p_values[i] <- pnorm(test_stat, lower.tail = TRUE)  # P-value for H0: beta1 = 0
}

# Display estimates from the first simulation
cat("Example estimates from the first simulation:\n")
cat("Estimated beta0:", beta0_estimates[1], "\n")
cat("Estimated beta1:", beta1_estimates[1], "\n")

# Plot histograms
par(mfrow = c(2, 2))  # Arrange plots in a 2x2 grid
hist(beta0_estimates, main = "Histogram of beta0", xlab = "beta0", col = "skyblue", border = "white")
hist(beta1_estimates, main = "Histogram of beta1", xlab = "beta1", col = "pink", border = "white")
hist(p_values, main = "Histogram of P-values", xlab = "P-value", col = "lightgreen", border = "white")

# Reset plot layout to default
par(mfrow = c(1, 1))