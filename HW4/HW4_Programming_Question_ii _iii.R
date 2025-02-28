# Set the seed for reproducibility
set.seed(123)

# Parameters
lambda_true <- 2
n <- 100

# Simulate 100 observations from Exp(lambda)
Y <- rexp(n, rate = lambda_true)

# Estimate lambda
lambda_hat <- 1 / mean(Y)

# 90% CI for lambda using pivotal quantity method
alpha <- 0.1
z <- qnorm(1 - alpha/2)
# CI for exponential distribution
lower_bound <- lambda_hat * (1 - z / sqrt(n))
upper_bound <- lambda_hat * (1 + z / sqrt(n))

cat("90% CI for lambda:", lower_bound, upper_bound, "\n")

# Check if true lambda is within the CI
if (lambda_true >= lower_bound & lambda_true <= upper_bound) {
  cat("The CI contains the true value of lambda.\n")
} else {
  cat("The CI does not contain the true value of lambda.\n")
}

# Number of repetitions
n_sim <- 1000
coverage <- 0

# Function to generate a single CI and check if it contains the true lambda
simulate_ci <- function(n, lambda_true, alpha) {
  Y <- rexp(n, rate = lambda_true)
  lambda_hat <- 1 / mean(Y)
  z <- qnorm(1 - alpha / 2)
  lower_bound <- lambda_hat * (1 - z / sqrt(n))
  upper_bound <- lambda_hat * (1 + z / sqrt(n))
  
  return(lambda_true >= lower_bound & lambda_true <= upper_bound)
}

# Run the experiment 1000 times
for (i in 1:n_sim) {
  coverage <- coverage + simulate_ci(n, lambda_true, alpha)
}

# Calculate coverage rate
coverage_rate <- coverage / n_sim
cat("Coverage rate of 90% CI:", coverage_rate, "\n")