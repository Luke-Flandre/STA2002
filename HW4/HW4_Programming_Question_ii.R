# Program 1: Simulate iid Yi ~ Exp(λ) and compute a 90% CI for λ
set.seed(123)  # Set seed for reproducibility

# Parameters
lambda_true <- 2
n <- 100
alpha <- 0.10

# Simulate iid Yi ~ Exp(λ)
y <- rexp(n, rate = lambda_true)  # Rate is λ
y_bar <- mean(y)  # Sample mean

# Compute the 90% CI using the pivotal quantity method
chi2_low <- qchisq(alpha / 2, 2 * n)
chi2_high <- qchisq(1 - alpha / 2, 2 * n)

lower_bound <- 2 * n / (chi2_high * y_bar)
upper_bound <- 2 * n / (chi2_low * y_bar)

# Display the confidence interval
cat("90% CI for λ: [", lower_bound, ",", upper_bound, "]\n")

# Check if true λ is in the CI
if (lower_bound <= lambda_true && upper_bound >= lambda_true) {
  cat("The true value λ =", lambda_true, "is within the 90% CI.\n")
} else {
  cat("The true value λ =", lambda_true, "is NOT within the 90% CI.\n")
}