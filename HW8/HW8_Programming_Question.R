# Set seed for reproducibility
set.seed(1)

# Define the given parameters
beta <- 0
sigma <- 2
xi <- c(3.4, 3.6, 3.8, 3.3, 3.4, 3.5, 3.7, 3.6, 3.7)
n <- length(xi)

# Generate synthetic data Yi ~ N(beta * xi, sigma^2)
Yi <- rnorm(n, mean = beta * xi, sd = sigma)

# (1) Likelihood Ratio Test for H0: beta = 0 vs H1: beta != 0

# Define the log-likelihood function for beta
log_likelihood <- function(beta) {
  -n/2 * log(2 * pi * sigma^2) - sum((Yi - beta * xi)^2) / (2 * sigma^2)
}

# Estimate beta under H0 (beta = 0) and under H1 (maximize likelihood for beta)
beta_H0 <- 0
beta_H1 <- sum(Yi * xi) / sum(xi^2)

# Calculate log-likelihoods
logL_H0 <- log_likelihood(beta_H0)
logL_H1 <- log_likelihood(beta_H1)

# Likelihood ratio statistic
LRT <- 2 * (logL_H1 - logL_H0)

# Calculate the p-value using chi-square distribution with 1 degree of freedom
p_value <- 1 - pchisq(LRT, df = 1)

# (2) Conclusion
cat("Likelihood Ratio Test statistic:", LRT, "\n")
cat("p-value:", p_value, "\n")

if (p_value < 0.05) {
  cat("Conclusion: Reject H0. There is evidence to suggest that beta is not equal to 0.\n")
} else {
  cat("Conclusion: Fail to reject H0. There is insufficient evidence to suggest that beta is not equal to 0.\n")
}