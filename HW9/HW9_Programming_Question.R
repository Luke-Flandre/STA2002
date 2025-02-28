# Set seed for reproducibility
set.seed(1999)

# Parameters
mu <- 1.0
mu0 <- 1.0
sigma2 <- 2
n <- 100
sigma <- sqrt(sigma2)

# Function to calculate p-value
calculate_p_value <- function(sample_mean, mu0, sigma, n) {
  z <- (sample_mean - mu0) / (sigma / sqrt(n))
  p_value <- 1 - pnorm(z)
  return(p_value)
}

# Part ii: Generate data and calculate p-values for H0: µ = µ0 = 1.0
p_values <- replicate(500, {
  sample_data <- rnorm(n, mean = mu, sd = sigma)
  sample_mean <- mean(sample_data)
  calculate_p_value(sample_mean, mu0, sigma, n)
})

# Part iii: Generate data and calculate p-values for H0: µ0 = -1.0
mu0 <- -1.0

p_values_alt <- replicate(500, {
  sample_data <- rnorm(n, mean = mu, sd = sigma)
  sample_mean <- mean(sample_data)
  calculate_p_value(sample_mean, mu0, sigma, n)
})

# Plot both histograms
par(mfrow=c(1,2)) # Set up the graphics layout to display 2 plots side by side

# Plot histogram of p-values under H0: µ = µ0 = 1.0
hist(p_values, main = "Histogram of p-values under H0: µ = µ0 = 1.0", xlab = "p-value", breaks = 20)


# Plot histogram of p-values under H1: µ > µ0 = -1.0, focusing on p-values close to 0
hist(p_values_alt, main = "Histogram of p-values under H1: µ > µ0 = -1.0", xlab = "p-value", breaks = seq(0, 1, by = 0.01), xlim = c(-1, 1))