# Set seed for reproducibility
set.seed(114514)

# Parameters
n <- 15 # sample size
sigma2 <- 1 # known variance
alpha <- 0.05 # significance level
z_alpha <- qnorm(1 - alpha) # critical value for alpha = 0.05
num_experiments <- 200 # number of repetitions

# Step i: Generate X1,..., Xn from N(0, 1) and Step ii: Perform hypothesis test
reject_H0_N0 <- 0 # Counter for rejections under N(0, 1)

for (i in 1:num_experiments) {
  # Generate n samples from N(0, 1)
  X <- rnorm(n, mean = 0, sd = sqrt(sigma2))
  
  # Calculate sample mean
  X_bar <- mean(X)
  
  # Calculate Z-statistic
  Z <- X_bar / (sqrt(sigma2 / n))
  
  # Test: reject H0 if Z > z_alpha
  if (Z > z_alpha) {
    reject_H0_N0 <- reject_H0_N0 + 1
  }
}

# Frequency of rejecting H0 under N(0, 1) (should be close to 5%)
freq_reject_H0_N0 <- reject_H0_N0 / num_experiments
cat("Frequency of rejecting H0 under N(0, 1):", freq_reject_H0_N0, "\n")

# Step iv: Now redo the experiment for N(1, 1)
reject_H0_N1 <- 0 # Counter for rejections under N(1, 1)

for (i in 1:num_experiments) {
  # Generate n samples from N(1, 1)
  X <- rnorm(n, mean = 1, sd = sqrt(sigma2))
  
  # Calculate sample mean
  X_bar <- mean(X)
  
  # Calculate Z-statistic
  Z <- X_bar / (sqrt(sigma2 / n))
  
  # Test: reject H0 if Z > z_alpha
  if (Z > z_alpha) {
    reject_H0_N1 <- reject_H0_N1 + 1
  }
}

# Frequency of rejecting H0 under N(1, 1) (should be significantly larger than 5%)
freq_reject_H0_N1 <- reject_H0_N1 / num_experiments
cat("Frequency of rejecting H0 under N(1, 1):", freq_reject_H0_N1, "\n")