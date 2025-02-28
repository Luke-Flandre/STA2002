#Install and load necessary package for skewness and kurtosis
install.packages("moments")
library(moments)

# Initialize vectors to store sample statistics
sample_means <- numeric(300)
sample_variances <- numeric(300)
sample_skewnesses <- numeric(300)
sample_kurtoses <- numeric(300)

# Repeat experiment 300 times
for (i in 1:n_experiments) {
  X <- runif(500, min = 0, max = 1)
  
  # Calculate sample statistics for each experiment
  sample_means[i] <- mean(X)
  sample_variances[i] <- var(X)
  sample_skewnesses[i] <- skewness(X)
  sample_kurtoses[i] <- kurtosis(X) - 3
}

# Plot  2*2 histograms for each sample statistic
par(mfrow = c(2, 2))

# Histogram for sample mean
hist(sample_means, main = "Means", xlab = "Sample Mean", col = "blue", breaks = 20)
abline(v = 0.5, col = "red")  # True mean

# Histogram for sample variance
hist(sample_variances, main = "Variances", xlab = "Sample Variance", col = "green", breaks = 20)
abline(v = 1/12, col = "red")

# Histogram for sample skewness
hist(sample_skewnesses, main = "Skewness", xlab = "Sample Skewness", col = "coral", breaks = 20)
abline(v = 0, col = "red")

# Histogram for sample kurtosis
hist(sample_kurtoses, main = "Kurtosis", xlab = "Sample Kurtosis", col = "gold", breaks = 20)
abline(v = -6/5, col = "red")

