# Load necessary library for plotting
library(ggplot2)

# Set seed for reproducibility
set.seed(123)

# Create an empty matrix to store running sample means for each repetition
running_means_matrix <- matrix(0, nrow = n_samples, ncol = n_repeats)

# Repeat the experiment 10 times
for (i in 1:10) {
  X <- runif(500, min = 0, max = 1)
  running_means_matrix[, i] <- cumsum(X) / seq_along(X)
}

# Convert the running means to a long format for plotting
running_means_df <- data.frame(Iteration = rep(1:500, 10),
                               RunningMean = as.vector(running_means_matrix),
                               Experiment = rep(1:10, each = 500))

# Plot the running sample means
ggplot(running_means_df, aes(x = Iteration, y = RunningMean, color = as.factor(Experiment))) +
  geom_line() +
  labs(title = "Running Sample Means for 10 Repeated Experiments",
       x = "Iteration",
       y = "Running Mean",
       color = "Experiment") +
  theme_minimal()
