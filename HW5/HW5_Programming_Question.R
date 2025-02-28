# Load necessary library for plotting
library(ggplot2)

# Set parameters
p_true <- 0.3  # True value of p
n <- 40        # Sample size
iterations <- 100  # Number of simulations
z <- qnorm(0.975)  # z value for 95% confidence (two-sided)

# Initialize vectors to store results
lower_bounds <- numeric(iterations)
upper_bounds <- numeric(iterations)
sample_proportions <- numeric(iterations)

# Simulation
set.seed(123)  # For reproducibility
for (i in 1:iterations) {
  # Generate Bernoulli sample
  sample <- rbinom(n, size = 1, prob = p_true)
  
  # Calculate sample proportion
  p_hat <- mean(sample)
  
  # Calculate standard error
  se <- sqrt(p_hat * (1 - p_hat) / n)
  
  # Confidence interval bounds
  lower_bounds[i] <- p_hat - z * se
  upper_bounds[i] <- p_hat + z * se
  sample_proportions[i] <- p_hat
}

# Create a data frame for plotting
data <- data.frame(
  Sample = 1:iterations,
  Lower = lower_bounds,
  Upper = upper_bounds,
  Sample_Proportion = sample_proportions,
  Contains_p_true = (lower_bounds <= p_true & upper_bounds >= p_true)  # Whether interval contains true p
)

# Plot confidence intervals
ggplot(data, aes(x = Sample_Proportion, y = Sample)) +
  geom_errorbarh(aes(xmin = Lower, xmax = Upper, color = Contains_p_true), height = 0.4) +
  geom_vline(xintercept = p_true, color = "green", linetype = "dashed") +
  scale_color_manual(values = c("red", "gray")) +
  labs(x = "p (Sample Proportion)", y = "Samples", 
       title = "Confidence Intervals for p",
       subtitle = "Red intervals do not contain the true p = 0.3") +
  theme_minimal()