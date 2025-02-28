# Load necessary library and initialize the parameter
library(ggplot2)

sample_sizes <- c(5, 10, 20, 50)
A_list <- list()
B_list <- list()

# Loop through each sample size
for (n in sample_sizes) {
  A_vals <- numeric(200)
  B_vals <- numeric(200)
  
  # Run 200 simulations for each sample size
  for (sim in 1:200) {
    Z <- rpois(n, 1)  # Generate Poisson distribution with lambda = 1
    A <- mean(Z)  # Calculate A
    B <- sqrt(n) * (A - 1)  # Calculate B
    
    A_vals[sim] <- A
    B_vals[sim] <- B
  }
  
  # Store results
  A_list[[paste0("n_", n)]] <- A_vals
  B_list[[paste0("n_", n)]] <- B_vals
}

# Plot histograms
par(mfrow = c(2, 4))  # 2 rows, 4 columns for the 8 histograms

# Plot histograms for A
for (n in sample_sizes) {
  hist(A_list[[paste0("n_", n)]], main = paste("Histogram of A (n =", n, ")"), xlab = "A", col = "blue")
}

# Plot histograms for B
for (n in sample_sizes) {
  hist(B_list[[paste0("n_", n)]], main = paste("Histogram of B (n =", n, ")"), xlab = "B", col = "green")
}