# Data from the question
attendance <- c(85, 73, 68, 56, 43)
time <- c(1, 2, 3, 4, 5)

log_likelihood <- function(params) {
  alpha <- params[1]
  beta <- params[2]
  
  # Calculate lambda for each time t
  lambda <- alpha + beta * time
  
  # Poisson log-likelihood
  ll <- sum(dpois(attendance, lambda = lambda, log = TRUE))
  
  return(-ll)  # Negative log-likelihood for minimization
}

# Initial guesses for alpha and beta
initial_guess <- c(50, -5)

# Optimize to find the MLE of alpha and beta
mle <- optim(par = initial_guess, fn = log_likelihood)

# Extract the MLE values
alpha_mle <- mle$par[1]
beta_mle <- mle$par[2]

cat("MLE of alpha:", alpha_mle, "\n")
cat("MLE of beta:", beta_mle, "\n")