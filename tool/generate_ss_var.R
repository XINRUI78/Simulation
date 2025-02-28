generate_ss_var <- function(n, n.cand, p, prev, correlation = "independent", tolerance = 0.01, max_iter = 100){
  x <- generate_X(n, n.cand, correlation = correlation)
  beta0 <- log(prev/(1-prev)) 
  beta <- generate_beta(n.cand, p)
  for (i in 1:max_iter) {
    # Compute linear predictor eta with current beta0
    eta <- beta0 + x %*% beta
    probabilities <- 1 / (1 + exp(-eta))  # Apply logistic function
    y <- rbinom(n, 1, probabilities)  # Generate binary outcome based on probabilities
    
    # Calculate actual prevalence
    actual_prev <- mean(y)
    
    # Check if actual prevalence is within tolerance of target prevalence
    if (abs(actual_prev - prev) < tolerance) {
      cat("Calibrated beta0 after", i, "iterations.\n")
      break
    }
    
    # Adjust beta0 if actual prevalence differs from target
    adjustment <- log((prev / (1 - prev)) / (actual_prev / (1 - actual_prev)))
    beta0 <- beta0 + adjustment
  }
  eta <- rep(beta0, n) + x%*%beta
  y <- stats::rbinom(n, 1, 1/(1+exp(-eta)))
  data <- data.frame(y,x)
  beta_all <- c(beta0, beta)
  return(list(data = data, beta_all = beta_all))}

generate_beta <- function(m, p) {
  # Step 1: Initialize the parameter vector with zeros
  beta <- numeric(m)
  
  # Step 2: Randomly select `p` indexes for the important covariates
  important_indexes <- sample(1:m, p)
  
  # Step 3: Generate coefficients for the important covariates
  # Draw `p` values from a standard normal distribution
  Z <- rnorm(p)
  
  # Apply the formula to each `Z` to get the coefficients for important variables
  beta_values <- Z + 0.5 * (Z > 0) - 0.5 * (Z <= 0)
  
  # Ensure that all selected coefficients satisfy |βj| > 0.5
  beta[important_indexes] <- beta_values
  
  # Return the parameter vector
  return(beta)
}

# Example usage
# Set parameters: total number of covariates `m` and number of important covariates `p`
m <- 100   # Total covariates
p <- 5     # Important covariates

# Generate the parameter vector β
set.seed(123)  # For reproducibility
beta <- generate_beta(m, p)

# Print the result
print(beta)
print(paste("Important coefficients:", which(beta != 0)))




# Load necessary library for generating multivariate normal distribution
library(MASS)

# Define a function to generate X matrix based on different setups
generate_X <- function(n, m, correlation = "independent") {
  if (correlation == "independent") {
    # Generate independent covariates
    X <- matrix(rnorm(n * m), nrow = n, ncol = m)
  } else if (correlation == "correlated") {
    # Create a correlation matrix where Corr(Xi, Xj) = 0.5^|i - j|
    corr_matrix <- outer(1:m, 1:m, function(i, j) 0.5^abs(i - j))
    # Generate multivariate normal data with the specified correlation
    X <- mvrnorm(n = n, mu = rep(0, m), Sigma = corr_matrix)
  } else {
    stop("Invalid correlation type. Choose 'independent' or 'correlated'.")
  }
  return(X)
}

# Example usage
# Set parameters
n_values <- c(100, 200, 500)
m_values <- seq(10, 50, by=10)
p_values <- c(3, 5, 10)  # Number of important covariates (use this in the variable selection step)

# Generate data for each setup
set.seed(123)  # For reproducibility

for (n in n_values) {
  for (m in m_values) {
    # Independent covariates
    X_independent <- generate_X(n = n, m = m, correlation = "independent")
    
    # Correlated covariates
    X_correlated <- generate_X(n = n, m = m, correlation = "correlated")
    
    # Print or save the generated matrices
    print(paste("Independent setup with n =", n, "and m =", m))
    print(head(X_independent))
    
    print(paste("Correlated setup with n =", n, "and m =", m))
    print(head(X_correlated))
  }
}
