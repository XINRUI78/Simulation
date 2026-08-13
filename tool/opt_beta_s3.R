library(MASS)  # For mvrnorm to simulate predictors
library(pROC)  # For AUC calculation

# Define the optimizer function
opt_beta_s3 <- function(n.para, n.true, prev, c, weights) {
  # Generate predictors (X) from multivariate normal distribution
  n = 500000
  sigma <- diag(n.para)
  true.idx <- seq_len(n.true)
  noise.idx <- (n.true + 1):n.para
  sigma[noise.idx, noise.idx] <- 0
  sigma[true.idx, noise.idx] <- 0.15
  sigma[noise.idx, true.idx] <- 0.15
  sigma[true.idx, true.idx] <- 0.5
  diag(sigma) <- 1
  # Generate data
  x <- mvtnorm::rmvnorm(n, mean = rep(0, n.para), sigma = sigma)
  
  
  objective <- function(para){
    beta0 <- para[1]  # Intercept
    s <- para[2]      # Scaling factor
    beta1 <- s * weights
    eta <- rep(beta0, n) + x %*% beta1
    p <- 1/(1+exp(-eta))
    y <- stats::rbinom(n, 1, p)
    pest <- mean(y)
    cstat <- pROC::roc(response = as.vector(y), predictor = as.vector(p), levels = c(0, 1), direction = "<")
    cest <- as.vector(cstat$auc)
    return((pest - prev)^2 + (cest - c)^2)
  }
  # Initial guesses for beta0 and s
  initial_para <- c(log(prev/(1-prev)), 1)
  tol = 1e-6
  # Perform optimization
  result <- optim(
    par = initial_para,
    fn = objective,
    method = "Nelder-Mead", 
    control = list(abstol = tol)
  )
  
  # Extract optimized coefficients
  beta0_opt <- result$par[1]
  s_opt <- result$par[2]
  beta1_opt <- s_opt * weights
  
  list(
    beta0 = beta0_opt,
    beta1 = beta1_opt,
    s = s_opt
  )
}



