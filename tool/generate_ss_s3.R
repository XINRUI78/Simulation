library(mvtnorm)
library(stats)
generate_ss_s3 <- function(n, n.para, n.true, beta0, beta, rho){
  sigma <- diag(n.para)
  true.idx <- seq_len(n.true)
  noise.idx <- (n.true + 1):n.para
  sigma[noise.idx, noise.idx] <- 0
  sigma[true.idx, noise.idx] <- rho
  sigma[noise.idx, true.idx] <- rho
  sigma[true.idx, true.idx] <- rho
  
  # Variances must equal one
  diag(sigma) <- 1
  # Generate data
  x <- rmvnorm(n, mean = rep(0, n.para), sigma = sigma)
  eta <- rep(beta0, n) + x%*%beta
  p <- 1/(1+exp(-eta))
  y <- rbinom(n, 1, p)
  data <- data.frame(y,x)}
