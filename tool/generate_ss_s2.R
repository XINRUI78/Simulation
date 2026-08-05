library(mvtnorm)
library(stats)
generate_ss_s2 <- function(n, n.para, n.true, beta0, beta){
  sigma <- diag(n.para)
  sigma[1:n.true, 1:n.true] <- 0.5
  diag(sigma) <- 1  # Ensure variances remain 1
  
  # Generate data
  x <- rmvnorm(n, mean = rep(0, n.para), sigma = sigma)
  eta <- rep(beta0, n) + x%*%beta
  p <- 1/(1+exp(-eta))
  y <- rbinom(n, 1, p)
  data <- data.frame(y,x)}
