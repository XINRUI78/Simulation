library(mvtnorm)
library(stats)
generate_ss_cor <- function(n, n.para, beta0, beta, sigma){
  # Generate data
  x <- rmvnorm(n, mean = rep(0, n.para), sigma = sigma)
  eta <- rep(beta0, n) + x%*%beta
  p <- 1/(1+exp(-eta))
  y <- rbinom(n, 1, p)
  data <- data.frame(y,x)}