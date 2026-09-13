######### Set simulation parameters
n.para <- 30
prev <- 0.3
c <- 0.8
nval <- 10000
percentage <- c(0.1, 0.2, 0.2, 0.5)

# Define number of predictors with relative strengths
strong <- percentage[1] * n.para  # 10 strong predictive variables
medium <- percentage[2] * n.para  # 30% medium predictive variables
weak <- percentage[3] * n.para    # 30% weak predictive variables
noise <- percentage[4] * n.para   # 30% noise predictive variables

# Assign relative strengths
weights <- c(rep(1, strong), 
             rep(0.5, medium), 
             rep(0.25, weak), 
             rep(0, noise))

# recommended sample size
install.packages("devtools")
require("devtools")
devtools::install_github("mpavlou/samplesizedev")
require(samplesizedev)
library(samplesizedev)

rss <- samplesizedev(outcome = "Binary", S = 0.9, phi = prev, c = c, p = n.para)
ndev <- rss$sim
ndev1 <- round(ndev/4*3)
ndev2 <- round(ndev/2)
######### obtain the coefficents of beta
opt_beta <- opt_beta(n.para, prev, c, weights)
beta0 <- opt_beta$beta0
beta <- opt_beta$beta1

result <- perform(ndev, n.para, beta0, beta, nval)
result1 <- perform(ndev1, n.para, beta0, beta, nval)
result2 <- perform(ndev2, n.para, beta0, beta, nval)
