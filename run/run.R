######### Set simulation parameters
n.para <- 30
prev <- 0.3
c <- 0.8
nval <- 10000

#rss <- samplesizedev(outcome = "Binary", S = 0.9, phi = prev, c = c, p = n.para)
#ndev <- rss$sim
ndev <- 1505
ndev1 <- round(ndev / 4 * 3)
ndev2 <- round(ndev / 2)
nedv3 <- round(ndev / 4)

######### obtain the coefficents of beta
run_simulation <- function(percentage) {

  strong <- percentage[1] * n.para
  medium <- percentage[2] * n.para
  weak <- percentage[3] * n.para
  noise <- percentage[4] * n.para
  weights <- c(rep(1, strong), rep(0.5, medium), rep(0.25, weak), rep(0, noise))

  beta_fit <- opt_beta(n.para, prev, c, weights)
  beta0 <- beta_fit$beta0
  beta <- beta_fit$beta1
  
  result_n <- perform(ndev, n.para, beta0, beta, nval)
  result_3n_4 <- perform(ndev1, n.para, beta0, beta, nval)
  result_n_2 <- perform(ndev2, n.para, beta0, beta, nval)
  result_n_4 <- perform(ndev3, n.para, beta0, beta, nval)
  
  return(list(result_n, result_3n_4, result_n_2,result_n_4))
}

sim1 <- run_simulation(c(0.1, 0.2, 0.2, 0.5))
sim3 <- run_simulation(c(0.5, 0, 0, 0.5))
sim4 <- run_simulation(c(0.2, 0.4, 0.4, 0))

write.csv(sim1$result,  "sim1_result.csv",  row.names = FALSE)
write.csv(sim1$result1, "sim1_result1.csv", row.names = FALSE)
write.csv(sim1$result2, "sim1_result2.csv", row.names = FALSE)
write.csv(sim3$result,  "sim3_result.csv",  row.names = FALSE)
write.csv(sim3$result1, "sim3_result1.csv", row.names = FALSE)
write.csv(sim3$result2, "sim3_result2.csv", row.names = FALSE)
write.csv(sim4$result,  "sim2_result.csv",  row.names = FALSE)
write.csv(sim4$result1, "sim2_result1.csv", row.names = FALSE)
write.csv(sim4$result2, "sim2_result2.csv", row.names = FALSE)

