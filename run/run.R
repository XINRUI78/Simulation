# Source required scripts
source("tool/generate_ss.R")
source("tool/perform.R")
source("tool/measures.R")
source("tool/opt_beta.R")
source("method/backward_pvalue.R")
source("method/berank.R")
source("method/lasso_exact.R")
source("method/mod_penal_ave_foreach.R")
source("method/unilogit.R")
source("method/unirank.R")

library(doParallel)
library(foreach)
library(RcppNumerical)
library(brglm2)
library(samplesizedev)

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
ndev3 <- round(ndev / 4)

run_simulation <- function(percentage, output_name, ndev, n.restrict, n.para = 30, prev = 0.3, c = 0.8, nval = 10000,
                           nrep = 1000, cores = 32) {

  # Predictor strengths
  strong <- percentage[1] * n.para
  medium <- percentage[2] * n.para
  weak <- percentage[3] * n.para
  noise <- percentage[4] * n.para

  weights <- c(rep(1, strong), rep(0.5, medium), rep(0.25, weak), rep(0, noise))

  # Obtain coefficients
  beta_fit <- opt_beta(n.para, prev, c, weights)
  beta0 <- beta_fit$beta0
  beta <- beta_fit$beta1

  # If these are calculated elsewhere in your code, keep your existing values
  prev_check <- prev
  auc_check <- c

  # Create output directory
  dir.create("results", recursive = TRUE, showWarnings = FALSE)

  # Parallel cluster
  cl <- parallel::makeCluster(cores)
  doParallel::registerDoParallel(cl)

  # Run repetitions
  result_ndev <- foreach(
    i = seq_len(nrep),
    .combine = rbind,
    .packages = c("mvtnorm", "pROC", "glmnet"),
    .export = c("berank", "lasso_exact", "stepwise_pvalue", "unirank"),
    .errorhandling = "pass"
  ) %dopar% {

    perform_s2(
      i = i,
      ndev = ndev,
      n.para = n.para,
      n.true = sum(weights != 0),
      beta0 = beta0,
      beta = beta,
      nval = nval,
      prev = prev_check,
      auc = auc_check,
      n.restrict = n.restrict
    )
  }

  parallel::stopCluster(cl)

  # Save CSV
  output_csv <- file.path("results", output_name)
  write.csv(result_ndev, file = output_csv, row.names = FALSE)

  return(result_ndev)
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

