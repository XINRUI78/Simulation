library(doParallel)
library(foreach)
library(RcppNumerical)
library(brglm2)
#library(samplesizedev)

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
    .export = c("berank", "lasso_exact", "unirank"),
    .errorhandling = "pass"
  ) %dopar% {

    perform(
      i = i,
      ndev = ndev,
      n.para = n.para,
      beta0 = beta0,
      beta = beta,
      nval = nval,
      prev = prev,
      auc = auc,
      n.restrict = n.restrict
    )
  }

  parallel::stopCluster(cl)

  # Save CSV
  output_csv <- file.path("results", output_name)
  write.csv(result_ndev, file = output_csv, row.names = FALSE)

  return(result_ndev)
}


