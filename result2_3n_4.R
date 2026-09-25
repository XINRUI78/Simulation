# Source required scripts
source("tool/generate_ss_s2.R")
source("tool/perform_s2.R")
source("tool/measures.R")
source("tool/run_s2.R")
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


# Create output directory
dir.create(
  "results",
  recursive = TRUE,
  showWarnings = FALSE
)

# Create parallel cluster
cl <- parallel::makeCluster(32)
doParallel::registerDoParallel(cl)

# Run 1,000 repetitions
result_ndev <- foreach(
  i = seq_len(1000),
  .combine = rbind,
  .packages = c(
    "mvtnorm",
    "pROC",
    "glmnet"
  ),
     .export = c("backward_pvalue", "mod_penal_ave_foreach", "berank", "lasso_exact", "unirank", "unilogit", "perform_s2", "generate_ss_s2", "measures","opt_beta_s2","safe_measures"),
  .errorhandling = "pass"
) %dopar% {
  
  perform_s2(
    i = i,
    ndev = ndev3,
    n.para = n.para,
    beta0 = beta0,
    beta = beta,
    nval = nval,
    prev = prev_check,
    auc = auc_check,
    n.restrict = 22
  )
}

# Stop parallel cluster
parallel::stopCluster(cl)

# Save CSV result
output_csv <- file.path(
  "results",
  "result2_3n_4.csv"
)

write.csv(
  result_ndev,
  file = output_csv,
  row.names = FALSE
)
