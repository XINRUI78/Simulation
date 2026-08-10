# Source required scripts
source("tool/generate_ss_s3.R")
source("tool/perform_s3.R")
source("tool/measures.R")
source("tool/run_s3.R")
source("method/back_logit.R")
source("method/backward_pvalue.R")
source("method/berank.R")
source("method/lasso_exact.R")
source("method/mod_penal_ave_foreach.R")
source("method/stepwise_pvalue.R")
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
  .export = c(
  "generate_ss_s3",
"perform_s3",
"measures",
"back_logit",
"backward_pvalue",
"berank",
"lasso_exact",
"mod_penal_ave_foreach",
"stepwise_pvalue",
"unilogit",
"unirank"
  )
) %dopar% {

  perform_s3(
    i = i,
    ndev = ndev,
    n.para = n.para,
    n.true = n.true,
    beta0 = beta0,
    beta = beta,
    nval = nval
  )
}

# Stop parallel cluster
parallel::stopCluster(cl)


# Save result
output_file <- file.path(
  "results",
  paste0("result_ndev_", ndev, "_1000_repetitions.rds")
)

saveRDS(
  result_ndev,
  file = output_file,
  compress = TRUE
)

