# Source required scripts
source("tool/generate_ss_s3.R")
source("tool/perform_s3.R")
source("tool/measures.R")
source("tool/run_s3.R")
source("method/back_logit.R")
source("method/backward_pvalue.R")
source("method/berank.R")
source("method/lasso_exact.R")
source("method/modified_pen.R")
source("method/stepwise.R")
source("method/unilogit.R")
source("method/unirank.R")

library(doParallel)
library(foreach)

# Read number of cores allocated by Myriad
ncores <- suppressWarnings(
  as.integer(Sys.getenv("NSLOTS", unset = "1"))
)

# Create output directory
dir.create(
  "results",
  recursive = TRUE,
  showWarnings = FALSE
)

# Create parallel cluster
cl <- parallel::makeCluster(ncores)
doParallel::registerDoParallel(cl)

# Run 1,000 repetitions
result_ndev <- foreach(
  i = seq_len(1000),
  .combine = rbind,
  .errorhandling = "pass",
  .packages = c(
    "mvtnorm",
    "pROC",
    "glmnet"
  ),
  .export = c(
    "perform_s3",
    "generate_ss_s3",
    "measures",
    "opt_beta_s3",
    "back_logit",
    "unilogit",
    "mod_penal_ave_foreach",
    "unirank",
    "berank",
    "lasso_exact"
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

