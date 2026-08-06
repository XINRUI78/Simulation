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

task <- as.integer(commandArgs(trailingOnly = TRUE)[1])

sample_sizes <- c(ndev, ndev1, ndev2)
sample_size <- sample_sizes[task]

ncores <- as.integer(Sys.getenv("NSLOTS"))

cl <- makeCluster(ncores)
registerDoParallel(cl)

dir.create("results", showWarnings = FALSE)

results <- foreach(
  i = 1:1000,
  .combine = rbind,
  .packages = c(
    "glmnet",
    "pROC",
    "mvtnorm"
  )
) %dopar% {

  perform_s3(
    i = i,
    ndev = sample_size,
    n.para = n.para,
    n.true = n.true,
    beta0 = beta0,
    beta = beta,
    nval = nval
  )

}

stopCluster(cl)

saveRDS(
  results,
  file = paste0(
    "results/result_n_",
    sample_size,
    ".rds"
  )
)
