# Source required scripts
source("tool/generate_ss_s3.R")
source("tool/perform_s3.R")
source("tool/measures.R")
source("method/back_logit.R")
source("method/backward_pvalue.R")
source("method/berank.R")
source("method/lasso_exact.R")
source("method/modified_pen.R")
source("method/stepwise.R")
source("method/unilogit.R")
source("method/unirank.R")

# Run simulation for ndev
result_n <- perform_s3(
  ndev,
  n.para,
  n.true,
  beta0,
  beta,
  nval
)

saveRDS(result_n, "result_n.rds")

# Run simulation for ndev1
result_n_2 <- perform_s3(
  ndev1,
  n.para,
  n.true,
  beta0,
  beta,
  nval
)

saveRDS(result_n_2, "result_n_2.rds")

# Run simulation for ndev2
result_n_4 <- perform_s3(
  ndev2,
  n.para,
  n.true,
  beta0,
  beta,
  nval
)

saveRDS(result_n_4, "result_n_4.rds")
