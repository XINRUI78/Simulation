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
 # Generate a large dataset (200,000) using function generate_ss
  # Check prevalence and C-statistic; 
  n <- 200000; 
  data <- generate_ss_s3(n, n.para, n.true, beta0, beta)
  prev_ <- round(mean(data[,1]),2)
  X <- as.matrix(data[,-1])
  eta <- rep(beta0, n) + X%*%beta
  p <- 1/(1+exp(-eta))
  cstat <- pROC::roc(response = as.numeric(data[,1]), predictor = as.vector(p), levels = c(0, 1), direction = "<")
  auc_ <- round(as.vector(cstat$auc),2)
test <- perform_s3 (2, ndev, n.para, n.true, beta0, beta, nval, prev_, auc_)
saveRDS(test,"result.rds")
