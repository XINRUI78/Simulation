library(doParallel)
library(foreach)

perform_s3 <- function(i, ndev, n.para, n.true, beta0, beta, nval){
  # Generate a large dataset (200,000) using function generate_ss
  # Check prevalence and C-statistic; 
  n <- 200000; 
  data <- generate_ss_s3(n, n.para, n.true, beta0, beta)
  prev <- round(mean(data[,1]),2)
  X <- as.matrix(data[,-1])
  eta <- rep(beta0, n) + X%*%beta
  p <- 1/(1+exp(-eta))
  cstat <- pROC::roc(response = as.numeric(data[,1]), predictor = as.vector(p), levels = c(0, 1), direction = "<")
  auc <- round(as.vector(cstat$auc),2)

                                                                                                     set.seed(i)
                                                                                                     
                                                                                                     # generate a development dataset of size ndev=2000
                                                                                                     data.dev <- generate_ss_s3(ndev, n.para, n.true, beta0, beta)
                                                                                                     x <- data.dev[,-1]
                                                                                                     y <- data.dev[,1]
                                                                                                     
                                                                                                     # generate a validation dataset of size nval
                                                                                                     data.val <- generate_ss_s3(nval, n.para, n.true, beta0, beta)
                                                                                                     xval <- data.val[,-1]
                                                                                                     yval <- data.val[,1]
                                                                                                     
                                                                                                     ##########################################################
                                                                                                     
                                                                                                     # Model fitting
                                                                                                     # Initialize matrices for the different methods for this iteration
                                                                                                     method_result <- matrix(NA, nrow = 17, ncol = 10 + n.para)  # 13 methods
                                                                                                     
                                                                                                     # method = 0 for full model
                                                                                                     fit <- glm(y ~ ., data = data.dev, family = 'binomial')
                                                                                                     eta_val <- as.matrix(cbind(1,xval))%*%coef(fit)
                                                                                                     p_val <- as.vector(1/(1+exp(-eta_val)))
                                                                                                     method_result[1,] <- c(prev, auc, ndev, 0, measures(yval, p_val), rep(1, n.para), NA)
                                                                                                     
                                                                                                     # method = 1 for backward elimination for p value of 0.05
                                                                                                     p_threshold = 0.05
                                                                                                     back_05 <- back_logit(data.dev, "y", "p_value", p_threshold) 
                                                                                                     varsel_back_05 <- back_05$varsel_back
                                                                                                     backmodel_05 <- back_05$backmodel
                                                                                                     back_eta_05 <- as.matrix(cbind(1,xval[,varsel_back_05 == 1]))%*%coef(backmodel_05)
                                                                                                     back_p_05 <- as.vector(1/(1+exp(-back_eta_05)))
                                                                                                     method_result[2,] <- c(prev, auc, ndev, 1, measures(yval, back_p_05), varsel_back_05, p_threshold)
                                                                                                     
                                                                                                     # method = 2 for backward elimination for p value of 0.15
                                                                                                     p_threshold = 0.15
                                                                                                     back_15 <- back_logit(data.dev, "y", "p_value", p_threshold) 
                                                                                                     varsel_back_15 <- back_15$varsel_back
                                                                                                     backmodel_15 <- back_15$backmodel
                                                                                                     back_eta_15 <- as.matrix(cbind(1,xval[,varsel_back_15 == 1]))%*%coef(backmodel_15)
                                                                                                     back_p_15 <- as.vector(1/(1+exp(-back_eta_15)))
                                                                                                     method_result[3,] <- c(prev, auc, ndev, 2, measures(yval, back_p_15), varsel_back_15, p_threshold)
                                                                                                     
                                                                                                     # method = 3 for univariable logistic (p < 0.05)
                                                                                                     p_threshold <- 0.05
                                                                                                     unisum_05 <- unilogit(x, y, p_threshold) 
                                                                                                     varsel_uni_05 <- unisum_05$varsel_uni
                                                                                                     unimodel_05 <- unisum_05$uni
                                                                                                     uni_eta_05 <- as.matrix(cbind(1, xval[, varsel_uni_05 == 1])) %*% coef(unimodel_05)
                                                                                                     uni_p_05 <- as.vector(1 / (1 + exp(-uni_eta_05)))
                                                                                                     method_result[4,] <- c(prev, auc, ndev, 3, measures(yval, uni_p_05), varsel_uni_05, p_threshold)
                                                                                                     
                                                                                                     # method = 4 for univariable logistic (p < 0.15)
                                                                                                     p_threshold <- 0.15
                                                                                                     unisum_15 <- unilogit(x, y, p_threshold) 
                                                                                                     varsel_uni_15 <- unisum_15$varsel_uni
                                                                                                     unimodel_15 <- unisum_15$uni
                                                                                                     uni_eta_15 <- as.matrix(cbind(1, xval[, varsel_uni_15 == 1])) %*% coef(unimodel_15)
                                                                                                     uni_p_15 <- as.vector(1 / (1 + exp(-uni_eta_15)))
                                                                                                     method_result[5, ] <- c(prev, auc, ndev, 4, measures(yval, uni_p_15), varsel_uni_15, p_threshold)
                                                                                                     
                                                                                                     # method = 5 for univariable logistic (p < 0.05) and backward elimination (p < 0.05)
                                                                                                     p_threshold = 0.05
                                                                                                     data.dev2 <- data.dev[, c(1, 1 + which(varsel_uni_05 == 1))]
                                                                                                     uni05_back <- back_logit(data.dev2, "y", "p_value", p_threshold) 
                                                                                                     
                                                                                                     varsel_uni05_back <- varsel_uni_05
                                                                                                     varsel_uni05_back[varsel_uni05_back == 1] <- uni05_back$varsel_back
                                                                                                     
                                                                                                     uni05_back_model <- uni05_back$backmodel
                                                                                                     uni05_back_eta <- as.matrix(cbind(1,xval[,varsel_uni05_back == 1]))%*%coef(uni05_back_model)
                                                                                                     uni05_back_p <- as.vector(1/(1+exp(-uni05_back_eta)))
                                                                                                     method_result[6,] <- c(prev, auc, ndev, 5, measures(yval, uni05_back_p), varsel_uni05_back, 0.05)
                                                                                                     
                                                                                                     # method = 6 for univariable logistic (p < 0.15) and backward elimination (p < 0.05)
                                                                                                     p_threshold = 0.05
                                                                                                     data.dev2 <- data.dev[, c(1, 1 + which(varsel_uni_15 ==1))]
                                                                                                     uni15_back <- back_logit(data.dev2, "y", "p_value", p_threshold) 
                                                                                                     
                                                                                                     varsel_uni15_back <- varsel_uni_15
                                                                                                     varsel_uni15_back[varsel_uni15_back == 1] <- uni15_back$varsel_back
                                                                                                     
                                                                                                     uni15_back_model <- uni15_back$backmodel
                                                                                                     uni15_back_eta <- as.matrix(cbind(1,xval[,varsel_uni15_back == 1]))%*%coef(uni15_back_model)
                                                                                                     uni15_back_p <- as.vector(1/(1+exp(-uni15_back_eta)))
                                                                                                     method_result[7,] <- c(prev, auc, ndev, 6, measures(yval, uni15_back_p), varsel_uni15_back, 0.15)
                                                                                                     
                                                                                                     # method = 7 for LASSO using lambda.min, method = 8 for LASSO using lambda.1se
                                                                                                     lasso <- glmnet::cv.glmnet(as.matrix(x), y, alpha = 1, family = "binomial", type.measure = "deviance")
                                                                                                     lambda_min <- lasso$lambda.min 
                                                                                                     lambda_1se <- lasso$lambda.1se 
                                                                                                     
                                                                                                     # validate the fitted models on the validation dataset
                                                                                                     lassomin_p <- as.vector(predict(lasso, as.matrix(xval), s = lambda_min, type="response"))
                                                                                                     lasso1se_p <- as.vector(predict(lasso, as.matrix(xval), s = lambda_1se, type="response"))
                                                                                                     
                                                                                                     varsel_min <- ifelse(as.numeric(coef(lasso, s = lambda_min)[-1]) != 0, 1, 0)
                                                                                                     varsel_1se <- ifelse(as.numeric(coef(lasso, s = lambda_1se)[-1]) != 0, 1, 0)
                                                                                                     
                                                                                                     method_result[8,] <- c(prev, auc, ndev, 7, measures(yval, lassomin_p), varsel_min, lambda_min)
                                                                                                     method_result[9,] <- c(prev, auc, ndev, 8, measures(yval, lasso1se_p), varsel_1se, lambda_1se)
                                                                                                     
                                                                                                     # method = 9 for LASSO lambda.min and MLE
                                                                                                     data.s2 <- data.dev[, c(1, 1 + which(varsel_min == 1))]
                                                                                                     fit <- glm(y ~ ., data = data.s2, family = 'binomial')
                                                                                                     eta_min_mle <- as.matrix(cbind(1,xval[, varsel_min == 1]))%*%coef(fit)
                                                                                                     p_min_mle <- as.vector(1/(1+exp(-eta_min_mle)))
                                                                                                     method_result[10,] <- c(prev, auc, ndev, 9, measures(yval, p_min_mle), varsel_min, lambda_min)
                                                                                                     
                                                                                                     # method = 10 for LASSO lambda.1se and MLE
                                                                                                     data.s2 <- data.dev[, c(1, 1 + which(varsel_1se == 1))]
                                                                                                     fit <- glm(y ~ ., data = data.s2, family = 'binomial')
                                                                                                     eta_1se_mle <- as.matrix(cbind(1,xval[, varsel_1se == 1]))%*%coef(fit)
                                                                                                     p_1se_mle <- as.vector(1/(1+exp(-eta_1se_mle)))
                                                                                                     method_result[11,] <- c(prev, auc, ndev, 10, measures(yval, p_1se_mle), varsel_1se, lambda_1se)
                                                                                                     
                                                                                                     # method = 11 for LASSO lambda.min and backward elimination (p < 0.05)
                                                                                                     p_threshold = 0.05
                                                                                                     data.s2 <- data.dev[, c(1, 1 + which(varsel_min == 1))]
                                                                                                     lassomin_back <- back_logit(data.s2, "y", "p_value", p_threshold) 
                                                                                                     
                                                                                                     varsel_min_back <- varsel_min
                                                                                                     varsel_min_back[varsel_min_back == 1] <- lassomin_back$varsel_back
                                                                                                     
                                                                                                     lasso_min_back_model <- lassomin_back$backmodel
                                                                                                     lasso_min_back_eta <- as.matrix(cbind(1,xval[,varsel_min_back == 1]))%*%coef(lasso_min_back_model)
                                                                                                     lasso_min_back_p <- as.vector(1/(1+exp(-lasso_min_back_eta)))
                                                                                                     method_result[12, ] <- c(prev, auc, ndev, 11, measures(yval, lasso_min_back_p), varsel_min_back, lambda_min)
                                                                                                     
                                                                                                     # method = 12 for LASSO lambda.1se and backward elimination (p < 0.05)
                                                                                                     p_threshold = 0.05
                                                                                                     data.s2 <- data.dev[, c(1, 1 + which(varsel_1se == 1))]
                                                                                                     lasso_1se_back <- back_logit(data.s2, "y", "p_value", p_threshold) 
                                                                                                     
                                                                                                     varsel_lasso1se_back <- varsel_1se
                                                                                                     varsel_lasso1se_back[varsel_lasso1se_back == 1] <- lasso_1se_back$varsel_back
                                                                                                     
                                                                                                     lasso1se_back_model <- lasso_1se_back$backmodel
                                                                                                     lasso1se_back_eta <- as.matrix(cbind(1,xval[,varsel_lasso1se_back == 1]))%*%coef(lasso1se_back_model)
                                                                                                     lasso1se_back_p <- as.vector(1/(1+exp(-lasso1se_back_eta)))
                                                                                                     method_result[13, ] <- c(prev, auc, ndev, 12, measures(yval, lasso1se_back_p), varsel_lasso1se_back, lambda_1se)
                                                                                                     
                                                                                                     # method = 13 for modified LASSO
                                                                                                     nfolds <- 10
                                                                                                     f <- nfolds / (nfolds - 1) - 1
                                                                                                     mod_lasso <- mod_penal_ave_foreach(x = as.matrix(x), y = y, bn = 20, method = "lasso", f = f,
                                                                                                                                        parallel = FALSE, nfolds = nfolds, boot = TRUE)
                                                                                                     eta_mod_lasso <- as.matrix(cbind(1,xval))%*% mod_lasso$beta.boot
                                                                                                     p_mod_lasso <- as.vector(1/(1+exp(-eta_mod_lasso)))
                                                                                                     varsel_mod_lasso <- ifelse(as.numeric(mod_lasso$beta.boot)[-1] != 0, 1, 0)
                                                                                                     method_result[14, ] <- c(prev, auc, ndev, 13, measures(yval, p_mod_lasso), varsel_mod_lasso, as.numeric(mod_lasso$lambda.boot))
                                                                                                     
                                                                                                     # method = 14 for univariable ranking with the top 15 predictors
                                                                                                     unirank <- unirank(as.matrix(x), y, 15)
                                                                                                     varsel_uni <- unirank$varsel_uni
                                                                                                     unimodel <- unirank$model
                                                                                                     uni_eta <- as.matrix(cbind(1,xval[,varsel_uni == 1]))%*%coef(unimodel)
                                                                                                     uni_p <- as.vector(1/(1+exp(-uni_eta)))
                                                                                                     method_result[15,] <- c(prev, auc, ndev, 14, measures(yval, uni_p), varsel_uni, NA)
                                                                                                     
                                                                                                     # method = 15 for backward elimination ranking with the top 15 predictors
                                                                                                     berank <- berank(as.matrix(x), y, 15)
                                                                                                     varsel_be <- berank$varsel_be
                                                                                                     bemodel <- berank$model
                                                                                                     be_eta <- as.matrix(cbind(1,xval[,varsel_be == 1]))%*%coef(bemodel)
                                                                                                     be_p <- as.vector(1/(1+exp(-be_eta)))
                                                                                                     method_result[16,] <- c(prev, auc, ndev, 15, measures(yval, be_p), varsel_be, NA)
                                                                                                     
                                                                                                     # method = 16 for LASSO less than 15
                                                                                                     lasso_exact <- lasso_exact(x, y, xval, 15, max_attempts = 10, initial_nlambda = 100) 
                                                                                                     varsel_lasso <- lasso_exact$varsel_lasso
                                                                                                     lassomodel <- lasso_exact$model
                                                                                                     lambda <- lasso_exact$lambda
                                                                                                     lasso_p <- lasso_exact$lasso_p
                                                                                                     method_result[17,] <- c(prev, auc, ndev, 16, measures(yval, lasso_p), varsel_lasso, lambda)
                                                                                                     
                                                                                              
                                                                                                     
                                                                                                     return(method_result)
                                                                                                   }
  
  # Stop parallel backend
  stopCluster(cl)
  
  colnames(results) <- c("prevalence", 
                         "anticipated c-stat", 
                         "ndev", 
                         "method", 
                         "calibration slope", 
                         "calibration in the large", 
                         "auc", 
                         "Brier score", 
                         "rmspe", 
                         paste0("varsel", 1:n.para),  
                         "option")
  
  return(results)
}

##############
allperform <- function(ndev, n.para, beta0, beta, nval){
  
  # Register parallel backend with the number of cores available
  n.cores <- parallel::detectCores() - 1  # use one less than total cores to avoid overloading
  cl <- makeCluster(n.cores)
  registerDoParallel(cl)
  
  # Generate a large dataset (200,000) using function generate_ss
  # Check prevalence and C-statistic; 
  n <- 200000; 
  data <- generate_ss(n, n.para, beta0, beta)
  prev <- round(mean(data[,1]),2)
  X <- as.matrix(data[,-1])
  eta <- rep(beta0, n) + X%*%beta
  p <- 1/(1+exp(-eta))
  cstat <- pROC::roc(response = as.numeric(data[,1]), predictor = as.vector(p), levels = c(0, 1), direction = "<")
  auc <- round(as.vector(cstat$auc),2)
  
  # create a matrix for each method
  n.loop <- 2000 # first try 5 loops to check the code, then 100
  results <- foreach(i = 1: n.loop, .combine = rbind, .packages = c("glmnet", "pROC", "stats"), .export = c("generate_ss", 
                                                                                                   "measures", "back_logit", "backward_pvalue", "mod_penal_ave_foreach", "unilogit", "unirank", "berank", "lasso_exact")) %dopar% {
                                                                                                     source("unirank.R")
                                                                                                     source("berank.R")                
                                                                                                     source("lasso_exact.R")   
                                                                                                     set.seed(i)
                                                                                                     
                                                                                                     # generate a development dataset of size ndev=2000
                                                                                                     data.dev <- generate_ss(ndev, n.para, beta0, beta)
                                                                                                     x <- data.dev[,-1]
                                                                                                     y <- data.dev[,1]
                                                                                                     
                                                                                                     # generate a validation dataset of size nval
                                                                                                     data.val <- generate_ss(nval, n.para, beta0, beta)
                                                                                                     xval <- data.val[,-1]
                                                                                                     yval <- data.val[,1]
                                                                                                     
                                                                                                     ##########################################################
                                                                                                     
                                                                                                     # Model fitting
                                                                                                     # Initialize matrices for the different methods for this iteration
                                                                                                     method_result <- matrix(NA, nrow = 17, ncol = 10 + n.para)  # 13 methods
                                                                                                     
                                                                                                     # method = 0 for full model
                                                                                                     fit <- glm(y ~ ., data = data.dev, family = 'binomial')
                                                                                                     eta_val <- as.matrix(cbind(1,xval))%*%coef(fit)
                                                                                                     p_val <- as.vector(1/(1+exp(-eta_val)))
                                                                                                     method_result[1,] <- c(prev, auc, ndev, 0, measures(yval, p_val), rep(1, n.para), NA)
                                                                                                     
                                                                                                     # method = 1 for backward elimination for p value of 0.05
                                                                                                     p_threshold = 0.05
                                                                                                     back_05 <- back_logit(data.dev, "y", "p_value", p_threshold) 
                                                                                                     varsel_back_05 <- back_05$varsel_back
                                                                                                     backmodel_05 <- back_05$backmodel
                                                                                                     back_eta_05 <- as.matrix(cbind(1,xval[,varsel_back_05 == 1]))%*%coef(backmodel_05)
                                                                                                     back_p_05 <- as.vector(1/(1+exp(-back_eta_05)))
                                                                                                     method_result[2,] <- c(prev, auc, ndev, 1, measures(yval, back_p_05), varsel_back_05, p_threshold)
                                                                                                     
                                                                                                     # method = 2 for backward elimination for p value of 0.15
                                                                                                     p_threshold = 0.15
                                                                                                     back_15 <- back_logit(data.dev, "y", "p_value", p_threshold) 
                                                                                                     varsel_back_15 <- back_15$varsel_back
                                                                                                     backmodel_15 <- back_15$backmodel
                                                                                                     back_eta_15 <- as.matrix(cbind(1,xval[,varsel_back_15 == 1]))%*%coef(backmodel_15)
                                                                                                     back_p_15 <- as.vector(1/(1+exp(-back_eta_15)))
                                                                                                     method_result[3,] <- c(prev, auc, ndev, 2, measures(yval, back_p_15), varsel_back_15, p_threshold)
                                                                                                     
                                                                                                     # method = 3 for univariable logistic (p < 0.05)
                                                                                                     p_threshold <- 0.05
                                                                                                     unisum_05 <- unilogit(x, y, p_threshold) 
                                                                                                     varsel_uni_05 <- unisum_05$varsel_uni
                                                                                                     unimodel_05 <- unisum_05$uni
                                                                                                     uni_eta_05 <- as.matrix(cbind(1, xval[, varsel_uni_05 == 1])) %*% coef(unimodel_05)
                                                                                                     uni_p_05 <- as.vector(1 / (1 + exp(-uni_eta_05)))
                                                                                                     method_result[4,] <- c(prev, auc, ndev, 3, measures(yval, uni_p_05), varsel_uni_05, p_threshold)
                                                                                                     
                                                                                                     # method = 4 for univariable logistic (p < 0.15)
                                                                                                     p_threshold <- 0.15
                                                                                                     unisum_15 <- unilogit(x, y, p_threshold) 
                                                                                                     varsel_uni_15 <- unisum_15$varsel_uni
                                                                                                     unimodel_15 <- unisum_15$uni
                                                                                                     uni_eta_15 <- as.matrix(cbind(1, xval[, varsel_uni_15 == 1])) %*% coef(unimodel_15)
                                                                                                     uni_p_15 <- as.vector(1 / (1 + exp(-uni_eta_15)))
                                                                                                     method_result[5, ] <- c(prev, auc, ndev, 4, measures(yval, uni_p_15), varsel_uni_15, p_threshold)
                                                                                                     
                                                                                                     # method = 5 for univariable logistic (p < 0.05) and backward elimination (p < 0.05)
                                                                                                     p_threshold = 0.05
                                                                                                     data.dev2 <- data.dev[, c(1, 1 + which(varsel_uni_05 == 1))]
                                                                                                     uni05_back <- back_logit(data.dev2, "y", "p_value", p_threshold) 
                                                                                                     
                                                                                                     varsel_uni05_back <- varsel_uni_05
                                                                                                     varsel_uni05_back[varsel_uni05_back == 1] <- uni05_back$varsel_back
                                                                                                     
                                                                                                     uni05_back_model <- uni05_back$backmodel
                                                                                                     uni05_back_eta <- as.matrix(cbind(1,xval[,varsel_uni05_back == 1]))%*%coef(uni05_back_model)
                                                                                                     uni05_back_p <- as.vector(1/(1+exp(-uni05_back_eta)))
                                                                                                     method_result[6,] <- c(prev, auc, ndev, 5, measures(yval, uni05_back_p), varsel_uni05_back, 0.05)
                                                                                                     
                                                                                                     # method = 6 for univariable logistic (p < 0.15) and backward elimination (p < 0.05)
                                                                                                     p_threshold = 0.05
                                                                                                     data.dev2 <- data.dev[, c(1, 1 + which(varsel_uni_15 ==1))]
                                                                                                     uni15_back <- back_logit(data.dev2, "y", "p_value", p_threshold) 
                                                                                                     
                                                                                                     varsel_uni15_back <- varsel_uni_15
                                                                                                     varsel_uni15_back[varsel_uni15_back == 1] <- uni15_back$varsel_back
                                                                                                     
                                                                                                     uni15_back_model <- uni15_back$backmodel
                                                                                                     uni15_back_eta <- as.matrix(cbind(1,xval[,varsel_uni15_back == 1]))%*%coef(uni15_back_model)
                                                                                                     uni15_back_p <- as.vector(1/(1+exp(-uni15_back_eta)))
                                                                                                     method_result[7,] <- c(prev, auc, ndev, 6, measures(yval, uni15_back_p), varsel_uni15_back, 0.15)
                                                                                                     
                                                                                                     # method = 7 for LASSO using lambda.min, method = 8 for LASSO using lambda.1se
                                                                                                     lasso <- glmnet::cv.glmnet(as.matrix(x), y, alpha = 1, family = "binomial", type.measure = "deviance")
                                                                                                     lambda_min <- lasso$lambda.min 
                                                                                                     lambda_1se <- lasso$lambda.1se 
                                                                                                     
                                                                                                     # validate the fitted models on the validation dataset
                                                                                                     lassomin_p <- as.vector(predict(lasso, as.matrix(xval), s = lambda_min, type="response"))
                                                                                                     lasso1se_p <- as.vector(predict(lasso, as.matrix(xval), s = lambda_1se, type="response"))
                                                                                                     
                                                                                                     varsel_min <- ifelse(as.numeric(coef(lasso, s = lambda_min)[-1]) != 0, 1, 0)
                                                                                                     varsel_1se <- ifelse(as.numeric(coef(lasso, s = lambda_1se)[-1]) != 0, 1, 0)
                                                                                                     
                                                                                                     method_result[8,] <- c(prev, auc, ndev, 7, measures(yval, lassomin_p), varsel_min, lambda_min)
                                                                                                     method_result[9,] <- c(prev, auc, ndev, 8, measures(yval, lasso1se_p), varsel_1se, lambda_1se)
                                                                                                     
                                                                                                     # method = 9 for LASSO lambda.min and MLE
                                                                                                     data.s2 <- data.dev[, c(1, 1 + which(varsel_min == 1))]
                                                                                                     fit <- glm(y ~ ., data = data.s2, family = 'binomial')
                                                                                                     eta_min_mle <- as.matrix(cbind(1,xval[, varsel_min == 1]))%*%coef(fit)
                                                                                                     p_min_mle <- as.vector(1/(1+exp(-eta_min_mle)))
                                                                                                     method_result[10,] <- c(prev, auc, ndev, 9, measures(yval, p_min_mle), varsel_min, lambda_min)
                                                                                                     
                                                                                                     # method = 10 for LASSO lambda.1se and MLE
                                                                                                     data.s2 <- data.dev[, c(1, 1 + which(varsel_1se == 1))]
                                                                                                     fit <- glm(y ~ ., data = data.s2, family = 'binomial')
                                                                                                     eta_1se_mle <- as.matrix(cbind(1,xval[, varsel_1se == 1]))%*%coef(fit)
                                                                                                     p_1se_mle <- as.vector(1/(1+exp(-eta_1se_mle)))
                                                                                                     method_result[11,] <- c(prev, auc, ndev, 10, measures(yval, p_1se_mle), varsel_1se, lambda_1se)
                                                                                                     
                                                                                                     # method = 11 for LASSO lambda.min and backward elimination (p < 0.05)
                                                                                                     p_threshold = 0.05
                                                                                                     data.s2 <- data.dev[, c(1, 1 + which(varsel_min == 1))]
                                                                                                     lassomin_back <- back_logit(data.s2, "y", "p_value", p_threshold) 
                                                                                                     
                                                                                                     varsel_min_back <- varsel_min
                                                                                                     varsel_min_back[varsel_min_back == 1] <- lassomin_back$varsel_back
                                                                                                     
                                                                                                     lasso_min_back_model <- lassomin_back$backmodel
                                                                                                     lasso_min_back_eta <- as.matrix(cbind(1,xval[,varsel_min_back == 1]))%*%coef(lasso_min_back_model)
                                                                                                     lasso_min_back_p <- as.vector(1/(1+exp(-lasso_min_back_eta)))
                                                                                                     method_result[12, ] <- c(prev, auc, ndev, 11, measures(yval, lasso_min_back_p), varsel_min_back, lambda_min)
                                                                                                     
                                                                                                     # method = 12 for LASSO lambda.1se and backward elimination (p < 0.05)
                                                                                                     p_threshold = 0.05
                                                                                                     data.s2 <- data.dev[, c(1, 1 + which(varsel_1se == 1))]
                                                                                                     lasso_1se_back <- back_logit(data.s2, "y", "p_value", p_threshold) 
                                                                                                     
                                                                                                     varsel_lasso1se_back <- varsel_1se
                                                                                                     varsel_lasso1se_back[varsel_lasso1se_back == 1] <- lasso_1se_back$varsel_back
                                                                                                     
                                                                                                     lasso1se_back_model <- lasso_1se_back$backmodel
                                                                                                     lasso1se_back_eta <- as.matrix(cbind(1,xval[,varsel_lasso1se_back == 1]))%*%coef(lasso1se_back_model)
                                                                                                     lasso1se_back_p <- as.vector(1/(1+exp(-lasso1se_back_eta)))
                                                                                                     method_result[13, ] <- c(prev, auc, ndev, 12, measures(yval, lasso1se_back_p), varsel_lasso1se_back, lambda_1se)
                                                                                                     
                                                                                                     # method = 13 for modified LASSO
                                                                                                     nfolds <- 10
                                                                                                     f <- nfolds / (nfolds - 1) - 1
                                                                                                     mod_lasso <- mod_penal_ave_foreach(x = as.matrix(x), y = y, bn = 20, method = "lasso", f = f,
                                                                                                                                        parallel = FALSE, nfolds = nfolds, boot = TRUE)
                                                                                                     eta_mod_lasso <- as.matrix(cbind(1,xval))%*% mod_lasso$beta.boot
                                                                                                     p_mod_lasso <- as.vector(1/(1+exp(-eta_mod_lasso)))
                                                                                                     varsel_mod_lasso <- ifelse(as.numeric(mod_lasso$beta.boot)[-1] != 0, 1, 0)
                                                                                                     method_result[14, ] <- c(prev, auc, ndev, 13, measures(yval, p_mod_lasso), varsel_mod_lasso, as.numeric(mod_lasso$lambda.boot))
                                                                                                     
                                                                                                     # method = 14 for univariable ranking with the top 15 predictors
                                                                                                     unirank <- unirank(as.matrix(x), y, 15)
                                                                                                     varsel_uni <- unirank$varsel_uni
                                                                                                     unimodel <- unirank$model
                                                                                                     uni_eta <- as.matrix(cbind(1,xval[,varsel_uni == 1]))%*%coef(unimodel)
                                                                                                     uni_p <- as.vector(1/(1+exp(-uni_eta)))
                                                                                                     method_result[15,] <- c(prev, auc, ndev, 14, measures(yval, uni_p), varsel_uni, NA)
                                                                                                 
                                                                                                     
                                                                                                     return(method_result)
                                                                                                   }
  
  # Stop parallel backend
  stopCluster(cl)
  
  colnames(results) <- c("prevalence", 
                         "anticipated c-stat", 
                         "ndev", 
                         "method", 
                         "calibration slope", 
                         "calibration in the large", 
                         "auc", 
                         "Brier score", 
                         "rmspe", 
                         paste0("varsel", 1:n.para),  
                         "option")
  
  return(results)
}
