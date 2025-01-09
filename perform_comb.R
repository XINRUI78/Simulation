# Set simulation parameters
n.para <- 30
prev <- 0.3
c <- 0.8
ndev <- 2000
nval <- 10000

percentage <- c(0.1, 0.3, 0.3, 0.3)

# Define relative strengths
strong <- percentage[1] * n.para  # 10% strong predictive variables
medium <- percentage[2] * n.para  # 30% medium predictive variables
weak <- percentage[3] * n.para    # 30% weak predictive variables
noise <- percentage[4] * n.para   # 30% noise predictive variables

# Assign relative strengths
weights <- c(rep(1, strong), 
             rep(0.5, medium), 
             rep(0.25, weak), 
             rep(0, noise))

opt_beta <- opt_beta(n.para, prev, c, weights)
beta0 <- opt_beta$beta0
beta <- opt_beta$beta1

result <- perform_comb(ndev, n.para, beta0, beta, nval)

# Convert the matrix to a data frame
summary <- as.data.frame(result)

##########################################################
# function of model performance of method combination

perform_comb <- function(ndev, n.para, beta0, beta, nval){
  
  # Generate a large dataset (200,000) using function generate_ss
  # Check prevalence and C-statistic; 
  # Choose beta0 such that prevalence=0.1
  # beta such that C-stat=0.75
  n <- 200000; 
  data <- generate_ss(n, n.para, beta0, beta)
  prev <- round(mean(data[,1]),2)
  X <- as.matrix(data[,-1])
  eta <- rep(beta0, n) + X%*%beta
  p <- 1/(1+exp(-eta))
  cstat <- pROC::roc(response = as.numeric(data[,1]), predictor = as.vector(p), levels = c(0, 1), direction = "<")
  auc <- round(as.vector(cstat$auc),2)
  
  # create a matrix for each method
  n.loop <- 1000 # first try 5 loops to check the code, then 100
  uni_05 <- matrix(NA, nrow = n.loop, ncol = 10 + n.para)
  uni_10 <- matrix(NA, nrow = n.loop, ncol = 10 + n.para)
  uni_05_back <- matrix(NA, nrow = n.loop, ncol = 10 + n.para)
  uni_10_back <- matrix(NA, nrow = n.loop, ncol = 10 + n.para)
  lassomin <- matrix(NA, nrow = n.loop, ncol = 10 + n.para)
  lasso1se <- matrix(NA, nrow = n.loop, ncol = 10 + n.para)
  lassomin_mle <- matrix(NA, nrow = n.loop, ncol = 10 + n.para)
  lasso1se_mle <- matrix(NA, nrow = n.loop, ncol = 10 + n.para)
  lasso_min_back <- matrix(NA, nrow = n.loop, ncol = 10 + n.para)
  lasso1se_back <- matrix(NA, nrow = n.loop, ncol = 10 + n.para)
  
  for (i in 1: n.loop){
    
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
    
    # fit models to development dataset
    
    # method = 0 for logistic using MLE
    #fit <- glm(y ~ ., data = data.dev, family = 'binomial')
    #eta_val <- as.matrix(cbind(1,xval))%*%coef(fit)
    #p_val <- as.vector(1/(1+exp(-eta_val)))
    
    #mle[i,] <- c(prev, auc, ndev, 0, measures(yval, p_val), rep(1, n.para), NA)
    
    # method = 1 for univariable logistic (p < 0.05)
    p_threshold <- 0.05
    unisum_05 <- unilogit(x, y, p_threshold) 
    varsel_uni_05 <- unisum_05$varsel_uni
    unimodel_05 <- unisum_05$uni
    uni_eta_05 <- as.matrix(cbind(1, xval[, varsel_uni_05 == 1])) %*% coef(unimodel_05)
    uni_p_05 <- as.vector(1 / (1 + exp(-uni_eta_05)))
    uni_05[i, ] <- c(prev, auc, ndev, 1, measures(yval, uni_p_05), varsel_uni_05, p_threshold)
    
    # method = 2 for univariable logistic (p < 0.1)
    p_threshold <- 0.1
    unisum_10 <- unilogit(x, y, p_threshold) 
    varsel_uni_10 <- unisum_10$varsel_uni
    unimodel_10 <- unisum_10$uni
    uni_eta_10 <- as.matrix(cbind(1, xval[, varsel_uni_10 == 1])) %*% coef(unimodel_10)
    uni_p_10 <- as.vector(1 / (1 + exp(-uni_eta_10)))
    uni_10[i, ] <- c(prev, auc, ndev, 2, measures(yval, uni_p_10), varsel_uni_10, p_threshold)
    
    # method = 3 for univariable logistic (p < 0.05) and backward elimination (p < 0.05)
    p_threshold = 0.05
    data.dev2 <- data.dev[, c(1, 1 + which(varsel_uni_05 == 1))]
    uni05_back <- back_logit(data.dev2, "y", "p_value", p_threshold) 
    
    varsel_uni05_back <- varsel_uni_05
    varsel_uni05_back[varsel_uni05_back == 1] <- uni05_back$varsel_back
    
    uni05_back_model <- uni05_back$backmodel
    uni05_back_eta <- as.matrix(cbind(1,xval[,varsel_uni05_back == 1]))%*%coef(uni05_back_model)
    uni05_back_p <- as.vector(1/(1+exp(-uni05_back_eta)))
    uni_05_back[i,] <- c(prev, auc, ndev, 3, measures(yval, uni05_back_p), varsel_uni05_back, p_threshold)
    
    # method = 4 for univariable logistic (p < 0.1) and backward elimination (p < 0.05)
    p_threshold = 0.05
    data.dev2 <- data.dev[, c(1, 1 + which(varsel_uni_10 ==1))]
    uni10_back <- back_logit(data.dev2, "y", "p_value", p_threshold) 
    
    varsel_uni10_back <- varsel_uni_10
    varsel_uni10_back[varsel_uni10_back == 1] <- uni10_back$varsel_back
    
    uni10_back_model <- uni10_back$backmodel
    uni10_back_eta <- as.matrix(cbind(1,xval[,varsel_uni10_back == 1]))%*%coef(uni10_back_model)
    uni10_back_p <- as.vector(1/(1+exp(-uni10_back_eta)))
    uni_10_back[i,] <- c(prev, auc, ndev, 4, measures(yval, uni10_back_p), varsel_uni10_back, p_threshold)
    
    # method = 5 for LASSO using lambda.min, method = 6 for LASSO using lambda.1se
    lasso <- glmnet::cv.glmnet(as.matrix(x), y, alpha = 1, family = "binomial", type.measure = "deviance")
    lambda_min <- lasso$lambda.min 
    lambda_1se <- lasso$lambda.1se 
    
    # validate the fitted models on the validation dataset
    lassomin_p <- as.vector(predict(lasso, as.matrix(xval), s = lambda_min, type="response"))
    lasso1se_p <- as.vector(predict(lasso, as.matrix(xval), s = lambda_1se, type="response"))
    
    varsel_min <- ifelse(as.numeric(coef(lasso, s = lambda_min)[-1]) != 0, 1, 0)
    varsel_1se <- ifelse(as.numeric(coef(lasso, s = lambda_1se)[-1]) != 0, 1, 0)
    
    lassomin[i,] <- c(prev, auc, ndev, 5, measures(yval, lassomin_p), varsel_min, lambda_min)
    lasso1se[i,] <- c(prev, auc, ndev, 6, measures(yval, lasso1se_p), varsel_1se, lambda_1se)
    
    # method = 7 for LASSO lambda.min and MLE
    data.s2 <- data.dev[, c(1, 1 + which(varsel_min == 1))]
    fit <- glm(y ~ ., data = data.s2, family = 'binomial')
    eta_val <- as.matrix(cbind(1,xval[, varsel_min == 1]))%*%coef(fit)
    p_val <- as.vector(1/(1+exp(-eta_val)))
    lassomin_mle[i,] <- c(prev, auc, ndev, 7, measures(yval, p_val), varsel_min, lambda_min)
  
    # method = 8 for LASSO lambda.1se and MLE
    data.s2 <- data.dev[, c(1, 1 + which(varsel_1se == 1))]
    fit <- glm(y ~ ., data = data.s2, family = 'binomial')
    eta_val <- as.matrix(cbind(1,xval[, varsel_1se == 1]))%*%coef(fit)
    p_val <- as.vector(1/(1+exp(-eta_val)))
    lasso1se_mle[i,] <- c(prev, auc, ndev, 8, measures(yval, p_val), varsel_1se, lambda_1se)
   
    # method = 9 for LASSO lambda.min and backward elimination (p < 0.05)
    p_threshold = 0.05
    data.s2 <- data.dev[, c(1, 1 + which(varsel_min == 1))]
    lassomin_back <- back_logit(data.s2, "y", "p_value", p_threshold) 
    
    varsel_min_back <- varsel_min
    varsel_min_back[varsel_min_back == 1] <- lassomin_back$varsel_back
    
    lasso_min_back_model <- lassomin_back$backmodel
    lasso_min_back_eta <- as.matrix(cbind(1,xval[,varsel_min_back == 1]))%*%coef(lasso_min_back_model)
    lasso_min_back_p <- as.vector(1/(1+exp(-lasso_min_back_eta)))
    lasso_min_back[i, ] <- c(prev, auc, ndev, 9, measures(yval, lasso_min_back_p), varsel_min_back, p_threshold)
    
    # method = 10 for LASSO lambda.1se and backward elimination (p < 0.05)
    p_threshold = 0.05
    data.s2 <- data.dev[, c(1, 1 + which(varsel_1se == 1))]
    lasso_1se_back <- back_logit(data.s2, "y", "p_value", p_threshold) 
    
    varsel_lasso1se_back <- varsel_1se
    varsel_lasso1se_back[varsel_lasso1se_back == 1] <- lasso_1se_back$varsel_back
    
    lasso1se_back_model <- lasso_1se_back$backmodel
    lasso1se_back_eta <- as.matrix(cbind(1,xval[,varsel_lasso1se_back == 1]))%*%coef(lasso1se_back_model)
    lasso1se_back_p <- as.vector(1/(1+exp(-lasso1se_back_eta)))
    lasso1se_back[i, ] <- c(prev, auc, ndev, 10, measures(yval, lasso1se_back_p), varsel_lasso1se_back, p_threshold)
  }
  
  results <- rbind(uni_05, uni_10, uni_05_back, uni_10_back, lassomin, lasso1se, lassomin_mle, lasso1se_mle, lasso_min_back, lasso1se_back)
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

