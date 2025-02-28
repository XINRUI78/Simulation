p_com <- function(ndev, n.para, beta0, beta, nval){
  source("unirank.R")
  source("berank.R")                
  source("lasso_exact.R")   
  set.seed(1)
  
  # generate a development dataset of size ndev=2000
  data.dev <- generate_ss(ndev, n.para, beta0, beta)
  x <- data.dev[,-1]
  y <- data.dev[,1]
  
  # generate a validation dataset of size nval
  data.val <- generate_ss(nval, n.para, beta0, beta)
  xval <- data.val[,-1]
  yval <- data.val[,1]
                                                                                                                              # Initialize matrices for the different methods for this iteration
                                                                                                                              p_result <- matrix(NA, ncol = 20, nrow = nval)  # 13 methods
                                                                                                                              
                                                                                                                              # method = 0 for full model
                                                                                                                              fit <- glm(y ~ ., data = data.dev, family = 'binomial')
                                                                                                                              eta_val <- as.matrix(cbind(1,xval))%*%coef(fit)
                                                                                                                              p_val <- as.vector(1/(1+exp(-eta_val)))
                                                                                                                              p_result[,1] <- p_val
                                                                                                                              
                                                                                                                              # method = 1 for backward elimination for p value of 0.05
                                                                                                                              p_threshold = 0.05
                                                                                                                              back_05 <- back_logit(data.dev, "y", "p_value", p_threshold) 
                                                                                                                              varsel_back_05 <- back_05$varsel_back
                                                                                                                              backmodel_05 <- back_05$backmodel
                                                                                                                              back_eta_05 <- as.matrix(cbind(1,xval[,varsel_back_05 == 1]))%*%coef(backmodel_05)
                                                                                                                              back_p_05 <- as.vector(1/(1+exp(-back_eta_05)))
                                                                                                                              p_result[,2] <- back_p_05
                                                                                                                              
                                                                                                                              # method = 2 for backward elimination for p value of 0.15
                                                                                                                              p_threshold = 0.15
                                                                                                                              back_15 <- back_logit(data.dev, "y", "p_value", p_threshold) 
                                                                                                                              varsel_back_15 <- back_15$varsel_back
                                                                                                                              backmodel_15 <- back_15$backmodel
                                                                                                                              back_eta_15 <- as.matrix(cbind(1,xval[,varsel_back_15 == 1]))%*%coef(backmodel_15)
                                                                                                                              back_p_15 <- as.vector(1/(1+exp(-back_eta_15)))
                                                                                                                              p_result[,3] <- back_p_15
                                                                                                                              
                                                                                                                              # method = 3 for univariable logistic (p < 0.05)
                                                                                                                              p_threshold <- 0.05
                                                                                                                              unisum_05 <- unilogit(x, y, p_threshold) 
                                                                                                                              varsel_uni_05 <- unisum_05$varsel_uni
                                                                                                                              unimodel_05 <- unisum_05$uni
                                                                                                                              uni_eta_05 <- as.matrix(cbind(1, xval[, varsel_uni_05 == 1])) %*% coef(unimodel_05)
                                                                                                                              uni_p_05 <- as.vector(1 / (1 + exp(-uni_eta_05)))
                                                                                                                              p_result[,4] <- uni_p_05
                                                                                                                              
                                                                                                                              # method = 4 for univariable logistic (p < 0.15)
                                                                                                                              p_threshold <- 0.15
                                                                                                                              unisum_15 <- unilogit(x, y, p_threshold) 
                                                                                                                              varsel_uni_15 <- unisum_15$varsel_uni
                                                                                                                              unimodel_15 <- unisum_15$uni
                                                                                                                              uni_eta_15 <- as.matrix(cbind(1, xval[, varsel_uni_15 == 1])) %*% coef(unimodel_15)
                                                                                                                              uni_p_15 <- as.vector(1 / (1 + exp(-uni_eta_15)))
                                                                                                                              p_result[,5] <- uni_p_15
                                                                                                                              
                                                                                                                              # method = 5 for univariable logistic (p < 0.05) and backward elimination (p < 0.05)
                                                                                                                              p_threshold = 0.05
                                                                                                                              data.dev2 <- data.dev[, c(1, 1 + which(varsel_uni_05 == 1))]
                                                                                                                              uni05_back <- back_logit(data.dev2, "y", "p_value", p_threshold) 
                                                                                                                              
                                                                                                                              varsel_uni05_back <- varsel_uni_05
                                                                                                                              varsel_uni05_back[varsel_uni05_back == 1] <- uni05_back$varsel_back
                                                                                                                              
                                                                                                                              uni05_back_model <- uni05_back$backmodel
                                                                                                                              uni05_back_eta <- as.matrix(cbind(1,xval[,varsel_uni05_back == 1]))%*%coef(uni05_back_model)
                                                                                                                              uni05_back_p <- as.vector(1/(1+exp(-uni05_back_eta)))
                                                                                                                              p_result[,6] <- uni05_back_p
                                                                                                                              
                                                                                                                              # method = 6 for univariable logistic (p < 0.15) and backward elimination (p < 0.05)
                                                                                                                              p_threshold = 0.05
                                                                                                                              data.dev2 <- data.dev[, c(1, 1 + which(varsel_uni_15 ==1))]
                                                                                                                              uni15_back <- back_logit(data.dev2, "y", "p_value", p_threshold) 
                                                                                                                              
                                                                                                                              varsel_uni15_back <- varsel_uni_15
                                                                                                                              varsel_uni15_back[varsel_uni15_back == 1] <- uni15_back$varsel_back
                                                                                                                              
                                                                                                                              uni15_back_model <- uni15_back$backmodel
                                                                                                                              uni15_back_eta <- as.matrix(cbind(1,xval[,varsel_uni15_back == 1]))%*%coef(uni15_back_model)
                                                                                                                              uni15_back_p <- as.vector(1/(1+exp(-uni15_back_eta)))
                                                                                                                              p_result[,7] <- uni15_back_p
                                                                                                                              
                                                                                                                              # method = 7 for LASSO using lambda.min, method = 8 for LASSO using lambda.1se
                                                                                                                              lasso <- glmnet::cv.glmnet(as.matrix(x), y, alpha = 1, family = "binomial", type.measure = "deviance")
                                                                                                                              lambda_min <- lasso$lambda.min 
                                                                                                                              lambda_1se <- lasso$lambda.1se 
                                                                                                                              
                                                                                                                              # validate the fitted models on the validation dataset
                                                                                                                              lassomin_p <- as.vector(predict(lasso, as.matrix(xval), s = lambda_min, type="response"))
                                                                                                                              lasso1se_p <- as.vector(predict(lasso, as.matrix(xval), s = lambda_1se, type="response"))
                                                                                                                              
                                                                                                                              varsel_min <- ifelse(as.numeric(coef(lasso, s = lambda_min)[-1]) != 0, 1, 0)
                                                                                                                              varsel_1se <- ifelse(as.numeric(coef(lasso, s = lambda_1se)[-1]) != 0, 1, 0)
                                                                                                                              
                                                                                                                              p_result[,8] <- lassomin_p
                                                                                                                              p_result[,9] <- lasso1se_p
                                                                                                                              
                                                                                                                              # method = 9 for LASSO lambda.min and MLE
                                                                                                                              data.s2 <- data.dev[, c(1, 1 + which(varsel_min == 1))]
                                                                                                                              fit <- glm(y ~ ., data = data.s2, family = 'binomial')
                                                                                                                              eta_min_mle <- as.matrix(cbind(1,xval[, varsel_min == 1]))%*%coef(fit)
                                                                                                                              p_min_mle <- as.vector(1/(1+exp(-eta_min_mle)))
                                                                                                                              p_result[,10] <- p_min_mle
                                                                                                                              
                                                                                                                              # method = 10 for LASSO lambda.1se and MLE
                                                                                                                              data.s2 <- data.dev[, c(1, 1 + which(varsel_1se == 1))]
                                                                                                                              fit <- glm(y ~ ., data = data.s2, family = 'binomial')
                                                                                                                              eta_1se_mle <- as.matrix(cbind(1,xval[, varsel_1se == 1]))%*%coef(fit)
                                                                                                                              p_1se_mle <- as.vector(1/(1+exp(-eta_1se_mle)))
                                                                                                                              p_result[,11] <- p_1se_mle
                                                                                                                              
                                                                                                                              # method = 11 for LASSO lambda.min and backward elimination (p < 0.05)
                                                                                                                              p_threshold = 0.05
                                                                                                                              data.s2 <- data.dev[, c(1, 1 + which(varsel_min == 1))]
                                                                                                                              lassomin_back <- back_logit(data.s2, "y", "p_value", p_threshold) 
                                                                                                                              
                                                                                                                              varsel_min_back <- varsel_min
                                                                                                                              varsel_min_back[varsel_min_back == 1] <- lassomin_back$varsel_back
                                                                                                                              
                                                                                                                              lasso_min_back_model <- lassomin_back$backmodel
                                                                                                                              lasso_min_back_eta <- as.matrix(cbind(1,xval[,varsel_min_back == 1]))%*%coef(lasso_min_back_model)
                                                                                                                              lasso_min_back_p <- as.vector(1/(1+exp(-lasso_min_back_eta)))
                                                                                                                              p_result[,12] <- lasso_min_back_p
                                                                                                                              
                                                                                                                              # method = 12 for LASSO lambda.1se and backward elimination (p < 0.05)
                                                                                                                              p_threshold = 0.05
                                                                                                                              data.s2 <- data.dev[, c(1, 1 + which(varsel_1se == 1))]
                                                                                                                              lasso_1se_back <- back_logit(data.s2, "y", "p_value", p_threshold) 
                                                                                                                              
                                                                                                                              varsel_lasso1se_back <- varsel_1se
                                                                                                                              varsel_lasso1se_back[varsel_lasso1se_back == 1] <- lasso_1se_back$varsel_back
                                                                                                                              
                                                                                                                              lasso1se_back_model <- lasso_1se_back$backmodel
                                                                                                                              lasso1se_back_eta <- as.matrix(cbind(1,xval[,varsel_lasso1se_back == 1]))%*%coef(lasso1se_back_model)
                                                                                                                              lasso1se_back_p <- as.vector(1/(1+exp(-lasso1se_back_eta)))
                                                                                                                              p_result[,13] <- lasso1se_back_p
                                                                                                                              
                                                                                                                              # method = 13 for modified LASSO
                                                                                                                              nfolds <- 10
                                                                                                                              f <- nfolds / (nfolds - 1) - 1
                                                                                                                              mod_lasso <- mod_penal_ave_foreach(x = as.matrix(x), y = y, bn = 20, method = "lasso", f = f,
                                                                                                                                                                 parallel = FALSE, nfolds = nfolds, boot = TRUE)
                                                                                                                              eta_mod_lasso <- as.matrix(cbind(1,xval))%*% mod_lasso$beta.boot
                                                                                                                              p_mod_lasso <- as.vector(1/(1+exp(-eta_mod_lasso)))
                                                                                                                              varsel_mod_lasso <- ifelse(as.numeric(mod_lasso$beta.boot)[-1] != 0, 1, 0)
                                                                                                                              p_result[,14] <- p_mod_lasso
                                                                                                                              
                                                                                                                              # method = 14 for univariable ranking with the top 15 predictors
                                                                                                                              unirank <- unirank(as.matrix(x), y, 15)
                                                                                                                              varsel_uni <- unirank$varsel_uni
                                                                                                                              unimodel <- unirank$model
                                                                                                                              uni_eta <- as.matrix(cbind(1,xval[,varsel_uni == 1]))%*%coef(unimodel)
                                                                                                                              uni_p <- as.vector(1/(1+exp(-uni_eta)))
                                                                                                                              p_result[,15] <- uni_p
                                                                                                                              
                                                                                                                              # method = 15 for backward elimination ranking with the top 15 predictors
                                                                                                                              berank <- berank(as.matrix(x), y, 15)
                                                                                                                              varsel_be <- berank$varsel_be
                                                                                                                              bemodel <- berank$model
                                                                                                                              be_eta <- as.matrix(cbind(1,xval[,varsel_be == 1]))%*%coef(bemodel)
                                                                                                                              be_p <- as.vector(1/(1+exp(-be_eta)))
                                                                                                                              p_result[,16] <- be_p
                                                                                                                              
                                                                                                                              # method = 16 for LASSO less than 15
                                                                                                                              lasso_exact <- lasso_exact(x, y, xval, 15, max_attempts = 10, initial_nlambda = 100) 
                                                                                                                              varsel_lasso <- lasso_exact$varsel_lasso
                                                                                                                              lassomodel <- lasso_exact$model
                                                                                                                              lambda <- lasso_exact$lambda
                                                                                                                              lasso_p <- lasso_exact$lasso_p
                                                                                                                              p_result[,17] <- lasso_p
                                                                                                                              
                                                                                                                              # method = 17 for random forest
                                                                                                                              library(randomForest)
                                                                                                                              rf_model <- randomForest(x = x, y = as.factor(y), ntree = 500)
                                                                                                                              rf_pred_prob <- predict(rf_model, xval, type = "prob")[,2]  # Get probability for class 1
                                                                                                                              p_result[,18] <- rf_pred_prob
                                                                                                                              
                                                                                                                              # method = 18 for random forest with top 15 features 
                                                                                                                              
                                                                                                                              # Select the top 15 most important features
                                                                                                                              rf_var_importance <- importance(rf_model)[, 1]  # Get importance scores
                                                                                                                              top_15_vars <- names(sort(rf_var_importance, decreasing = TRUE))[1:min(15, length(rf_var_importance))]
                                                                                                                              rf_varsel_top15 <- rep(0, n.para)
                                                                                                                              rf_varsel_top15[match(top_15_vars, colnames(x))] <- 1  # Mark selected variables as 1
                                                                                                                              
                                                                                                                              # Subset data to only include the selected top 15 features
                                                                                                                              x_top15 <- x[, top_15_vars]
                                                                                                                              xval_top15 <- xval[, top_15_vars]
                                                                                                                              rf_top15 <- randomForest(x = x_top15, y = as.factor(y), ntree = 500)
                                                                                                                              rf_pred_prob_top15 <- predict(rf_top15, xval_top15, type = "prob")[,2]  # Get probability for class 1
                                                                                                                              p_result[,19] <- rf_pred_prob_top15
                                                                                                                              
                                                                                                                              # true predicted probability
                                                                                                                              eta <- rep(beta0, nval) + as.matrix(xval)%*%beta
                                                                                                                              p <- 1/(1+exp(-eta))
                                                                                                                              p_result[,20] <- p
                                                                                                                              
                                                                                                                              return(p_result)
                                                                                                                            }
  
 