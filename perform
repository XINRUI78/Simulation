# ideal we use
# n.para (numeric) : the number of candidate predictor parameters
# prev (numeric) : outcome prevalence
# cstat (numeric) 
# to find beta0, beta and sigma

# Output
# prev, cstat, ndev, method, measures(length: 5), varsel(length: n.para), option
# mean(varsel) is the prob of each predictor being selected

# example: perform(1000, n.para = 8, beta0 = -2.53, beta = c(0, 0, 0, 0.45, 0.45, 0.45, 0.45, 0.45), 10000)

perform <- function(ndev, n.para, beta0, beta, nval){
  
# Generate a large dataset (200,000) using function generate_ss
# Check prevalence and C-statistic; 
# Choose beta0 such that prevalence=0.1
# beta such that C-stat=0.75
n <- 200000; 
data <- generate_ss(n, n.para, beta0, beta)
prev <- mean(data[,1])
X <- as.matrix(data[,-1])
eta <- rep(beta0, n) + X%*%beta
p <- 1/(1+exp(-eta))
cstat <- pROC::roc(response = as.numeric(data[,1]), predictor = as.vector(p), levels = c(0, 1), direction = "<")
auc <- as.vector(cstat$auc)

# create a matrix for each method
n.loop <- 5 # first try 5 loops to check the code, then 100
mle <- matrix(, nrow = n.loop, ncol = 10 + n.para)
lassomin <- matrix(, nrow = n.loop, ncol = 10 + n.para)
lasso1se <- matrix(, nrow = n.loop, ncol = 10 + n.para)
uni <- matrix(, nrow = n.loop, ncol = 10 + n.para)
all <- matrix(, nrow = n.loop, ncol = 10 + n.para)
backaic <- matrix(, nrow = n.loop, ncol = 10 + n.para)
backp <- matrix(, nrow = n.loop, ncol = 10 + n.para)
blasso <- matrix(, nrow = n.loop, ncol = 10 + n.para)
horseshoe <- matrix(, nrow = n.loop, ncol = 10 + n.para)

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
fit <- glm(y ~ ., data = data.dev, family = 'binomial')
eta_val <- as.matrix(cbind(1,xval))%*%coef(fit)
p_val <- as.vector(1/(1+exp(-eta_val)))

mle[i,] <- c(prev, auc, ndev, 0, measures(yval, p_val), rep(1, n.para), NA)


# method = 1 for LASSO using lambda.min, method = 2 for LASSO using lambda.1se
lasso <- glmnet::cv.glmnet(as.matrix(x), y, alpha = 1, family = "binomial", type.measure = "deviance")
lambda_min <- lasso$lambda.min 
lambda_1se <- lasso$lambda.1se 

# validate the fitted models on the validation dataset
lassomin_p <- as.vector(predict(lasso, as.matrix(xval), s = lambda_min, type="response"))
lasso1se_p <- as.vector(predict(lasso, as.matrix(xval), s = lambda_1se, type="response"))

varsel_min <- ifelse(as.numeric(coef(lasso, s = lambda_min)[-1]) != 0, 1, 0)
varsel_1se <- ifelse(as.numeric(coef(lasso, s = lambda_1se)[-1]) != 0, 1, 0)

lassomin[i,] <- c(prev, auc, ndev, 1, measures(yval, lassomin_p), varsel_min, lambda_min)
lasso1se[i,] <- c(prev, auc, ndev, 2, measures(yval, lasso1se_p), varsel_1se, lambda_1se)

# method = 3 for univariable logistic
p_threshold = 0.05
unisum <- unilogit(x, y, p_threshold) 
varsel_uni <- unisum$varsel_uni
unimodel <- unisum$uni
uni_eta<- as.matrix(cbind(1,xval[,varsel_uni==1]))%*%coef(unimodel)
uni_p <- as.vector(1/(1+exp(-uni_eta)))
uni[i,] <- c(prev, auc, ndev, 3, measures(yval, uni_p), varsel_uni, p_threshold)

# method = 4 for all subset and AIC
library(glmulti)
allsubset_model <- do.call("glmulti", list(y ~ ., data = data.dev, family = "binomial", 
                                           level = 1, method = "h", crit = "aic"))
# Extract the formula of the final model
all_formula <- allsubset_model@formulas[[1]]
# varsel_all
varsel_all <- numeric(n.para)
all_predictors <- all.vars(all_formula)[-1] 
varsel_all[match(all_predictors, names(x))] <- 1

# Fit the final model
all_model <- allsubset_model@objects[[1]]
all_eta <- as.matrix(cbind(1,xval[,varsel_all==1]))%*%coef(all_model)
all_p <- as.vector(1/(1+exp(-all_eta)))

all[i,] <- c(prev, auc, ndev, 4, measures(yval, all_p), varsel_all, "AIC")

# method = 5 for backward elimination and AIC
back_aic <- back_logit(data.dev, "y", "AIC") 
varsel_backaic <- back_aic$varsel_back
backaic_model <- back_aic$backmodel
back_aic_eta<- as.matrix(cbind(1,xval[,varsel_backaic==1]))%*%coef(backaic_model)
backaic_p <- as.vector(1/(1+exp(-back_aic_eta)))
backaic[i,] <- c(prev, auc, ndev, 5, measures(yval, backaic_p), varsel_backaic, "AIC")

# method = 6 for backward elimination and p-value threshold
p_threshold = 0.05
back_p <- back_logit(data.dev, "y", "p_value", p_threshold) 
varsel_backp <- back_p$varsel_back
backp_model <- back_p$backmodel
back_p_eta<- as.matrix(cbind(1,xval[,varsel_backp == 1]))%*%coef(backp_model)
backp_p <- as.vector(1/(1+exp(-back_p_eta)))
backp[i,] <- c(prev, auc, ndev, 6, measures(yval, backp_p), varsel_backp, p_threshold)


# method = 7 for Bayesian LASSO
library(rstanarm)
# Fit Bayesian Lasso for binary outcomes using rstanarm
blasso_model <- stan_glm(y ~ ., data = data.dev, family = binomial(link = "logit"),
                         prior = lasso(df = 1, autoscale = TRUE), iter = 5000, chains = 2, 
                         thin = 10, warmup = 2000, iter = 5000, core = 2, seed = i)
intercept_blasso <- blasso_model$coefficients[1]
beta_blasso <- blasso_model$coefficients[-1]
blasso_ci <- posterior_interval(blasso_model, prob = 0.95)[-1,]
varsel_blasso <- ifelse(blasso_ci[ ,1] > 0 | blasso_ci[ ,2] < 0, 1, 0)
blasso_eta <- as.matrix(cbind(1,xval[,varsel_blasso == 1]))%*%append(intercept_blasso,beta_blasso[varsel_blasso == 1])
blasso_p <- as.vector(1/(1+exp(-blasso_eta)))
blasso[i,] <- c(prev, auc, ndev, 7, measures(yval, blasso_p), varsel_blasso, NA)

# method = 7 for Bayesian horseshoe
hs_model <- stan_glm(y ~ ., data = data.dev, family = binomial(link = "logit"),
                     prior = hs(), chains = 2, thin = 10, warmup = 2000, iter = 5000, core = 2, seed = i)
intercept_hs <- hs_model$coefficients[1]
beta_hs <- hs_model$coefficients[-1]
hs_ci <- posterior_interval(hs_model, prob = 0.95)[-1,]
varsel_hs <- ifelse(hs_ci[ ,1] > 0 | hs_ci[ ,2] < 0, 1, 0)
hs_eta <- as.matrix(cbind(1,xval[,varsel_hs == 1]))%*%append(intercept_hs, beta_hs[varsel_hs == 1])
hs_p <- as.vector(1/(1+exp(-hs_eta)))
horseshoe[i,] <- c(prev, auc, ndev, 8, measures(yval, hs_p), varsel_hs, NA)

}

results <- rbind(mle, lassomin, lasso1se, uni, all, backaic, backp, blasso, horseshoe)
colnames(results) <- c("prevalence", 
                 "anticipated c-stat", 
                 "ndev", 
                 "method", 
                 "calibration slope", 
                 "calibration in the large", 
                 "auc", 
                 "Brier score", 
                 "MAPE", 
                 paste0("varsel", 1:n.para),  
                 "option")

return(results)
}


