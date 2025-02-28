# method = 4 for all subsets
set.seed(i)
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