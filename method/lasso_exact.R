lasso_exact <- function(x, y, xval, number, max_attempts = 10, initial_nlambda = 100) {
    library(glmnet)
    
    attempt <- 1
    nlambda <- initial_nlambda
    selected_lambda <- NULL
    
    while (attempt <= max_attempts) {
      # Fit LASSO using cross-validation with current nlambda
      cv_lasso <- cv.glmnet(as.matrix(x), y, family = "binomial", alpha = 1, nfolds = 10, nlambda = nlambda)
      
      # Extract coefficients from the fitted LASSO model
      coef_matrix <- as.matrix(coef(cv_lasso$glmnet.fit))[-1, ]  # Remove intercept
      num_selected <- colSums(coef_matrix != 0)  # Count nonzero coefficients for each lambda
      
      # Find all lambda values that select exactly 'number' predictors
      valid_indices <- which(num_selected == number)
      
      if (length(valid_indices) > 0) {
        # If multiple lambdas select 'number' predictors, choose the one with the lowest CV error
        valid_lambdas <- cv_lasso$lambda[valid_indices]
        valid_errors <- cv_lasso$cvm[valid_indices]
        selected_lambda <- valid_lambdas[which.min(valid_errors)]
        break  # Stop searching once we find an exact match
      } else {
        # If no exact match, increase nlambda and refit
        nlambda <- nlambda * 2
        attempt <- attempt + 1
      }
    }
    
    # **Refit LASSO model using only the selected lambda**
    mid_model <- glmnet(as.matrix(x), y, family = "binomial", alpha = 1, lambda = selected_lambda)
    mid_var <- ifelse(as.numeric(coef(mid_model, s = selected_lambda)[-1]) != 0, 1, 0)
    
    # Ensure we have at most 15 predictors
    x_top15 <- x[, which(mid_var == 1)]
    
    final_model <- cv.glmnet(as.matrix(x_top15), y, family = "binomial", alpha = 1, nfolds = 10)
    final_lambda <- final_model$lambda.min 
    final_var <- ifelse(as.numeric(coef(final_model, s = final_lambda)[-1]) != 0, 1, 0)
    lasso_p <- as.vector(predict(final_model, as.matrix(xval[,which(mid_var == 1)]), s = final_lambda, type="response"))
    # Create binary selection vector
    varsel_lasso <- mid_var
    varsel_lasso[varsel_lasso == 1] <- final_var
    
    # Return results
    return(list(
      varsel_lasso = varsel_lasso,  # Binary vector indicating selected predictors
      model = final_model,  # Final LASSO model refitted at selected_lambda
      lambda = final_lambda,  # Chosen lambda value
      lasso_p = lasso_p
    ))
  }
  


    
    
  
 