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
      # If exact match found, select lambda with lowest CV error
      valid_lambdas <- cv_lasso$lambda[valid_indices]
      valid_errors <- cv_lasso$cvm[valid_indices]
      selected_lambda <- valid_lambdas[which.min(valid_errors)]
      break  # Stop searching
    } else {
      # Keep track of the best available lambda selecting fewer predictors
      lower_indices <- which(num_selected < number)  # Find all lambdas selecting fewer predictors
      if (length(lower_indices) > 0) {
        closest_index <- lower_indices[which.max(num_selected[lower_indices])]  # Highest among fewer predictors
        best_lower_lambda <- cv_lasso$lambda[closest_index]
      }
      
      # Increase nlambda and retry
      nlambda <- nlambda * 2
      attempt <- attempt + 1
    }
}

# If no exact match was found, use the closest lambda that selects fewer predictors
if (!exists("selected_lambda") || is.null(selected_lambda)) {
  selected_lambda <- best_lower_lambda
}

    
    # **Refit LASSO model using only the selected lambda**
    mid_var <- ifelse(as.numeric(coef(cv_lasso, s = selected_lambda)[-1]) != 0, 1, 0)
    
    if (sum(mid_var) == 1) {
      return(list(
        varsel_lasso = mid_var,  # Selected predictor
        model = cv_lasso,  # Return the original LASSO model
        lambda = selected_lambda,  # Chosen lambda
        lasso_p = as.vector(predict(cv_lasso, as.matrix(xval), s = selected_lambda, type="response"))
      ))
    }
    
    # Ensure we have at most 15 predictors
    x_top <- x[, which(mid_var == 1), drop = FALSE]
    xval_top <- xval[, which(mid_var == 1), drop = FALSE]
    
    final_model <- cv.glmnet(as.matrix(x_top), y, family = "binomial", alpha = 1, nfolds = 10)
    final_lambda <- final_model$lambda.min 
    final_var <- ifelse(as.numeric(coef(final_model, s = final_lambda)[-1]) != 0, 1, 0)
    lasso_p <- as.vector(predict(final_model, as.matrix(xval_top), s = final_lambda, type="response"))
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
  


    
    
  
 