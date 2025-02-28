berank <- function(x, y, number = 15) {
  library(stats)  # For glm()
  
  # Start with all predictors
  predictors <- colnames(x)
  
  while (length(predictors) > number) {
    # Fit logistic regression model
    formula <- as.formula(paste("y ~", paste(predictors, collapse = " + ")))
    model <- glm(formula, data = data.frame(y, x[, predictors, drop = FALSE]), family = binomial)
    
    # Extract p-values
    p_values <- summary(model)$coefficients[-1, 4]  # Exclude intercept
    
    # Identify the variable with the highest p-value
    max_p_var <- names(p_values)[which.max(p_values)]
    
    # Remove the variable with the highest p-value
    predictors <- setdiff(predictors, max_p_var)
  }
  
  # Fit final model with exactly `number` predictors
  final_formula <- as.formula(paste("y ~", paste(predictors, collapse = " + ")))
  final_model <- glm(final_formula, data = data.frame(y, x[, predictors, drop = FALSE]), family = binomial)
  
  # Create binary selection vector
  varsel_be <- rep(0, ncol(x))
  names(varsel_be) <- colnames(x)
  varsel_be[predictors] <- 1  # Mark selected predictors as 1
  varsel_be <- unname(varsel_be)  # Remove names
  
  # Return results
  return(list(
    varsel_be = varsel_be,  # Unnamed binary vector indicating selected predictors
    model = final_model  # Final logistic regression model
  ))
}
