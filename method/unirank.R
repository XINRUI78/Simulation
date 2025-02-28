unirank <- function(x, y, number) {
  
  # Function to fit univariate logistic regression and extract p-value
  get_p_value <- function(var) {
    model <- glm(y ~ var, family = binomial)
    summary(model)$coefficients[2, 4]  # Extract p-value
  }
  
  # Compute p-values for each predictor
  p_values <- sapply(1:ncol(x), function(i) get_p_value(x[, i]))
  
  # Rank predictors by p-value (smallest first)
  ranked_indices <- order(p_values)
  
  # Select the top 15 predictors
  top_vars <- ranked_indices[1:number]
  
  # Sort selected predictors by their original order
  top_vars <- sort(top_vars)
  
  # Create binary selection vector
  varsel_uni <- rep(0, ncol(x))
  varsel_uni[top_vars] <- 1  # Mark selected predictors as 1
  
  # Fit multivariable logistic regression with selected predictors
  selected_cols <- colnames(x)[top_vars]
  final_formula <- as.formula(paste("y ~", paste(selected_cols, collapse = " + ")))
  final_model <- glm(final_formula, data = as.data.frame(cbind(y, x)), family = binomial)
  
  # Return results
  return(list(
    varsel_uni = varsel_uni, 
    model = final_model
  ))
}
