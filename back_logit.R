# select variables using backward elimination 
# crit: "AIC", "BIC", "p_value"

back_logit <- function(data, response, crit, p_threshold) {
 
  # Create the full model
  full_model <- glm(as.formula(paste(response, "~ .")), data = data, family = binomial)
  
  # Perform backward selection
  if (crit == "AIC" || crit == "BIC") {
    # Stepwise selection based on AIC or BIC
    final_model <- step(full_model, direction = "backward", k = ifelse(crit == "AIC", 2, log(nrow(data))))
  } else if (crit == "p_value") {
    # backward elimination based on p-value threshold
    final_model <- backward_pvalue(data, response, p_threshold)
    }
    else {
    stop("Invalid criterion. Use 'AIC', 'BIC', or 'p_value'.")
  }
  
  # Get the final predictors
  back_predictors <- names(coef(final_model))[-1] # Exclude the intercept
  
  # Create the indicator row for variable retention
  all_predictors <- setdiff(names(data), response)
  indicator <- ifelse(all_predictors %in% back_predictors, 1, 0)
  
  # Return the final model and indicator row
  return(list(
    varsel_back = indicator,
    backmodel = final_model
  ))
}

# backward elimination based on p-value has no existing package
backward_pvalue <- function(data, response, p_threshold) {
  
  # Start with the full model for backward elimination
  current_formula <- as.formula(paste(response, "~ ."))
  model <- glm(current_formula, data = data, family = binomial)
  predictors <- setdiff(names(data), response)
  
  # Main loop for stepwise selection
  while (TRUE) {
    removed <- FALSE
    
    # Backward Step: Remove predictors with p-value > threshold
      p_values <- summary(model)$coefficients[-1, 4]  # Exclude intercept
      names(p_values) <- rownames(summary(model)$coefficients)[-1]  # Get predictor names
      max_pval <- max(p_values, na.rm = TRUE)
      
      # If there is a predictor to remove with p-value > threshold
      if (max_pval > p_threshold) {
        worst_predictor <- names(which.max(p_values))
        current_formula <- as.formula(paste(response, "~", paste(setdiff(predictors, worst_predictor), collapse = " + ")))
        model <- glm(current_formula, data = data, family = binomial)
        predictors <- setdiff(predictors, worst_predictor)
        removed <- TRUE
      }
    
    # Stop if no further changes can be made
    if (!removed) {
      break
    }
  }
  
  return(model)
}
