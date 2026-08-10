stepwise_pvalue <- function(data, response, direction = "stepwise_backward", p_threshold = 0.05) {
  
  # Initialize model based on the direction
  if (direction == "forward" || direction == "stepwise_forward") {
    # Start with the null model for forward selection
    model <- glm(as.formula(paste(response, "~ 1")), data = data, family = binomial)
    predictors <- setdiff(names(data), response)
  } else if (direction == "backward" || direction == "stepwise_backward") {
    # Start with the full model for backward elimination
    current_formula <- as.formula(paste(response, "~ ."))
    model <- glm(current_formula, data = data, family = binomial)
    predictors <- setdiff(names(data), response)
  } else {
    stop("Invalid direction. Choose 'forward', 'backward', 'stepwise_forward', or 'stepwise_backward'.")
  }
  
  # Main loop for stepwise selection
  while (TRUE) {
    added <- FALSE
    removed <- FALSE
    
    # Forward Step: Add predictors with the lowest p-value if below threshold
    if (direction == "forward" || direction == "stepwise_forward" || direction == "stepwise_backward") {
      p_values <- sapply(predictors, function(pred) {
        temp_model <- glm(as.formula(paste(response, "~", paste(c(all.vars(formula(model)), pred), collapse = " + "))),
                          data = data, family = binomial)
        # Extract p-value for the predictor
        pval <- summary(temp_model)$coefficients[grep(paste0("^", pred, "$"), rownames(summary(temp_model)$coefficients)), 4]
        if (length(pval) == 0) NA else pval  # Return NA if predictor is not found
      })
      
      # Remove NAs and check minimum p-value
      p_values <- unlist(p_values)  # Convert to numeric vector
      min_pval <- min(p_values, na.rm = TRUE)
      
      # If there is a predictor to add with p-value < threshold
      if (min_pval < p_threshold) {
        best_predictor <- names(which.min(p_values))
        model <- glm(as.formula(paste(response, "~", paste(c(all.vars(formula(model)), best_predictor), collapse = " + "))),
                     data = data, family = binomial)
        predictors <- setdiff(predictors, best_predictor)
        added <- TRUE
      }
    }
    
    # Backward Step: Remove predictors with p-value > threshold
    if (direction == "backward" || direction == "stepwise_backward" || direction == "stepwise_forward") {
      p_values <- summary(model)$coefficients[-1, 4]  # Exclude intercept
      names(p_values) <- rownames(summary(model)$coefficients)[-1]  # Get predictor names
      max_pval <- max(p_values, na.rm = TRUE)
      
      # If there is a predictor to remove with p-value > threshold
      if (max_pval > p_threshold) {
        worst_predictor <- names(which.max(p_values))
        current_formula <- as.formula(paste(response, "~", paste(setdiff(all.vars(formula(model)), worst_predictor), collapse = " + ")))
        model <- glm(current_formula, data = data, family = binomial)
        predictors <- setdiff(predictors, worst_predictor)
        removed <- TRUE
      }
    }
    
    # Stop if no further changes can be made
    if (!added && !removed) {
      break
    }
  }
  
  return(model)
}
