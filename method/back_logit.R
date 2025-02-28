back_logit <- function(data, response, crit, p_threshold) {
 
  # Convert response to a factor for logistic regression
  data[[response]] <- as.factor(data[[response]])
  
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
    stop("Invalid criterion. Use 'AIC', 'BIC', or 'p'.")
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

