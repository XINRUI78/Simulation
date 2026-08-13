measures <- function(yval, p_val){

  # Prevent probabilities of exactly 0 or 1
  eps <- 1e-15
  p_val <- pmin(pmax(p_val, eps), 1 - eps)

  # Check for missing/non-finite predictions
  if (any(!is.finite(p_val))) {
    stop("p_val contains NA, NaN, or infinite values")
  }

  eta_val <- log(p_val/(1-p_val))
  
  # Calibration slope
  fitcal <- speedglm::speedglm(yval ~ eta_val, family = binomial())
  cal_slope <- as.vector(coef(fitcal)[2]) 
  
  # Calibration in the large
  off <- speedglm::speedglm(yval ~ 1, offset = eta_val, family = binomial())
  cal_large <- as.vector(coef(off))
  
  # AUC
  cstat <- pROC::roc(response = yval, predictor = as.vector(p_val), levels = c(0, 1), direction = "<")
  auc <- as.vector(cstat$auc)
  
  # Brier score
  brier <- mean((p_val - yval)^2)
  
  # Root mean square prediction error (RMSPE)
  rmspe <- sqrt(mean((p_val - yval)^2))
  
  return(c(cal_slope, cal_large, auc, brier, rmspe))
}
