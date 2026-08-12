lasso_exact <- function(x, y, xval, number, max_attempts = 10, initial_nlambda = 100) {
library(glmnet)
attempt <- 1
nlambda <- initial_nlambda
selected_lambda <- NULL

while (attempt <= max_attempts) {

  cv_lasso <- cv.glmnet(
    as.matrix(x),
    y,
    family = "binomial",
    alpha = 1,
    nfolds = 10,
    nlambda = nlambda
  )

  coef_matrix <- as.matrix(
    coef(cv_lasso$glmnet.fit)
  )[-1, , drop = FALSE]

  num_selected <- colSums(coef_matrix != 0)

  # Find lambdas selecting exactly 'number' predictors
  valid_indices <- which(num_selected == number)

  if (length(valid_indices) > 0) {

    # Among exact matches, choose lowest CV error
    best_index <- valid_indices[
      which.min(cv_lasso$cvm[valid_indices])
    ]

    selected_lambda <- cv_lasso$lambda[best_index]
    break

  } else {

    nlambda <- nlambda * 2
    attempt <- attempt + 1
  }
}

# Fallback after max_attempts:
# choose the closest smaller number of predictors,
# then lowest CV error among those models
if (is.null(selected_lambda)) {

  smaller_indices <- which(num_selected < number)

  closest_smaller <- max(num_selected[smaller_indices])

  candidate_indices <- which(
    num_selected == closest_smaller
  )

  best_index <- candidate_indices[
    which.min(cv_lasso$cvm[candidate_indices])
  ]

  selected_lambda <- cv_lasso$lambda[best_index]
}
  return(list(selected_lambda = selected_lambda))
}    


    
    
  
 
