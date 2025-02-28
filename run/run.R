######### Set simulation parameters
n.para <- 30
prev <- 0.3
c <- 0.8
nval <- 10000
percentage <- c(0.1, 0.2, 0.2, 0.5)

# Define number of predictors with relative strengths
strong <- percentage[1] * n.para  # 10 strong predictive variables
medium <- percentage[2] * n.para  # 30% medium predictive variables
weak <- percentage[3] * n.para    # 30% weak predictive variables
noise <- percentage[4] * n.para   # 30% noise predictive variables

# Assign relative strengths
weights <- c(rep(1, strong), 
             rep(0.5, medium), 
             rep(0.25, weak), 
             rep(0, noise))

# recommended sample size
install.packages("devtools")
require("devtools")
devtools::install_github("mpavlou/samplesizedev")
require(samplesizedev)
library(samplesizedev)

rss <- samplesizedev(outcome = "Binary", S = 0.9, phi = prev, c = c, p = n.para)
ndev <- rss$sim
ndev1 <- round(ndev/4*3)
ndev2 <- round(ndev/2)
######### obtain the coefficents of beta
opt_beta <- opt_beta(n.para, prev, c, weights)
beta0 <- opt_beta$beta0
beta <- opt_beta$beta1

result <- perform(ndev, n.para, beta0, beta, nval)
result1 <- perform(ndev1, n.para, beta0, beta, nval)
result2 <- perform(ndev2, n.para, beta0, beta, nval)
# Convert the matrix to a data frame
summary <- as.data.frame(result)
summary1 <- as.data.frame(result1)
summary2 <- as.data.frame(result2)
combine <- rbind(summary, summary1, summary2)

# Load necessary library
library(dplyr)
strong_predictors <- paste0("varsel", 1:strong)  # 10% strong predictors
medium_predictors <- paste0("varsel", (strong  + 1):(strong + medium))  # Next 30% medium predictors
weak_predictors <- paste0("varsel", (strong + medium + 1):(strong + medium + weak))  # Next 30% weak predictors
noise_predictors <- paste0("varsel", (strong + medium + weak + 1):n.para)  # Final 30% noise predictors

# Group by 'method' and calculate the mean for each metric
summary_table <- summary %>%
  group_by(method) %>%
  summarize(
    mean_auc = mean(auc, na.rm = TRUE),
    mean_rmspe = mean(rmspe, na.rm = TRUE),
    mean_calibration_slope = mean(`calibration slope`, na.rm = TRUE),
    mean_calibration_in_large = mean(`calibration in the large`, na.rm = TRUE),
    mean_brier_score = mean(`Brier score`, na.rm = TRUE)
  )

# Print the summarized table
print(summary_table)

pre_result2422 <- summary_2422 %>%
  group_by(method) %>%
  summarize(
    strong_mean = mean(rowMeans(across(all_of(strong_predictors)), na.rm = TRUE)),
    medium_mean = mean(rowMeans(across(all_of(medium_predictors)), na.rm = TRUE)),
    weak_mean = mean(rowMeans(across(all_of(weak_predictors)), na.rm = TRUE)),
    noise_mean = mean(rowMeans(across(all_of(noise_predictors)), na.rm = TRUE))
  )

pre_result1 <- summary %>%
  mutate(
    strong_mean = rowMeans(summary[, strong_predictors], na.rm = TRUE),
    medium_mean = rowMeans(summary[, medium_predictors], na.rm = TRUE),
    weak_mean = rowMeans(summary[, weak_predictors], na.rm = TRUE),
    noise_mean = rowMeans(summary[, noise_predictors], na.rm = TRUE)
  )
pre_result <- pre_result[-c(7,8),]
pre_result$method <- c("Uni0.05", "Uni0.1", "Uni0.05-BE", "Uni0.1-BE", "LASSOmin", 
               "LASSO1se", "LASSOmin-BE", "LASSO1se-BE")
pre_result$method <- factor(pre_result$method, levels = c("Uni0.05", "Uni0.1", "Uni0.05-BE", "Uni0.1-BE", "LASSOmin", 
                                   "LASSO1se", "LASSOmin-BE", "LASSO1se-BE"))