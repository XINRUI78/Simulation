library(ggplot2)
library(RColorBrewer)  # For distinct color palettes
library(dplyr)
# Ensure 'result' exists and has enough columns
# Create data frame
data <- data.frame(
  CS = result_df$`calibration slope`,
  AUC = result_df$auc,
  BS = result_df$`Brier score`,
  rmspe = result_df$rmspe,
  Method = as.factor(result_df$method),  # Convert to factor for coloring
  abscab = (log(result_df$`calibration slope`)-log(1))^2,
  Color = ifelse(result_df$`calibration slope` > 1, "yellow", "blue") 
) 

data$Method <- rep(c("MLE", "BE0.05", "BE0.15", "Uni0.05", "Uni0.15", "Uni0.05-BE", "Uni0.15-BE", "LASSOmin", "LASSO1se", "LASSOmin-MLE", "LASSO1se-MLE", 
                 "LASSOmin-BE", "LASSO1se-BE", "modifiedLASSO", "Uni15", "BE15", "LASSO15", "Random Forest", "Random Forest15"), 1000)
data$Method <- factor(data$Method, levels = c("MLE", "BE0.05", "BE0.15", "Uni0.05", "Uni0.15", "Uni0.05-BE", "Uni0.15-BE", "LASSOmin", "LASSO1se", "LASSOmin-MLE", "LASSO1se-MLE", 
                                                          "LASSOmin-BE", "LASSO1se-BE", "modifiedLASSO", "Uni15", "BE15", "LASSO15", "Random Forest", "Random Forest15"))

# Compute Pearson correlation for each method
correlation_data <- data %>%
  group_by(Method) %>%
  summarise(Correlation = cor(CS, AUC, method = "pearson", use = "complete.obs")) %>%
  mutate(Correlation_Label = paste0("r = ", round(Correlation, 2)))  # Format correlation text

# Merge correlation data with main dataset
data1 <- left_join(data, correlation_data, by = "Method")

# Scatter plot with centered title and top-right correlation label
CSAUC <- ggplot(data1, aes(x = CS, y = AUC, color = "black")) +  
  geom_point(size = 1) +  # Smaller points for clarity
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # Add trend line
  facet_wrap(~ Method, nrow = 4, ncol = 5) +  # 4x5 Grid
  labs(
    x = "Calibration Slope",
    y = "C-Statistic",
    title = "C-Statistic vs Calibration Slope for each method"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),  # Panel background white
    plot.background = element_rect(fill = "white", color = NA),   # Entire plot background white
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),  # Black border
    axis.ticks = element_line(color = "black", linewidth = 0.5),
    axis.text = element_text(size = 10, color = "black"),
    legend.position = "none",  # Remove the legend from this plot
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
    strip.text = element_text(size = 11)  # Adjust facet labels size
  ) +
  scale_x_continuous(breaks = seq(0.8, 2, by = 0.2)) + 
  scale_color_identity() +  # Keep predefined blue/yellow color
  # Add correlation text in the top-right corner of each facet
  geom_text(aes(x = max(CS, na.rm = TRUE),  
                y = max(AUC, na.rm = TRUE),  
                label = Correlation_Label), 
            hjust = 1, vjust = 1, size = 4, color = "black")  # Align to top-right

ggsave(
  "CS_AUC.png",
  plot = CSAUC,
  width = 11,    # Set the width to a narrow value (in inches)
  height = 6,   # Adjust the height accordingly
  dpi = 300
)


############################
data <- data.frame(
  CalibrationSlope = result_df$`calibration slope`,
  AUC = result_df$auc,
  BS = result_df$`Brier score`,
  rmspe = result_df$rmspe,
  Method = as.factor(result_df$method),  # Convert to factor for coloring
  abscab = (log(result_df$`calibration slope`)-log(1))^2) %>%
  filter(CalibrationSlope > 1) 

# Create scatter plot
ggplot(data, aes(x = CS, y = AUC, color = Color)) +
  geom_point(size = 3) +  # Points
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # Trend line
  facet_wrap(~ Method, nrow = 4, ncol = 5) +  # 4x5 Grid
  labs(
    x = "Calibration Slope",
    y = "AUC (C-statistic)",
    title = "Scatter Plots for Each Method"
  ) +
  scale_color_identity() +
  theme_minimal()

# Create scatter plot
ggplot(data, aes(x = abscab, y = AUC, color = "black")) +
  geom_point(size = 3) +  # Points
  geom_smooth(method = "lm", color = "red", se = FALSE) +  # Trend line
  facet_wrap(~ Method, nrow = 4, ncol = 5) +  # 4x5 Grid
  labs(
    x = "Calibration Slope",
    y = "AUC (C-statistic)",
    title = "Scatter Plots for Each Method"
  ) +
  scale_color_identity() +
  theme_minimal()
#######################
colnames(p_this) <- c("MLE", "BE0.05", "BE0.15", "Uni0.05", "Uni0.15", "Uni0.05-BE", "Uni0.15-BE", "LASSOmin", "LASSO1se", "LASSOmin-MLE", "LASSO1se-MLE", 
                     "LASSOmin-BE", "LASSO1se-BE", "modifiedLASSO", "Uni15", "BE15", "LASSO15", "Random Forest", "Random Forest15", "X")

data_long <- as.data.frame(p_this) %>%
  pivot_longer(cols = 1:19, names_to = "Metric", values_to = "Value") %>%
  mutate(Metric = factor(Metric, levels = c("MLE", "BE0.05", "BE0.15", "Uni0.05", "Uni0.15", "Uni0.05-BE", "Uni0.15-BE", "LASSOmin", "LASSO1se", "LASSOmin-MLE", "LASSO1se-MLE", 
                                            "LASSOmin-BE", "LASSO1se-BE", "modifiedLASSO", "Uni15", "BE15", "LASSO15", "Random Forest", "Random Forest15")))  # Force numeric order

# Create scatter plot with correct ordering
test <- ggplot(data_long, aes(x = X, y = Value)) +  
  geom_point(size = 1, color = "black") +  # Points
  geom_abline(slope = 1, intercept = 0, color = "red") +  # Y = X line
  facet_wrap(~ Metric, nrow = 4, ncol = 5, scales = "free_y") +  # 4x5 grid, correct order
  labs(
    x = "Actual Probability",
    y = "Predicted Probability",
    title = "Predicted Probability vs Actual Probability for each method"
  ) +  
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),  # Panel background white
    plot.background = element_rect(fill = "white", color = NA),   # Entire plot background white
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),  # Black border
    axis.ticks = element_line(color = "black", linewidth = 0.5),
    axis.text = element_text(size = 10, color = "black"),
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
  ) 

ggsave(
  "true_predicted.png",
  plot = test,
  width = 11,    # Set the width to a narrow value (in inches)
  height = 8,   # Adjust the height accordingly
  dpi = 300
)

# Fit linear models and extract slope + deviation statistics
model_stats <- data_long %>%
  group_by(Metric) %>%
  summarise(
    Slope = coef(lm(Value ~ X))[2],  # Extract slope from lm()
    RSE = summary(lm(Value ~ X))$sigma,  # Residual standard error
    R2 = summary(lm(Value ~ X))$r.squared,  # R-squared
    MAE = mean(abs(residuals(lm(Value ~ X)))),  # Mean Absolute Error
    abslope = abs(coef(lm(Value ~ X))[2]-1)
  ) %>%
  arrange(as.numeric(gsub("Y_", "", Metric)))  # Ensure numerical ordering