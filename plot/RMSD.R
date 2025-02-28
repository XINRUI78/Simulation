summary <- as.data.frame(result)
summary1 <- as.data.frame(result1)
summary2 <- as.data.frame(result2)
combine <- rbind(summary, summary1, summary2)

cs <- data.frame(
  size = rep(c("N", "3N/4", "N/2"), each = 14000),
  slope = combine$`calibration slope`,
  method = rep(c("MLE", "BE0.05", "BE0.15", "Uni0.05", "Uni0.15", "Uni0.05-BE", "Uni0.15-BE", "LASSOmin", 
                     "LASSO1se", "LASSOmin-MLE", "LASSO1se-MLE", "LASSOmin-BE", "LASSO1se-BE", "modifiedLASSO"), 3000)
)
cs$size <- factor(cs$size, levels = c("N/2", "3N/4", "N"))
cs$method <- factor(cs$method, levels = c("MLE", "BE0.05", "BE0.15", "Uni0.05", "Uni0.15", "Uni0.05-BE", "Uni0.15-BE", "LASSOmin", 
                                          "LASSO1se", "LASSOmin-MLE", "LASSO1se-MLE", "LASSOmin-BE", "LASSO1se-BE", "modifiedLASSO"))
# Define a function to calculate RMSD for each sample size
calculate_RMSD <- function(data) {
  target <- 1  # Target value of CS (for perfectly calibrated model)
  # Calculate RMSD for the given subset of data
  RMSD <- sqrt(mean((log(data) - log(target))^2))
  return(RMSD)
}

# Apply the RMSD calculation for each combination of 'size' and 'method'
RMSD_results <- cs %>%
  group_by(size, method) %>%
  summarise(RMSD_value = calculate_RMSD(slope))

# Load necessary libraries
library(ggplot2)
library(dplyr)

# Filter out the "LASSO1se" method
RMSD_results_filtered <- RMSD_results %>%
  filter(method != "LASSO1se")

# Get the RMSD value for MLE at "N" sample size
mle_rmsd_n <- RMSD_results_filtered %>%
  filter(method == "MLE" & size == "N") %>%
  pull(RMSD_value)

# Create the line plot
line_gg <- ggplot(RMSD_results_filtered, aes(x = size, y = RMSD_value, group = method, color = as.factor(method))) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = mle_rmsd_n, linetype = "dashed", color = "black", linewidth = 1) +
  labs(
    x = "Sample Size",
    y = "RMSD log(Calibration Slope)",
    color = "Method",
    title = "RMSD for Different Variable Selection Methods Across Sample Sizes"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA), # Panel background white
    plot.background = element_rect(fill = "white", color = NA),  # Entire plot background white
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5), # Black border
    axis.ticks = element_line(color = "black", linewidth = 0.5),
    axis.text = element_text(size = 10, color = "black"),
    legend.background = element_rect(fill = "white", color = NA), # Legend background white
    legend.key = element_rect(fill = "white", color = NA),        # Legend key background white
    legend.position = "bottom",                                   # Move legend to the bottom
    plot.title = element_text(hjust = 0.5, size = 10, face = "bold") # Centered title
  ) +
  scale_color_manual(values = c("azure4", "wheat","gold",
                               "darkorange", "darkgoldenrod", "seagreen2",  "mediumturquoise",  
                               "plum1", "red2", "#BF79D6", "darkorchid1", "royalblue2", "slateblue2"
  ))
# Print the plot
print(line_gg)

