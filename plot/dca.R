install.packages("dcurves")

# install other packages used in this tutorial
install.packages(
  c("tidyverse", "survival", "gt", "broom",
    "gtsummary", "rsample", "labelled")
)

# load packages
library(dcurves)
library(tidyverse)
library(gtsummary)
# Convert matrix to dataframe

my_data <- as.data.frame(cbind(p_this[,-20], yval))
# Rename columns: first 19 as Treatment 1-19, last column as Outcome
colnames(my_data) <- c("MLE", "BE0.05", "BE0.15", "Uni0.05", "Uni0.15", "Uni0.05-BE", "Uni0.15-BE", "LASSOmin", "LASSO1se", "LASSOmin-MLE", "LASSO1se-MLE", 
                         "LASSOmin-BE", "LASSO1se-BE", "modifiedLASSO", "Uni15", "BE15", "LASSO15", "Random Forest", "Random Forest15", "Outcome")

# Run Decision Curve Analysis for all treatments
dca_results <- dca(Outcome ~ ., 
                   data = my_data)  # Includes all 19 treatments

# Plot Decision Curve Analysis
plot(dca_results)

# Extract net benefit data

filtered_dca <- dca_results$dca %>%
  filter(!variable %in% c("none", "all", "Random Forest", "Random Forest15"), threshold >= 0 & threshold <= 0.5) %>% # Also limit threshold to 0-25%
  mutate(variable = factor(variable, levels = c("MLE", "BE0.05", "BE0.15", "Uni0.05", "Uni0.15", "Uni0.05-BE", "Uni0.15-BE", "LASSOmin", "LASSO1se", "LASSOmin-MLE", "LASSO1se-MLE", 
                                                "LASSOmin-BE", "LASSO1se-BE", "modifiedLASSO", "Uni15", "BE15", "LASSO15"))) 
filtered_dca <- dca_results$dca %>%
  filter(variable %in% c("BE0.15", "MLE"), threshold >= 0 & threshold <= 0.5) %>% # Also limit threshold to 0-25%
  mutate(variable = factor(variable, levels = c("BE0.15", "MLE")))

# Plot Decision Curve Analysis without "Treat None"
ggplot(filtered_dca, aes(x = threshold, y = net_benefit, color = variable)) +
  geom_line(size = 1) +  # Plot decision curves
  labs(
    x = "Threshold Probability",
    y = "Net Benefit",
    title = "Decision Curve Analysis (Without Treat None, Thresholds: 0-10%)"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("black", "wheat","gold", "darkorange", "darkgoldenrod", 
                                "seagreen2", "mediumturquoise", "plum1", "hotpink1", "red2", 
                                "#BF79D6", "darkorchid1", "royalblue2", "slateblue2", "brown4", 
                                "coral2", "blue")) +
  scale_x_continuous(limits = c(0.05, 0.1)) +  # Restrict x-axis to 0-25%
scale_y_continuous(limits = c(0.22, 0.27))  # Restrict x-axis to 0-25%
#, "cyan", "cyan4"
########################
library(dcurves)
library(tidyverse)

# Extract model names (excluding MLE)
treatment_models <- c("BE0.05", "BE0.15", "Uni0.05", "Uni0.15", "Uni0.05-BE", "Uni0.15-BE", "LASSOmin", "LASSO1se", "LASSOmin-MLE", "LASSO1se-MLE", 
                      "LASSOmin-BE", "LASSO1se-BE", "modifiedLASSO", "Uni15", "BE15", "LASSO15")

# Create an empty list to store plots
plot_list <- list()

# Loop through each treatment model (excluding MLE)
for (model in treatment_models) {
  
  formula_str <- paste("Outcome ~ MLE + `", model, "`", sep = "")  
  formula <- as.formula(formula_str)  # Convert to formula
  # Run Decision Curve Analysis for MLE + one treatment
  dca_results <- dca(
    formula,  # Include MLE and one treatment at a time
    data = my_data
  )
  
  # Remove "Treat None" and "Treat All" from the dataset
  filtered_dca <- dca_results$dca %>%
    filter(!variable %in% c("none", "all"), threshold >= 0 & threshold <= 0.25) %>%
    mutate(variable = factor(variable, levels = c("MLE", model)))  # Ensure correct legend order
  
  # Generate DCA plot for this model
  p <- ggplot(filtered_dca, aes(x = threshold, y = net_benefit, color = variable)) +
    geom_line(size = 0.5) +
    labs(title = paste("DCA: MLE vs.", model)) +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", color = NA),  # Panel background white
      plot.background = element_rect(fill = "white", color = NA),   # Entire plot background white
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),  # Black border
      axis.ticks = element_line(color = "black", linewidth = 0.5),
      legend.position = "none",  # Remove the legend from this plot
      plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
     
    ) +
    scale_x_continuous(limits = c(0, 0.25)) +
    scale_color_manual(values = c("black", "blue"))  # MLE in black, treatment in blue
  
  # Store plot in the list
  plot_list[[model]] <- p
}

# Display all plots in a grid (if using RMarkdown)
library(patchwork)
final_plot <- wrap_plots(plot_list, nrow = 4, ncol = 4) +
  plot_annotation(
    title = "Decision Curve Analysis (DCA)",
    theme = theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    )
  ) &
  theme(
    axis.title.x = element_text(size = 12, face = "bold"),  # Shared x-label
    axis.title.y = element_text(size = 12, face = "bold")   # Shared y-label
  ) +
  labs(x = "Threshold Probability", y = "Net Benefit")  # Single shared x and y labels

# Print the final plot
print(final_plot)
# OR: Print each plot separately (if running in R console)
for (p in plot_list) {
  print(p)
}

