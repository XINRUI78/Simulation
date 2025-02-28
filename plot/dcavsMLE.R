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
library(ggpubr)  # For ggarrange()
library(grid)  # Load grid for textGrob()

my_data <- as.data.frame(cbind(p_this[,-20], yval))
# Rename columns: first 19 as Treatment 1-19, last column as Outcome
colnames(my_data) <- c("MLE", "BE0.05", "BE0.15", "Uni0.05", "Uni0.15", "Uni0.05-BE", "Uni0.15-BE", "LASSOmin", "LASSO1se", "LASSOmin-MLE", "LASSO1se-MLE", 
                       "LASSOmin-BE", "LASSO1se-BE", "modifiedLASSO", "Uni15", "BE15", "LASSO15", "Random Forest", "Random Forest15", "Outcome")

# Extract model names (excluding MLE)
treatment_models <- c("BE0.05", "BE0.15", "Uni0.05", "Uni0.15", "Uni0.05-BE", "Uni0.15-BE", "LASSOmin", "LASSO1se", "LASSOmin-MLE", "LASSO1se-MLE", 
                      "LASSOmin-BE", "LASSO1se-BE", "modifiedLASSO", "Uni15", "BE15", "LASSO15")
plot_list <- list()

# Loop through each treatment model (excluding MLE)
for (model in treatment_models) {
  
  # Create formula dynamically with backticks (`) for special characters
  formula_str <- paste("Outcome ~ MLE + `", model, "`", sep = "")  
  formula <- as.formula(formula_str)  # Convert to formula
  
  # Run Decision Curve Analysis for MLE + one treatment
  dca_results <- dca(
    formula,  
    data = my_data
  )
  
  # Extract and clean the DCA data
  filtered_dca <- dca_results$dca %>%
    filter(!variable %in% c("none", "all"), threshold >= 0.05 & threshold <= 0.25) %>%
    mutate(variable = factor(variable, levels = c("MLE", model)))  # Ensure correct legend order
  
  # Generate DCA plot for this model
  p <- ggplot(filtered_dca, aes(x = threshold, y = net_benefit, color = variable)) +
    geom_line(size = 0.5) +
    labs(title = paste("MLE vs", model)) +
    theme_minimal() +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
      axis.ticks = element_line(color = "black", linewidth = 0.5),
      axis.text = element_text(size = 10, color = "black"),
      legend.position = "none",  
      plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
      axis.title.x = element_blank(),  # Remove individual x labels
      axis.title.y = element_blank()   # Remove individual y labels
    ) +
    scale_x_continuous(limits = c(0.05, 0.25)) +
    scale_color_manual(values = c("black", "orange"))  
  
  # Store plot in the list
  plot_list[[model]] <- p
}


# Display all plots in a grid (if using RMarkdown)

final_plot <- ggarrange(plotlist = plot_list, 
                        ncol = 3, nrow = 6,  # Arrange in 4x4 grid
                        common.legend = TRUE,  # Shared legend
                        legend = "bottom")  # Position legend at bottom
final_plot_annotated <- annotate_figure(final_plot,
                left = textGrob("Net Benefit", rot = 90, vjust = 1, gp = gpar(fontsize = 12)),
                bottom = textGrob("Threshold Probability", vjust = -0.5, gp = gpar(fontsize = 12)),
                top = textGrob("Decision Curve Analysis (DCA)", gp = gpar(fontsize = 14, face = "bold"))) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),  # Ensure white background
    panel.background = element_rect(fill = "white", color = NA)  # Set panel background to white
  )

ggsave(
  "dca_1.png",
  plot = final_plot_annotated,
  width = 30,    # Set the width to a narrow value (in inches)
  height = 50,   # Adjust the height accordingly
  dpi = 300,
  limitsize = FALSE
)
