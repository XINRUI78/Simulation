###################################
plot_rmsd <- ggplot(RMSD_results_filtered, aes(x = size, y = RMSD_value, group = method, color = as.factor(method))) +
  geom_line(linewidth = 1) + 
  geom_hline(yintercept = mle_rmsd_n, linetype = "dashed", color = "black", linewidth = 1) + 
  labs(
    x = "Sample Size",
    y = "RMSD log(Calibration Slope)",
    color = "Method"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),  # Panel background white
    plot.background = element_rect(fill = "white", color = NA),   # Entire plot background white
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),  # Black border
    axis.ticks = element_line(color = "black", linewidth = 0.5),
    axis.text = element_text(size = 10, color = "black"),
    legend.position = "none",  # Remove the legend from this plot
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold")
  ) +
  scale_color_manual(values = c("azure4", "wheat","gold", "darkorange", "darkgoldenrod", 
                                "seagreen2", "mediumturquoise", "plum1", "red2", 
                                "#BF79D6", "darkorchid1", "royalblue2", "slateblue2")) 


# Create the second plot for probability of well-calibrated models with a legend
plot_prob <- ggplot(p_well_cal_results_filtered, aes(x = size, y = p_well_cal, group = method, color = as.factor(method))) + 
  geom_line(linewidth = 1) + 
  geom_hline(yintercept = mle_p_well_n, linetype = "dashed", color = "black", linewidth = 1) + 
  labs(
    x = "Sample Size",
    y = "Probability of CS in [0.9, 1.1]",
    color = "Method",
  ) + 
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
    axis.ticks = element_line(color = "black", linewidth = 0.5),
    axis.text = element_text(size = 10, color = "black"),
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
    legend.position = "bottom"  # Position the legend at the bottom
  ) +
  scale_color_manual(values = c("azure4", "wheat","gold", "darkorange", "darkgoldenrod", 
                                "seagreen2", "mediumturquoise", "plum1", "red2", 
                                "#BF79D6", "darkorchid1", "royalblue2", "slateblue2"))

# Combine the two plots horizontally and manually add the shared legend at the bottom
combined_plot <- plot_rmsd + plot_prob + plot_layout(guides = "collect") 
combined_plot <- combined_plot + 
  plot_annotation(
    title = "Combining bias and variability in the estimated calibration slope", 
    subtitle = "Prevalence = 0.3, C-statistic = 0.8, N = 1505",
    theme = theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 14, face = "bold")
    )
  ) & 
  theme(legend.position = "bottom")  # Place legend at the bottom for both plots

# Print the combined plot
print(combined_plot)

ggsave(
  "comb_cs.png",
  plot = combined_plot,
  width = 10,    # Set the width to a narrow value (in inches)
  height = 6,   # Adjust the height accordingly
  dpi = 300
)