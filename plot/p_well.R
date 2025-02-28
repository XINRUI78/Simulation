# Function to calculate the probability of being well-calibrated (CS between 0.9 and 1.1)
calculate_p_well_cal <- function(data) {
  # Indicator function I(CS >= 0.9 & CS <= 1.1)
  n_sim <- length(data)
  well_cal_count <- sum(data >= 0.9 & data <= 1.1)
  
  # Calculate probability
  p_well_cal <- well_cal_count / n_sim
  return(p_well_cal)
}

# Apply the p_well_cal calculation for each combination of 'size' and 'method'
p_well_cal_results <- cs %>%
  group_by(size, method) %>%
  summarise(p_well_cal = calculate_p_well_cal(slope))

p_well_cal_results_filtered <- p_well_cal_results %>%
  filter(method != "LASSO1se")

# Get the RMSD value for MLE at "N" sample size
mle_p_well_n <- p_well_cal_results_filtered %>%
  filter(method == "MLE" & size == "N") %>%
  pull(p_well_cal)


# Print the p_well_cal results
print(p_well_cal_results)

# Create the line plot for p_well_cal
line_gg <- ggplot(p_well_cal_results, aes(x = size, y = p_well_cal, group = method, color = as.factor(method))) +
  geom_line(linewidth = 1) +
  labs(
    x = "Sample Size",
    y = "Probability of CS in [0.9, 1.1]",
    color = "Method"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),  # Panel background white
    plot.background = element_rect(fill = "white", color = NA),   # Entire plot background white
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),  # Black border
    axis.ticks = element_line(color = "black", linewidth = 0.5),
    axis.text = element_text(size = 10, color = "black"),
    legend.background = element_rect(fill = "white", color = NA),  # Legend background white
    legend.key = element_rect(fill = "white", color = NA),         # Legend key background white
    legend.position = "bottom",                                    # Move legend to the bottom
    plot.title = element_text(hjust = 0.5, size = 10, face = "bold")  # Centered title
  ) +
  scale_color_manual(values = c("azure4", "wheat","gold",
                                "darkorange", "darkgoldenrod", "seagreen2",  "mediumturquoise",  
                                "plum1", "hotpink1", "red2", "#BF79D6", "darkorchid1", "royalblue2", "slateblue2"
  ))

# Print the plot
print(line_gg)

# Save the plot
ggsave(
  "p_well_cal_sample_size_plot_example.png",
  plot = line_gg,
  width = 7.5,    # Set the width to a narrow value (in inches)
  height = 7,     # Adjust the height accordingly
  dpi = 600
)
