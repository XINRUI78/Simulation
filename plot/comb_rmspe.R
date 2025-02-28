# Combine different sample sizes in one boxplot, sample size as the x-axis

comb_rmspe <- data.frame(
  size <- rep(c("N", "3N/4", "N/2"), each = 14000),
  rmspe = combine$rmspe,
  method = rep(c("MLE", "BE0.05", "BE0.15", "Uni0.05", "Uni0.15", "Uni0.05-BE", "Uni0.15-BE", "LASSOmin", 
                 "LASSO1se", "LASSOmin-MLE", "LASSO1se-MLE", "LASSOmin-BE", "LASSO1se-BE", "modifiedLASSO"), 3000)
)
comb_rmspe$size <- factor(comb_rmspe$size, levels = c("N/2", "3N/4", "N"))
comb_rmspe$method <- factor(comb_rmspe$method, levels = c("MLE", "BE0.05", "BE0.15", "Uni0.05", "Uni0.15", "Uni0.05-BE", "Uni0.15-BE", "LASSOmin", 
                                                          "LASSO1se", "LASSOmin-MLE", "LASSO1se-MLE", "LASSOmin-BE", "LASSO1se-BE", "modifiedLASSO"))

rmspe_filtered <- comb_rmspe %>%
  filter(method != "LASSO1se")

comb_rmspe_plot <- ggplot(rmspe_filtered, aes(x = size, y = rmspe, fill = method)) +
  geom_boxplot(
    width = 0.75,
    position = position_dodge(0.83),
    outlier.size = 1,
    outlier.shape = 16
  ) +
  labs(
    x = "Sample size",
    y = "RMSPE",
    title = "Estimated RMSPE given by different variable selection methods\nPrevalence = 0.3, C-statistic = 0.8, N = 1505, Number of predictors = 30"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),  # Panel background white
    plot.background = element_rect(fill = "white", color = NA),   # Entire plot background white
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),  # Black border
    axis.ticks = element_line(color = "black", size = 0.5),                      
    axis.text = element_text(size = 11, color = "black"),
    axis.title.x = element_text(margin = margin(t = 10)), 
    legend.background = element_rect(fill = "white", color = NA),  # Legend background white
    legend.key = element_rect(fill = "white", color = NA),         # Legend key background white
    legend.position = "bottom",                                   # Legend below plot
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold")  # Centered title
  ) +
  scale_fill_manual(values = c("azure4", "wheat","gold",
                               "darkorange", "darkgoldenrod", "seagreen2",  "mediumturquoise",  
                               "plum1", "red2", "#BF79D6", "darkorchid1", "royalblue2", "slateblue2"
  ))

ggsave(
  "rmspe(not).png",
  plot = comb_rmspe_plot,
  width = 7,    # Set the width to a narrow value (in inches)
  height = 6,   # Adjust the height accordingly
  dpi = 300
)