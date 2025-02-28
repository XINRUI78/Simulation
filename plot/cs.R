cs <- data.frame(
  size = rep("N", 19000),
  slope = summary$`calibration slope`,
  method = rep(c("MLE", "BE0.05", "BE0.15", "Uni0.05", "Uni0.15", "Uni0.05-BE", "Uni0.15-BE", "LASSOmin", 
                 "LASSO1se", "LASSOmin-MLE", "LASSO1se-MLE", "LASSOmin-BE", "LASSO1se-BE", "modifiedLASSO", 
                 "Uni15", "BE15", "LASSO15", "Random Forest", "Random Forest15"), 1000)
)

cs$method <- factor(cs$method, levels = c("MLE", "BE0.05", "BE0.15", "BE15", "Uni0.05", "Uni0.15","Uni15", "Uni0.05-BE", "Uni0.15-BE", "LASSOmin", 
                                                   "LASSO15", "LASSO1se", "LASSOmin-MLE", "LASSO1se-MLE", "LASSOmin-BE", "LASSO1se-BE", "modifiedLASSO", 
                                                   "Random Forest", "Random Forest15"))

cs_filtered <- cs %>%
  filter(!method %in% c("LASSO1se", "Random Forest", "Random Forest15"))
#0.75, 0.8
comb_cs_plot <- ggplot(cs_filtered, aes(x = size, y = slope, fill = method)) +
  geom_boxplot(
    width = 0.75,
    position = position_dodge(0.83),
    outlier.size = 1,
    outlier.shape = 16
  ) +
  geom_hline(
    yintercept = 1,         # Add horizontal line at y = 1
    linetype = "dashed",    # Dashed line
    color = "black",        # Line color
    linewidth = 0.3         # Line thickness (updated from size to linewidth)
  ) +
  labs(
    x = "Sample size",
    y = "Calibration slope",
    title = "Estimated calibration slope given by different variable selection methods at recommended sample size\nPrevalence = 0.3, C-statistic = 0.8, N = 1505, Number of predictors = 30, \nPercentage of each type of predictors = (0.1, 0.2, 0.2, 0.5)"
  ) +
  scale_y_continuous(
    breaks = c(0.8, 1, 1.2, 1.4, 1.6) # Define where the ticks should be
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),  # Panel background white
    plot.background = element_rect(fill = "white", color = NA),   # Entire plot background white
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),  # Black border
    axis.ticks = element_line(color = "black", size = 0.5),                      
    axis.text = element_text(size = 11, color = "black"),
    legend.background = element_rect(fill = "white", color = NA),  # Legend background white
    legend.key = element_rect(fill = "white", color = NA),         # Legend key background white
    plot.title = element_text(hjust = 0.5, size = 12, face = "bold")  # Centered title
  ) +
  scale_fill_manual(values = c("azure4", "wheat","gold","coral2", "darkorange", "darkgoldenrod", "brown4", 
                               "seagreen2", "mediumturquoise", "plum1", "blue", "red2", 
                               "#BF79D6", "darkorchid1", "royalblue2", "slateblue2"
                               ))
  scale_fill_manual(values = c("azure4", "wheat","gold","coral2", "darkorange", "darkgoldenrod", "brown4", 
                               "seagreen2", "mediumturquoise", "plum1", "blue", "hotpink1", "red2", 
                               "#BF79D6", "darkorchid1", "royalblue2", "slateblue2", 
                               "cyan", "cyan4"))


ggsave(
  "cs19_3.png",
  plot = comb_cs_plot,
  width = 10,    # Set the width to a narrow value (in inches)
  height = 6,   # Adjust the height accordingly
  dpi = 300
)