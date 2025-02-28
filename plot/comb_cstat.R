#
summary <- as.data.frame(result)
summary_50$fpara <- rowSums(summary_50[,10:39])
summary1_50 <- as.data.frame(result1_50)
summary2_50 <- as.data.frame(result2_50)
combine_50 <- rbind(summary_50, summary1_50, summary2_50)


#combine_sum, combine_sum1, combine_sum2 then combine_complete
combine_sum <- summary %>%
  group_by(method) %>%
  summarize(
    mean_auc = mean(auc, na.rm = TRUE),
    mean_rmspe = mean(rmspe, na.rm = TRUE),
    mean_calibration_slope = mean(`calibration slope`, na.rm = TRUE),
    mean_calibration_in_large = mean(`calibration in the large`, na.rm = TRUE),
    mean_brier_score = mean(`Brier score`, na.rm = TRUE)
  )
combine_complete <- rbind(combine_sum, combine_sum1, combine_sum2)
####################
comb_cstat <- data.frame(
  size <- rep(c("N", "3N/4", "N/2"), each = 14000),
  Cstatistic = combine$auc,
  method = rep(c("MLE", "BE0.05", "BE0.15", "Uni0.05", "Uni0.15", "Uni0.05-BE", "Uni0.15-BE", "LASSOmin", 
                 "LASSO1se", "LASSOmin-MLE", "LASSO1se-MLE", "LASSOmin-BE", "LASSO1se-BE", "modifiedLASSO"), 3000)
)
comb_cstat$size <- factor(comb_cstat$size, levels = c("N/2", "3N/4", "N"))
comb_cstat$method <- factor(comb_cstat$method, levels = c("MLE", "BE0.05", "BE0.15", "Uni0.05", "Uni0.15", "Uni0.05-BE", "Uni0.15-BE", "LASSOmin", 
                                                          "LASSO1se", "LASSOmin-MLE", "LASSO1se-MLE", "LASSOmin-BE", "LASSO1se-BE", "modifiedLASSO"))

cstat_filtered <- comb_cstat %>%
  filter(method != "LASSO1se")

comb_cstat_plot <- ggplot(cstat_filtered, aes(x = size, y = Cstatistic, fill = method)) +
  geom_boxplot(
    width = 0.75,
    position = position_dodge(0.83),
    outlier.size = 1,
    outlier.shape = 16
  ) +
  labs(
    x = "Sample size",
    y = "C-statistic",
    title = "Estimated c-statistic given by different variable selection methods\nPrevalence = 0.3, C-statistic = 0.8, N = 1505, Number of predictors = 30"
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
                                "plum1","red2", "#BF79D6", "darkorchid1", "royalblue2", "slateblue2"
  ))

ggsave(
  "cstat(not).png",
  plot = comb_cstat_plot,
  width = 7,    # Set the width to a narrow value (in inches)
  height = 6,   # Adjust the height accordingly
  dpi = 300
)