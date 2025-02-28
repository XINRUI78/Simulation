# Prevalence = 0.3, C-statistic = 0.8, N = 2000"
all_predictors <- paste0("varsel", 1:n.para)
strong_predictors <- paste0("varsel", 1:strong)  # 10% strong predictors
medium_predictors <- paste0("varsel", (strong  + 1):(strong + medium))  # Next 30% medium predictors
weak_predictors <- paste0("varsel", (strong + medium + 1):(strong + medium + weak))  # Next 30% weak predictors
noise_predictors <- paste0("varsel", (strong + medium + weak + 1):n.para)  # Final 30% noise predictors


line_gg <- function(summary){

library(ggplot2)
library(tidyr)
library(dplyr)

#### change summary_ and ggsave("_ggg.png")
pre_result <- summary %>%
  group_by(method) %>%
  summarize(
    strong_mean = mean(rowMeans(across(all_of(strong_predictors)), na.rm = TRUE)),
    medium_mean = mean(rowMeans(across(all_of(medium_predictors)), na.rm = TRUE)),
    weak_mean = mean(rowMeans(across(all_of(weak_predictors)), na.rm = TRUE)),
    noise_mean = mean(rowMeans(across(all_of(noise_predictors)), na.rm = TRUE)))
  

pre_result <- pre_result[-c(10,11),]
pre_result$method <- c("MLE", "BE0.05", "BE0.15", "Uni0.05", "Uni0.15", "Uni0.05-BE", "Uni0.15-BE", "LASSOmin", "LASSO1se", "LASSOmin-BE",
                       "LASSO1se-BE", "modifiedLASSO")
pre_result$method <- factor(pre_result$method, levels = c("MLE", "BE0.05", "BE0.15", "Uni0.05", "Uni0.15", "Uni0.05-BE", "Uni0.15-BE", "LASSOmin", "LASSO1se", 
                                                          "LASSOmin-BE", "LASSO1se-BE", "modifiedLASSO"))
  
pre_result_long <- pre_result %>%
  pivot_longer(
    cols = c(noise_mean, weak_mean, medium_mean, strong_mean),
    names_to = "Predictor_Type",
    values_to = "Mean_Value"
  ) %>%
  mutate(
    Predictor_Type = factor(Predictor_Type, 
                            levels = c("noise_mean", "weak_mean", "medium_mean", "strong_mean"),
                            labels = c("Noise", "Weak", "Medium", "Strong"))
  )

# Create the line plot
line <-ggplot(pre_result_long, aes(x = Predictor_Type, y = Mean_Value, group = method, color = as.factor(method))) +
  geom_line(linewidth = 1) +
  labs(
    x = "Predictive strength of predictor variables",
    y = "Probability of predictors being selected",
    color = "Method"
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
                                "plum1", "hotpink1", "darkorchid1", "royalblue2", "slateblue2"
  )) 
  return(line)
}



# Load necessary libraries
library(ggplot2)
library(dplyr)
library(patchwork)  # For combining plots

# Assuming summary, summary1, and summary2 are already defined and contain your data


plot1 <- line_gg(summary) + labs(caption = "(c) Sample Size = N") +
     theme(plot.caption = element_text(size=11, hjust=0.5))
plot2 <- line_gg(summary1) + labs(caption = "(b) Sample Size = 3N/4") +
  theme(plot.caption = element_text(size=11, hjust=0.5))
plot3 <- line_gg(summary2) + labs(caption = "(a) Sample Size = N/2") +
  theme(plot.caption = element_text(size=11, hjust=0.5))

# Combine the three plots using patchwork
design <- 
  "1122
   1122
   #33#
   #33#"
combined_plot <- plot3 + plot2 + plot1 + plot_layout(guides = "collect", design = design) + 
  plot_annotation(title = sprintf("Estimated probability of each type of predictors being selected by different variable selection methods\n
      Number of predictors = %d, Percentage of each type = (%.1f, %.1f, %.1f, %.1f)", 
                                  n.para, percentage[1], percentage[2], percentage[3], percentage[4]),
                  theme = theme(
                    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))) & 
  theme(legend.position = "right") 
# Print the combined plot
print(combined_plot)

# Save the combined plot
ggsave("line_para_50.png", combined_plot, width = 14, height = 10, dpi = 600)

###################
f_para <- data.frame(
  size = rep(c("N", "3N/4", "N/2"), each = 14000),
  f = rowSums(combine1[all_predictors]),
  method = rep(c("MLE", "BE0.05", "BE0.15", "Uni0.05", "Uni0.15", "Uni0.05-BE", "Uni0.15-BE", "LASSOmin", 
                 "LASSO1se", "LASSOmin-MLE", "LASSO1se-MLE", "LASSOmin-BE", "LASSO1se-BE", "modifiedLASSO"), 3000)
)
f_para$size <- factor(f_para$size, levels = c("N/2", "3N/4", "N"))
f_para$method <- factor(f_para$method, levels = c("MLE", "BE0.05", "BE0.15", "Uni0.05", "Uni0.15", "Uni0.05-BE", "Uni0.15-BE", "LASSOmin", 
                                          "LASSO1se", "LASSOmin-MLE", "LASSO1se-MLE", "LASSOmin-BE", "LASSO1se-BE", "modifiedLASSO"))
fpara_filtered <- f_para %>%
  filter(!method %in% c("LASSOmin-MLE", "LASSO1se-MLE"))

#para_result <- f_para %>%
#  group_by(size, method) %>%
#  summarize(f_para = mean(f))

#para_result_filtered <- para_result %>%
#  filter(!method %in% c("MLE","LASSOmin", 
#                        "LASSO1se", "LASSOmin-MLE", "LASSO1se-MLE", "LASSOmin-BE", "modifiedLASSO"))

plot_fpara <- ggplot(fpara_filtered, aes(x = size, y = f, fill = method)) +
  geom_boxplot(
    width = 0.75,
    position = position_dodge(0.83),
    outlier.size = 1,
    outlier.shape = 16
  ) +
  geom_hline(yintercept = 30, linetype = "dashed", color = "black", linewidth = 0.5) +
  labs(
    x = "Sample size",
    y = "Number of selected predictors",
    title = "Number of selected predictors given by different variable selection methods\nPrevalence = 0.3, C-statistic = 0.8, N = 1505, Number of predictors = 30"
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
  scale_fill_manual(values = c("azure4", "wheat","gold", "darkorange", "darkgoldenrod", 
                               "seagreen2", "mediumturquoise", "plum1", "hotpink1", 
                               "darkorchid1", "royalblue2", "slateblue2"
                               ))
print(plot_fpara)


ggsave(
  "fpara_20.png",
  plot = plot_fpara,
  width = 7,    # Set the width to a narrow value (in inches)
  height = 6,   # Adjust the height accordingly
  dpi = 300
)

