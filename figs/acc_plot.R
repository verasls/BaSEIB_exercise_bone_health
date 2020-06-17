# Load packages -----------------------------------------------------------

library(tidyverse)

# Load and prepare data ---------------------------------------------------

acc_plot_df <- read_csv("output/interaction_peaks_emm.csv") %>% 
  mutate(
    group = recode(
      as.factor(group),
      "Control" = "Control group",
      "Exercise" = "Exercise group"
    ), 
    time = as.factor(time)
  )

# Overall plots config
dodge <- position_dodge(0.2)

# Plot --------------------------------------------------------------------

acc_plot <- ggplot(data = acc_plot_df) +
  geom_point(
    aes(x = time, y = emmean, shape = group, colour = group),
    position = dodge, size = 4
  ) +
  geom_line(
    aes(x = time, y = emmean, linetype = group, group = group, colour = group),
    position = dodge, size = 1
  ) +
  geom_errorbar(
    aes(x = time, ymin = lower.CL, ymax = upper.CL, group = group, colour = group),
    position = dodge, size = 1, width = 0.1
  ) +
  scale_y_continuous(
    breaks = seq(- 40, 100, 20),
    limits = c(-40, 105)
  ) +
  scale_x_discrete(
    labels = c(
      "1" = "Pre-BS\n\nCG: n = 18\nEG: n = 36",
      "2" = "1-month post-BS\n\nCG: n = 16\nEG: n = 35",
      "3" = "6-months post-BS\n\nCG: n = 17\nEG: n = 33",
      "4" = "12-months post-BS\n\nCG: n = 14\nEG: n = 20"
    )
  ) +
  scale_color_manual(values = c("#0072B2", "#D55E00")) +
  theme_classic() +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    legend.position = "top",
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 12, face = "bold")
  ) +
  labs(
    x = "",
    y = expression(paste("N° of acceleration peaks above 4.9", italic(g), " per day"))
  ) +
  annotate("segment", x = 4.2, xend = 4.2, y = 9.77, yend = 73.8, size = 0.5) +
  annotate("segment", x = 4.15, xend = 4.2, y = 9.77, yend = 9.77, size = 0.5) +
  annotate("segment", x = 4.15, xend = 4.2, y = 73.8, yend = 73.8, size = 0.5) +
  annotate("text", x = 4.3, y = 41.785, label = "italic(p) == 0.005", angle = 90, parse = TRUE)

ggsave(
  filename = "figs/acc_fig.pdf",
  plot = acc_plot, width = 20, height = 20, dpi = 200, units = "cm"
)