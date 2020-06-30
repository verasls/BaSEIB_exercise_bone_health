# Load packages -----------------------------------------------------------

library(tidyverse)
library(cowplot)

# Load and prepare data ---------------------------------------------------

LS_plot_df <- read_csv("output/interaction_LS_emm.csv") %>% 
  mutate(
    time = as.factor(time),
    group = recode(
      as.factor(group),
      "Control" = "Control group",
      "Exercise" = "Exercise group"
    )
  )

TH_plot_df <- read_csv("output/interaction_TH_emm.csv") %>% 
  mutate(
    time = as.factor(time),
    group = recode(
      as.factor(group),
      "Control" = "Control group",
      "Exercise" = "Exercise group"
    )
  )

FN_plot_df <- read_csv("output/interaction_FN_emm.csv") %>% 
  mutate(
    time = as.factor(time),
    group = recode(
      as.factor(group),
      "Control" = "Control group",
      "Exercise" = "Exercise group"
    )
  )

TR_plot_df <- read_csv("output/interaction_TR_emm.csv") %>% 
  mutate(
    time = as.factor(time),
    group = recode(
      as.factor(group),
      "Control" = "Control group",
      "Exercise" = "Exercise group"
    )
  )

# Overall plots config
dodge <- position_dodge(0.2)

# LS plot -----------------------------------------------------------------

LS_plot <- ggplot(data = LS_plot_df) +
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
    breaks = seq(0, 2, 0.01), 
    labels = scales::number_format(accuracy = 0.001)
  ) +
  scale_x_discrete(
    labels = c(
      "1" = "Pre-BS\n\nCG: n = 19\nEG: n = 39",
      "2" = "1-month post-BS\n\nCG: n = 19\nEG: n = 39",
      "3" = "6-months post-BS\n\nCG: n = 19\nEG: n = 38",
      "4" = "12-months post-BS\n\nCG: n = 16\nEG: n = 31"
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
    y = quote("Lumbar spine bone mineral density"~(g%.%cm^-2))
  ) +
  annotate("segment", x = 4.20, xend = 4.20, y = 0.998, yend = 1.026, size = 0.5) +
  annotate("segment", x = 4.15, xend = 4.20, y = 0.998, yend = 0.998, size = 0.5) +
  annotate("segment", x = 4.15, xend = 4.20, y = 1.026, yend = 1.026, size = 0.5) +
  annotate("text", x = 4.3, y = 1.012, label = "italic(p) == 0.005", angle = 90, parse = TRUE) +
  annotate("text", x = 4.4, y = 1.012, label = "paste(\"+ 0.028\", g%.%cm^-2)", angle = 90, parse = TRUE)

# TR plot -----------------------------------------------------------------

TR_plot <- ggplot(data = TR_plot_df) +
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
  scale_y_continuous(breaks = seq(0, 2, 0.005)) +
  scale_x_discrete(
    labels = c(
      "1" = "Pre-BS\n\nCG: n = 20\nEG: n = 40",
      "2" = "1-month post-BS\n\nCG: n = 18\nEG: n = 38",
      "3" = "6-months post-BS\n\nCG: n = 19\nEG: n = 39",
      "4" = "12-months post-BS\n\nCG: n = 16\nEG: n = 33"
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
    y = quote("One-third radius bone mineral density"~(g%.%cm^-2))
  ) +
  annotate("segment", x = 4.20, xend = 4.20, y = 0.679, yend = 0.692, size = 0.5) +
  annotate("segment", x = 4.15, xend = 4.20, y = 0.679, yend = 0.679, size = 0.5) +
  annotate("segment", x = 4.15, xend = 4.20, y = 0.692, yend = 0.692, size = 0.5) +
  annotate("text", x = 4.3, y = 0.686, label = "italic(p) == 0.014", angle = 90, parse = TRUE) +
  annotate("text", x = 4.4, y = 0.686, label = "paste(\"+ 0.013\", g%.%cm^-2)", angle = 90, parse = TRUE)

# TH plot -----------------------------------------------------------------

TH_plot <- ggplot(data = TH_plot_df) +
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
    breaks = seq(0, 2, 0.01), 
    labels = scales::number_format(accuracy = 0.001)
  ) +
  scale_x_discrete(
    labels = c(
      "1" = "Pre-BS\n\nCG: n = 20\nEG: n = 40",
      "2" = "1-month post-BS\n\nCG: n = 18\nEG: n = 39",
      "3" = "6-months post-BS\n\nCG: n = 19\nEG: n = 39",
      "4" = "12-months post-BS\n\nCG: n = 16\nEG: n = 33"
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
    y = quote("Total hip bone mineral density"~(g%.%cm^-2))
  )

# FN plot -----------------------------------------------------------------

FN_plot <- ggplot(data = FN_plot_df) +
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
    breaks = seq(0, 2, 0.01), 
    labels = scales::number_format(accuracy = 0.001)
  ) +
  scale_x_discrete(
    labels = c(
      "1" = "Pre-BS\n\nCG: n = 20\nEG: n = 40",
      "2" = "1-month post-BS\n\nCG: n = 19\nEG: n = 40",
      "3" = "6-months post-BS\n\nCG: n = 19\nEG: n = 39",
      "4" = "12-months post-BS\n\nCG: n = 16\nEG: n = 33"
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
    y = quote("Femoral neck bone mineral density"~(g%.%cm^-2))
  )

# Plot grid ---------------------------------------------------------------

legend <- get_legend(TH_plot)

grid_1 <- plot_grid(
  LS_plot + theme(legend.position = "none"),
  TR_plot + theme(legend.position = "none"),
  TH_plot + theme(legend.position = "none"),
  FN_plot + theme(legend.position = "none"),
  ncol = 2, nrow = 2
)

grid <- plot_grid(legend, grid_1, ncol = 1, rel_heights = c(0.1, 1))

ggsave(
  filename = "figs/fig2.pdf",
  plot = grid, width = 40, height = 40, dpi = 200, units = "cm"
)
