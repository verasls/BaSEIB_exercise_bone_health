# Load packages -----------------------------------------------------------

library(tidyverse)
library(cowplot)

# Load and prepare data ---------------------------------------------------

steps_plot_df <- read_csv(here("output", "interaction_steps_emm.csv")) %>%
  mutate(
    time = as.factor(time),
    group = recode(
      as.factor(group),
      "Control" = "Control group",
      "Exercise" = "Exercise group"
    )
  )

SB_plot_df <- read_csv(here("output", "interaction_SB_emm.csv")) %>%
  mutate(
    time = as.factor(time),
    group = recode(
      as.factor(group),
      "Control" = "Control group",
      "Exercise" = "Exercise group"
    )
  )

LPA_plot_df <- read_csv(here("output", "interaction_LPA_emm.csv")) %>%
  mutate(
    time = as.factor(time),
    group = recode(
      as.factor(group),
      "Control" = "Control group",
      "Exercise" = "Exercise group"
    )
  )

MVPA_plot_df <- read_csv(here("output", "interaction_MVPA_emm.csv")) %>%
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

# Steps plot --------------------------------------------------------------

steps_plot <- ggplot(data = steps_plot_df) +
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
    breaks = seq(4000, 9000, 1000),
    limits = c(4000, 9000)
  ) +
  scale_x_discrete(
    labels = c(
      "1" = "Pre-BS\n\nCG: n = 19\nEG: n = 37",
      "2" = "1-month post-BS\n\nCG: n = 16\nEG: n = 33",
      "3" = "6-months post-BS\n\nCG: n = 16\nEG: n = 33",
      "4" = "12-months post-BS\n\nCG: n = 14\nEG: n = 24"
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
    y = "Number of steps"
  )

# Sedentary behavior plot -------------------------------------------------

SB_plot <- ggplot(data = SB_plot_df) +
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
    breaks = seq(6, 9, 1),
    limits = c(6, 9)
  ) +
  scale_x_discrete(
    labels = c(
      "1" = "Pre-BS\n\nCG: n = 19\nEG: n = 37",
      "2" = "1-month post-BS\n\nCG: n = 16\nEG: n = 33",
      "3" = "6-months post-BS\n\nCG: n = 16\nEG: n = 33",
      "4" = "12-months post-BS\n\nCG: n = 14\nEG: n = 24"
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
    y = "Time in sedentary behavior (hours)"
  )

# Light physical activity plot --------------------------------------------

LPA_plot <- ggplot(data = LPA_plot_df) +
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
    breaks = seq(4, 8, 1),
    limits = c(4, 8)
  ) +
  scale_x_discrete(
    labels = c(
      "1" = "Pre-BS\n\nCG: n = 19\nEG: n = 37",
      "2" = "1-month post-BS\n\nCG: n = 16\nEG: n = 33",
      "3" = "6-months post-BS\n\nCG: n = 16\nEG: n = 33",
      "4" = "12-months post-BS\n\nCG: n = 14\nEG: n = 24"
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
    y = "Time in light physical activity (hours)"
  )

# Moderate-to-vigorous physical activity plot -----------------------------

MVPA_plot <- ggplot(data = MVPA_plot_df) +
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
    breaks = seq(15, 50, 5),
    limits = c(15, 50)
  ) +
  scale_x_discrete(
    labels = c(
      "1" = "Pre-BS\n\nCG: n = 19\nEG: n = 37",
      "2" = "1-month post-BS\n\nCG: n = 16\nEG: n = 33",
      "3" = "6-months post-BS\n\nCG: n = 16\nEG: n = 33",
      "4" = "12-months post-BS\n\nCG: n = 14\nEG: n = 24"
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
    y = "Time in moderate-to-vigorous physical activity (minutes)"
  )

# Plot grid ---------------------------------------------------------------

legend <- get_legend(steps_plot)

grid_1 <- plot_grid(
  steps_plot + theme(legend.position = "none"),
  SB_plot + theme(legend.position = "none"),
  LPA_plot + theme(legend.position = "none"),
  MVPA_plot + theme(legend.position = "none"),
  ncol = 2, nrow = 2
)

grid <- plot_grid(legend, grid_1, ncol = 1, rel_heights = c(0.1, 1))

ggsave(
  filename = "figs/physical_activity.pdf",
  plot = grid, width = 40, height = 40, dpi = 200, units = "cm"
)
