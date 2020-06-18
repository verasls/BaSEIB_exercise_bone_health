# Load packages -----------------------------------------------------------

library(tidyverse)
library(cowplot)

# Load and prepare data ---------------------------------------------------

ext_plot_df <- read_csv(here("output", "interaction_pt_ext_emm_df.csv")) %>%
  mutate(
    time = as.factor(time),
    group = recode(
      as.factor(group),
      "Control" = "Control group",
      "Exercise" = "Exercise group"
    )
  )

fle_plot_df <- read_csv(here("output", "interaction_pt_fle_emm_df.csv")) %>%
  mutate(
    time = as.factor(time),
    group = recode(
      as.factor(group),
      "Control" = "Control group",
      "Exercise" = "Exercise group"
    )
  )

rel_ext_plot_df <- read_csv(here("output", "interaction_pt_ext_rel_emm_df.csv")) %>%
  mutate(
    time = as.factor(time),
    group = recode(
      as.factor(group),
      "Control" = "Control group",
      "Exercise" = "Exercise group"
    )
  )

rel_fle_plot_df <- read_csv(here("output", "interaction_pt_fle_rel_emm_df.csv")) %>%
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

# Peak torque extension plot ----------------------------------------------

ext_plot <- ggplot(data = ext_plot_df) +
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
    breaks = seq(110, 160, 10),
    limits = c(105, 160)
  ) +
  scale_x_discrete(
    labels = c(
      "1" = "Pre-BS\n\nCG: n = 20\nEG: n = 41",
      "2" = "1-month post-BS\n\nCG: n = 20\nEG: n = 40",
      "3" = "6-months post-BS\n\nCG: n = 20\nEG: n = 40",
      "4" = "12-months post-BS\n\nCG: n = 15\nEG: n = 31"
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
    y = quote("Knee extension absolute strength"~(Nm))
  )

# Peak torque flexion plot ------------------------------------------------

fle_plot <- ggplot(data = fle_plot_df) +
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
    breaks = seq(55, 85, 5),
    limits = c(55, 85)
  ) +
  scale_x_discrete(
    labels = c(
      "1" = "Pre-BS\n\nCG: n = 20\nEG: n = 41",
      "2" = "1-month post-BS\n\nCG: n = 20\nEG: n = 40",
      "3" = "6-months post-BS\n\nCG: n = 20\nEG: n = 40",
      "4" = "12-months post-BS\n\nCG: n = 15\nEG: n = 31"
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
    y = quote("Knee flexion absolute strength"~(Nm))
  )

# Relative peak torque extension plot -------------------------------------

rel_ext_plot <- ggplot(data = rel_ext_plot_df) +
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
    breaks = seq(1.1, 2, 0.1),
    limits = c(1.1, 2)
  ) +
  scale_x_discrete(
    labels = c(
      "1" = "Pre-BS\n\nCG: n = 20\nEG: n = 41",
      "2" = "1-month post-BS\n\nCG: n = 20\nEG: n = 40",
      "3" = "6-months post-BS\n\nCG: n = 20\nEG: n = 40",
      "4" = "12-months post-BS\n\nCG: n = 15\nEG: n = 31"
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
    y = quote("Knee extension relative strength"~(Nm%.%kg^-1))
  )

# Relative peak torque flexion plot ---------------------------------------

rel_fle_plot <- ggplot(data = rel_fle_plot_df) +
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
    breaks = seq(0.6, 1.1, 0.1),
    limits = c(0.55, 1.1)
  ) +
  scale_x_discrete(
    labels = c(
      "1" = "Pre-BS\n\nCG: n = 20\nEG: n = 41",
      "2" = "1-month post-BS\n\nCG: n = 20\nEG: n = 40",
      "3" = "6-months post-BS\n\nCG: n = 20\nEG: n = 40",
      "4" = "12-months post-BS\n\nCG: n = 15\nEG: n = 31"
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
    y = quote("Knee flexion relative strength"~(Nm%.%kg^-1))
  )

# Plot grid ---------------------------------------------------------------

legend <- get_legend(ext_plot)

grid_1 <- plot_grid(
  ext_plot + theme(legend.position = "none"),
  fle_plot + theme(legend.position = "none"),
  rel_ext_plot + theme(legend.position = "none"),
  rel_fle_plot + theme(legend.position = "none"),
  ncol = 2, nrow = 2
)

grid <- plot_grid(legend, grid_1, ncol = 1, rel_heights = c(0.1, 1))

ggsave(
  filename = "figs/strength.pdf",
  plot = grid, width = 40, height = 40, dpi = 200, units = "cm"
)