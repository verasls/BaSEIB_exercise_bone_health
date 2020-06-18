# Load packages -----------------------------------------------------------

library(tidyverse)
library(cowplot)

# Load and prepare data ---------------------------------------------------

BM_plot_df <- read_csv(here("output", "interaction_body_mass_emm.csv")) %>%
  mutate(
    time = as.factor(time),
    group = recode(
      as.factor(group),
      "Control" = "Control group",
      "Exercise" = "Exercise group"
    )
  )

BMI_plot_df <- read_csv(here("output", "interaction_BMI_emm.csv")) %>%
  mutate(
    time = as.factor(time),
    group = recode(
      as.factor(group),
      "Control" = "Control group",
      "Exercise" = "Exercise group"
    )
  )

FM_plot_df <- read_csv(here("output", "interaction_fat_mass_emm.csv")) %>%
  mutate(
    time = as.factor(time),
    group = recode(
      as.factor(group),
      "Control" = "Control group",
      "Exercise" = "Exercise group"
    )
  )

LM_plot_df <- read_csv(here("output", "interaction_lean_mass_emm.csv")) %>%
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

# Body mass plot ----------------------------------------------------------

BM_plot <- ggplot(data = BM_plot_df) +
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
    breaks = seq(70, 120, 10),
    limits = c(65, 120)
  ) +
  scale_x_discrete(
    labels = c(
      "1" = "Pre-BS\n\nCG: n = 20\nEG: n = 41",
      "2" = "1-month post-BS\n\nCG: n = 20\nEG: n = 41",
      "3" = "6-months post-BS\n\nCG: n = 20\nEG: n = 41",
      "4" = "12-months post-BS\n\nCG: n = 20\nEG: n = 41"
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
    y = quote("Body mass"~(kg))
  )

# BMI plot ----------------------------------------------------------------

BMI_plot <- ggplot(data = BMI_plot_df) +
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
    breaks = seq(25, 45, 5),
    limits = c(25, 47)
  ) +
  scale_x_discrete(
    labels = c(
      "1" = "Pre-BS\n\nCG: n = 20\nEG: n = 41",
      "2" = "1-month post-BS\n\nCG: n = 20\nEG: n = 41",
      "3" = "6-months post-BS\n\nCG: n = 20\nEG: n = 40",
      "4" = "12-months post-BS\n\nCG: n = 16\nEG: n = 34"
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
    y = quote("Body mass index"~(kg%.%m^-2))
  )

# Fat mass plot -----------------------------------------------------------

FM_plot <- ggplot(data = FM_plot_df) +
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
    breaks = seq(20, 60, 10),
    limits = c(20, 60)
  ) +
  scale_x_discrete(
    labels = c(
      "1" = "Pre-BS\n\nCG: n = 20\nEG: n = 41",
      "2" = "1-month post-BS\n\nCG: n = 20\nEG: n = 41",
      "3" = "6-months post-BS\n\nCG: n = 20\nEG: n = 40",
      "4" = "12-months post-BS\n\nCG: n = 16\nEG: n = 34"
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
    y = quote("Whole body fat mass"~(kg))
  )

# Lean mass plot ----------------------------------------------------------

LM_plot <- ggplot(data = LM_plot_df) +
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
    breaks = seq(40, 55, 5),
    limits = c(40, 56)
  ) +
  scale_x_discrete(
    labels = c(
      "1" = "Pre-BS\n\nCG: n = 20\nEG: n = 41",
      "2" = "1-month post-BS\n\nCG: n = 20\nEG: n = 41",
      "3" = "6-months post-BS\n\nCG: n = 20\nEG: n = 40",
      "4" = "12-months post-BS\n\nCG: n = 16\nEG: n = 34"
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
    y = quote("Whole body lean mass"~(kg))
  )

# Plot grid ---------------------------------------------------------------

legend <- get_legend(BM_plot)

grid_1 <- plot_grid(
  BM_plot + theme(legend.position = "none"),
  BMI_plot + theme(legend.position = "none"),
  FM_plot + theme(legend.position = "none"),
  LM_plot + theme(legend.position = "none"),
  ncol = 2, nrow = 2
)

grid <- plot_grid(legend, grid_1, ncol = 1, rel_heights = c(0.1, 1))

ggsave(
  filename = "figs/body_composition.pdf",
  plot = grid, width = 40, height = 40, dpi = 200, units = "cm"
)
