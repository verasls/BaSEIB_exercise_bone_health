# Load packages -----------------------------------------------------------

library(tidyverse)
library(cowplot)

# Load and prepare data ---------------------------------------------------

LS_plot_df <- read_csv("output/interaction_LS_PP_emm_df.csv") %>%
  mutate(
    time = as.factor(time),
    attend_cat = recode(
      as.factor(attend_cat),
      "Control" = "Control group",
      "Under 50% training attendance" = "Exercise group (under 50% training attendance)",
      "Over 50% training attendance" = "Exercise group (over 50% training attendance)"
    )
    # attend_cat = as.factor(attend_cat)
  )

TR_plot_df <- read_csv("output/interaction_TR_PP_emm_df.csv") %>%
  mutate(
    time = as.factor(time),
    attend_cat = as.factor(attend_cat)
  )

TH_plot_df <- read_csv("output/interaction_TH_PP_emm_df.csv") %>%
  mutate(
    time = as.factor(time),
    attend_cat = as.factor(attend_cat)
  )

FN_plot_df <- read_csv("output/interaction_FN_PP_emm_df.csv") %>%
  mutate(
    time = as.factor(time),
    attend_cat = as.factor(attend_cat)
  )

# Overall plots config
dodge <- position_dodge(0.2)

# LS plot -----------------------------------------------------------------

LS_plot <- ggplot(data = LS_plot_df) +
  geom_point(
    aes(x = time, y = emmean, shape = attend_cat, colour = attend_cat),
    position = dodge, size = 4
  ) +
  geom_line(
    aes(x = time, y = emmean, linetype = attend_cat, group = attend_cat, colour = attend_cat),
    position = dodge, size = 1
  ) +
  geom_errorbar(
    aes(x = time, ymin = lower.CL, ymax = upper.CL, group = attend_cat, colour = attend_cat),
    position = dodge, size = 1, width = 0.1
  ) +
  scale_y_continuous(
    breaks = seq(0, 2, 0.01),
    labels = scales::number_format(accuracy = 0.001)
  ) +
  scale_x_discrete(
    labels = c(
      "1" = "Pre-BS",
      "2" = "1-month post-BS",
      "3" = "6-months post-BS",
      "4" = "12-months post-BS"
    )
  ) +
  scale_color_manual(values = c("#0072B2", "#D55E00", "#009E73")) +
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
  )

# TR plot -----------------------------------------------------------------

TR_plot <- ggplot(data = TR_plot_df) +
  geom_point(
    aes(x = time, y = emmean, shape = attend_cat, colour = attend_cat),
    position = dodge, size = 4
  ) +
  geom_line(
    aes(x = time, y = emmean, linetype = attend_cat, group = attend_cat, colour = attend_cat),
    position = dodge, size = 1
  ) +
  geom_errorbar(
    aes(x = time, ymin = lower.CL, ymax = upper.CL, group = attend_cat, colour = attend_cat),
    position = dodge, size = 1, width = 0.1
  ) +
  scale_y_continuous(
    breaks = seq(0, 2, 0.01),
    labels = scales::number_format(accuracy = 0.001)
  ) +
  scale_x_discrete(
    labels = c(
      "1" = "Pre-BS",
      "2" = "1-month post-BS",
      "3" = "6-months post-BS",
      "4" = "12-months post-BS"
    )
  ) +
  scale_color_manual(values = c("#0072B2", "#D55E00", "#009E73")) +
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
  )

# TH plot -----------------------------------------------------------------

TH_plot <- ggplot(data = TH_plot_df) +
  geom_point(
    aes(x = time, y = emmean, shape = attend_cat, colour = attend_cat),
    position = dodge, size = 4
  ) +
  geom_line(
    aes(x = time, y = emmean, linetype = attend_cat, group = attend_cat, colour = attend_cat),
    position = dodge, size = 1
  ) +
  geom_errorbar(
    aes(x = time, ymin = lower.CL, ymax = upper.CL, group = attend_cat, colour = attend_cat),
    position = dodge, size = 1, width = 0.1
  ) +
  scale_y_continuous(
    breaks = seq(0, 2, 0.01),
    labels = scales::number_format(accuracy = 0.001)
  ) +
  scale_x_discrete(
    labels = c(
      "1" = "Pre-BS",
      "2" = "1-month post-BS",
      "3" = "6-months post-BS",
      "4" = "12-months post-BS"
    )
  ) +
  scale_color_manual(values = c("#0072B2", "#D55E00", "#009E73")) +
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
    aes(x = time, y = emmean, shape = attend_cat, colour = attend_cat),
    position = dodge, size = 4
  ) +
  geom_line(
    aes(x = time, y = emmean, linetype = attend_cat, group = attend_cat, colour = attend_cat),
    position = dodge, size = 1
  ) +
  geom_errorbar(
    aes(x = time, ymin = lower.CL, ymax = upper.CL, group = attend_cat, colour = attend_cat),
    position = dodge, size = 1, width = 0.1
  ) +
  scale_y_continuous(
    breaks = seq(0, 2, 0.01),
    labels = scales::number_format(accuracy = 0.001)
  ) +
  scale_x_discrete(
    labels = c(
      "1" = "Pre-BS",
      "2" = "1-month post-BS",
      "3" = "6-months post-BS",
      "4" = "12-months post-BS"
    )
  ) +
  scale_color_manual(values = c("#0072B2", "#D55E00", "#009E73")) +
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
  filename = "figs/fig_PP.pdf",
  plot = grid, width = 40, height = 40, dpi = 200, units = "cm"
)
