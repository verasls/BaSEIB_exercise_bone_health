# Load packages -----------------------------------------------------------

library(tidyverse)

# Load and prepare data ---------------------------------------------------

# Steps
steps_plot_df <- read.csv("output/interaction_steps_emm.csv")

steps_plot_df$time <- as.factor(steps_plot_df$time)
steps_plot_df$time <- recode(
  steps_plot_df$time,
  "1" = "Pre-BS",
  "2" = "1-month post-BS",
  "3" = "6-months post-BS",
  "4" = "12-months post-BS"
)

steps_plot_df$attend_cat <- factor(
  steps_plot_df$attend_cat,
  levels = c(
    "Control", 
    "Under 50% training attendance", 
    "Over 50% training attendance"
  )
)
steps_plot_df$attend_cat <- recode(
  steps_plot_df$attend_cat,
  "Control" = "Control group",
  "Under 50% training attendance" = "Exercise group (under 50% training attendance)",
  "Over 50% training attendance" = "Exercise group (over 50% training attendance)"
)

# Sedentary behaviour
SB_plot_df <- read.csv("output/interaction_SB_emm.csv")

SB_plot_df$time <- as.factor(SB_plot_df$time)
SB_plot_df$time <- recode(
  SB_plot_df$time,
  "1" = "Pre-BS",
  "2" = "1-month post-BS",
  "3" = "6-months post-BS",
  "4" = "12-months post-BS"
)

SB_plot_df$attend_cat <- factor(
  SB_plot_df$attend_cat,
  levels = c(
    "Control", 
    "Under 50% training attendance", 
    "Over 50% training attendance"
  )
)
SB_plot_df$attend_cat <- recode(
  SB_plot_df$attend_cat,
  "Control" = "Control group",
  "Under 50% training attendance" = "Exercise group (under 50% training attendance)",
  "Over 50% training attendance" = "Exercise group (over 50% training attendance)"
)

# Light physical activity
LPA_plot_df <- read.csv("output/interaction_LPA_emm.csv")

LPA_plot_df$time <- as.factor(LPA_plot_df$time)
LPA_plot_df$time <- recode(
  LPA_plot_df$time,
  "1" = "Pre-BS",
  "2" = "1-month post-BS",
  "3" = "6-months post-BS",
  "4" = "12-months post-BS"
)

LPA_plot_df$attend_cat <- factor(
  LPA_plot_df$attend_cat,
  levels = c(
    "Control", 
    "Under 50% training attendance", 
    "Over 50% training attendance"
  )
)
LPA_plot_df$attend_cat <- recode(
  LPA_plot_df$attend_cat,
  "Control" = "Control group",
  "Under 50% training attendance" = "Exercise group (under 50% training attendance)",
  "Over 50% training attendance" = "Exercise group (over 50% training attendance)"
)

# Moderate-to-vigorous physical activity
MVPA_plot_df <- read.csv("output/interaction_MVPA_emm.csv")

MVPA_plot_df$time <- as.factor(MVPA_plot_df$time)
MVPA_plot_df$time <- recode(
  MVPA_plot_df$time,
  "1" = "Pre-BS",
  "2" = "1-month post-BS",
  "3" = "6-months post-BS",
  "4" = "12-months post-BS"
)

MVPA_plot_df$attend_cat <- factor(
  MVPA_plot_df$attend_cat,
  levels = c(
    "Control", 
    "Under 50% training attendance", 
    "Over 50% training attendance"
  )
)
MVPA_plot_df$attend_cat <- recode(
  MVPA_plot_df$attend_cat,
  "Control" = "Control group",
  "Under 50% training attendance" = "Exercise group (under 50% training attendance)",
  "Over 50% training attendance" = "Exercise group (over 50% training attendance)"
)

# Overall plots config
dodge <- position_dodge(0.2)

# Steps plot --------------------------------------------------------------

steps_plot <- ggplot(data = steps_plot_df) +
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
    y = "% number of steps change"
  )

# SB plot -----------------------------------------------------------------

SB_plot <- ggplot(data = SB_plot_df) +
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
    y = "% time in sedentary behaviour change"
  )

# LPA plot ----------------------------------------------------------------

LPA_plot <- ggplot(data = LPA_plot_df) +
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
    y = "% time in light physical activity change"
  )

# MVPA plot ---------------------------------------------------------------

MVPA_plot <- ggplot(data = MVPA_plot_df) +
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
    y = "% time in moderate-to-vigorous physical activity change"
  )