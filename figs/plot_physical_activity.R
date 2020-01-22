# Load packages -----------------------------------------------------------

library(tidyverse)

# Load and prepare data ---------------------------------------------------

steps_plot_df <- read.csv("output/interaction_steps_emm.csv") %>% 
  mutate(variable = "% number of steps change")
SB_plot_df <- read.csv("output/interaction_SB_emm.csv") %>% 
  mutate(variable = "% time in sedentary behavior change")
LPA_plot_df <- read.csv("output/interaction_LPA_emm.csv") %>% 
  mutate(variable = "% time in light physical activity change")
MVPA_plot_df <- read.csv("output/interaction_MVPA_emm.csv") %>% 
  mutate(variable = "% time in moderate-to-vigorous physical activity change")

# Combine into a single data frame
delta_PA_plot_df <- steps_plot_df %>% 
  rbind(
    SB_plot_df,
    LPA_plot_df,
    MVPA_plot_df
  ) %>% 
  filter(time == 4)

# Refactor variables
delta_PA_plot_df$variable <- factor(
  delta_PA_plot_df$variable,
  levels = c(
    "% number of steps change", 
    "% time in sedentary behavior change", 
    "% time in light physical activity change", 
    "% time in moderate-to-vigorous physical activity change"
  )
)
delta_PA_plot_df$attend_cat <- factor(
  delta_PA_plot_df$attend_cat,
  levels = c(
    "Control", 
    "Under 50% training attendance", 
    "Over 50% training attendance"
  )
)
delta_PA_plot_df$attend_cat <- recode(
  delta_PA_plot_df$attend_cat,
  "Control" = "CG: n  = 12",
  "Under 50% training attendance" = "EG<50%: n = 12",
  "Over 50% training attendance" = "EG≥50%: n = 14"
)

# Overall plots config
dodge <- position_dodge(0.2)

# Steps plot --------------------------------------------------------------

steps_plot <- ggplot(data = filter(delta_PA_plot_df, variable == "% number of steps change")) +
  geom_point(
    aes(x = attend_cat, y = emmean, shape = attend_cat, colour = attend_cat),
    position = dodge, size = 4
  ) +
  geom_line(
    aes(x = attend_cat, y = emmean, linetype = attend_cat, group = attend_cat, colour = attend_cat),
    position = dodge, size = 1
  ) +
  geom_errorbar(
    aes(x = attend_cat, ymin = lower.CL, ymax = upper.CL, group = attend_cat, colour = attend_cat), 
    position = dodge, size = 1, width = 0.1
  ) +
  scale_color_manual(values = c("#0072B2", "#D55E00", "#009E73")) +
  scale_y_continuous(limits = c(5, 85), breaks = seq(5, 85, 5)) +
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

SB_plot <- ggplot(data = filter(delta_PA_plot_df, variable == "% time in sedentary behavior change")) +
  geom_point(
    aes(x = attend_cat, y = emmean, shape = attend_cat, colour = attend_cat),
    position = dodge, size = 4
  ) +
  geom_line(
    aes(x = attend_cat, y = emmean, linetype = attend_cat, group = attend_cat, colour = attend_cat),
    position = dodge, size = 1
  ) +
  geom_errorbar(
    aes(x = attend_cat, ymin = lower.CL, ymax = upper.CL, group = attend_cat, colour = attend_cat), 
    position = dodge, size = 1, width = 0.1
  ) +
  scale_color_manual(values = c("#0072B2", "#D55E00", "#009E73")) +
  scale_y_continuous(limits = c(-20, 20), breaks = seq(-20, 20, 5)) +
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
    y = "% time in sedentary behavior change"
  )

# LPA plot ----------------------------------------------------------------

LPA_plot <- ggplot(data = filter(delta_PA_plot_df, variable == "% time in light physical activity change")) +
  geom_point(
    aes(x = attend_cat, y = emmean, shape = attend_cat, colour = attend_cat),
    position = dodge, size = 4
  ) +
  geom_line(
    aes(x = attend_cat, y = emmean, linetype = attend_cat, group = attend_cat, colour = attend_cat),
    position = dodge, size = 1
  ) +
  geom_errorbar(
    aes(x = attend_cat, ymin = lower.CL, ymax = upper.CL, group = attend_cat, colour = attend_cat), 
    position = dodge, size = 1, width = 0.1
  ) +
  scale_color_manual(values = c("#0072B2", "#D55E00", "#009E73")) +
  scale_y_continuous(limits = c(-10, 40), breaks = seq(-10, 40, 5)) +
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

MVPA_plot <- ggplot(data = filter(delta_PA_plot_df, variable == "% time in moderate-to-vigorous physical activity change")) +
  geom_point(
    aes(x = attend_cat, y = emmean, shape = attend_cat, colour = attend_cat),
    position = dodge, size = 4
  ) +
  geom_line(
    aes(x = attend_cat, y = emmean, linetype = attend_cat, group = attend_cat, colour = attend_cat),
    position = dodge, size = 1
  ) +
  geom_errorbar(
    aes(x = attend_cat, ymin = lower.CL, ymax = upper.CL, group = attend_cat, colour = attend_cat), 
    position = dodge, size = 1, width = 0.1
  ) +
  scale_color_manual(values = c("#0072B2", "#D55E00", "#009E73")) +
  scale_y_continuous(limits = c(-20, 120), breaks = seq(-20, 120, 10)) +
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