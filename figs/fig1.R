# Load packages -----------------------------------------------------------

library(tidyverse)
library(cowplot)

# Load and prepare data ---------------------------------------------------

TH_plot_df <- read.csv("output/interaction_TH_emm.csv")
FN_plot_df <- read.csv("output/interaction_FN_emm.csv")
LS_plot_df <- read.csv("output/interaction_LS_emm.csv")
TR_plot_df <- read.csv("output/interaction_TR_emm.csv")

TH_plot_df$group <- as.factor(TH_plot_df$group)
TH_plot_df$time  <- as.factor(TH_plot_df$time)
FN_plot_df$group <- as.factor(FN_plot_df$group)
FN_plot_df$time  <- as.factor(FN_plot_df$time)
LS_plot_df$group <- as.factor(LS_plot_df$group)
LS_plot_df$time  <- as.factor(LS_plot_df$time)
TR_plot_df$group <- as.factor(TR_plot_df$group)
TR_plot_df$time  <- as.factor(TR_plot_df$time)


# Overall plots config
dodge <- position_dodge(0.2)

# TH plot -----------------------------------------------------------------

TH_plot <- ggplot(data = TH_plot_df) +
  geom_point(
    aes(x = time, y = emmean, colour = group),
    position = dodge, size = 2
  ) +
  geom_line(
    aes(x = time, y = emmean, colour = group, group = group),
    position = dodge, size = 1
  ) +
  geom_errorbar(
    aes(x = time, ymin = lower.CL, ymax = upper.CL, colour = group), 
    position = dodge, size = 1, width = 0.1
  ) +
  scale_color_manual(values = c("#0072B2", "#E69F00")) +
  scale_y_continuous(breaks = seq(0, 2, 0.01)) +
  labs(
    x = "Time",
    y = "Femoral neck BMD",
    colour = "Group" 
  ) +
  theme_classic()

# FN plot -----------------------------------------------------------------

FN_plot <- ggplot(data = FN_plot_df) +
  geom_point(
    aes(x = time, y = emmean, colour = group),
    position = dodge, size = 2
  ) +
  geom_line(
    aes(x = time, y = emmean, colour = group, group = group),
    position = dodge, size = 1
  ) +
  geom_errorbar(
    aes(x = time, ymin = lower.CL, ymax = upper.CL, colour = group), 
    position = dodge, size = 1, width = 0.1
  ) +
  scale_color_manual(values = c("#0072B2", "#E69F00")) +
  scale_y_continuous(breaks = seq(0, 2, 0.01)) +
  labs(
    x = "Time",
    y = "Femoral neck BMD",
    colour = "Group" 
  ) +
  theme_classic()

# LS plot -----------------------------------------------------------------

LS_plot <- ggplot(data = LS_plot_df) +
  geom_point(
    aes(x = time, y = emmean, colour = group),
    position = dodge, size = 2
  ) +
  geom_line(
    aes(x = time, y = emmean, colour = group, group = group),
    position = dodge, size = 1
  ) +
  geom_errorbar(
    aes(x = time, ymin = lower.CL, ymax = upper.CL, colour = group), 
    position = dodge, size = 1, width = 0.1
  ) +
  scale_color_manual(values = c("#0072B2", "#E69F00")) +
  scale_y_continuous(breaks = seq(0, 2, 0.01)) +
  labs(
    x = "Time",
    y = "Lumbar spine BMD",
    colour = "Group" 
  ) +
  theme_classic()