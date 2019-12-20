# Load packages -----------------------------------------------------------

library(tidyverse)

# Load and prepare data ---------------------------------------------------

# Peak torque knee extension
PT_ext_plot_df <- read.csv("output/interaction_PT_ext_emm.csv")
PT_ext_plot_df$lower.CL[1:3] <- 0
PT_ext_plot_df$upper.CL[1:3] <- 0

PT_ext_plot_df$time <- as.factor(PT_ext_plot_df$time)
PT_ext_plot_df$time <- recode(
  PT_ext_plot_df$time,
  "1" = "Pre-BS",
  "2" = "1-month post-BS",
  "3" = "6-months post-BS",
  "4" = "12-months post-BS"
)

PT_ext_plot_df$attend_cat <- factor(
  PT_ext_plot_df$attend_cat,
  levels = c(
    "Control", 
    "Under 50% training attendance", 
    "Over 50% training attendance"
  )
)
PT_ext_plot_df$attend_cat <- recode(
  PT_ext_plot_df$attend_cat,
  "Control" = "Control group",
  "Under 50% training attendance" = "Exercise group (under 50% training attendance)",
  "Over 50% training attendance" = "Exercise group (over 50% training attendance)"
)

# Peak torque knee flexion
PT_fle_plot_df <- read.csv("output/interaction_PT_fle_emm.csv")
PT_fle_plot_df$lower.CL[1:3] <- 0
PT_fle_plot_df$upper.CL[1:3] <- 0

PT_fle_plot_df$time <- as.factor(PT_fle_plot_df$time)
PT_fle_plot_df$time <- recode(
  PT_fle_plot_df$time,
  "1" = "Pre-BS",
  "2" = "1-month post-BS",
  "3" = "6-months post-BS",
  "4" = "12-months post-BS"
)

PT_fle_plot_df$attend_cat <- factor(
  PT_fle_plot_df$attend_cat,
  levels = c(
    "Control", 
    "Under 50% training attendance", 
    "Over 50% training attendance"
  )
)
PT_fle_plot_df$attend_cat <- recode(
  PT_fle_plot_df$attend_cat,
  "Control" = "Control group",
  "Under 50% training attendance" = "Exercise group (under 50% training attendance)",
  "Over 50% training attendance" = "Exercise group (over 50% training attendance)"
)

# Peak torque knee extension / body mass
PT_ext_body_mass_plot_df <- read.csv("output/interaction_PT_ext_body_mass_emm.csv")
PT_ext_body_mass_plot_df$lower.CL[1:3] <- 0
PT_ext_body_mass_plot_df$upper.CL[1:3] <- 0

PT_ext_body_mass_plot_df$time <- as.factor(PT_ext_body_mass_plot_df$time)
PT_ext_body_mass_plot_df$time <- recode(
  PT_ext_body_mass_plot_df$time,
  "1" = "Pre-BS",
  "2" = "1-month post-BS",
  "3" = "6-months post-BS",
  "4" = "12-months post-BS"
)

PT_ext_body_mass_plot_df$attend_cat <- factor(
  PT_ext_body_mass_plot_df$attend_cat,
  levels = c(
    "Control", 
    "Under 50% training attendance", 
    "Over 50% training attendance"
  )
)
PT_ext_body_mass_plot_df$attend_cat <- recode(
  PT_ext_body_mass_plot_df$attend_cat,
  "Control" = "Control group",
  "Under 50% training attendance" = "Exercise group (under 50% training attendance)",
  "Over 50% training attendance" = "Exercise group (over 50% training attendance)"
)

# Peak torque knee flexion / body mass
PT_fle_body_mass_plot_df <- read.csv("output/interaction_PT_fle_body_mass_emm.csv")
PT_fle_body_mass_plot_df$lower.CL[1:3] <- 0
PT_fle_body_mass_plot_df$upper.CL[1:3] <- 0

PT_fle_body_mass_plot_df$time <- as.factor(PT_fle_body_mass_plot_df$time)
PT_fle_body_mass_plot_df$time <- recode(
  PT_fle_body_mass_plot_df$time,
  "1" = "Pre-BS",
  "2" = "1-month post-BS",
  "3" = "6-months post-BS",
  "4" = "12-months post-BS"
)

PT_fle_body_mass_plot_df$attend_cat <- factor(
  PT_fle_body_mass_plot_df$attend_cat,
  levels = c(
    "Control", 
    "Under 50% training attendance", 
    "Over 50% training attendance"
  )
)
PT_fle_body_mass_plot_df$attend_cat <- recode(
  PT_fle_body_mass_plot_df$attend_cat,
  "Control" = "Control group",
  "Under 50% training attendance" = "Exercise group (under 50% training attendance)",
  "Over 50% training attendance" = "Exercise group (over 50% training attendance)"
)
# Overall plots config
dodge <- position_dodge(0.2)

# Plot peak torque knee extension -----------------------------------------

PT_ext_plot <- ggplot(data = PT_ext_plot_df) +
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
    y = "% knee extension peak torque change"
  )

# Plot peak torque knee flexion -------------------------------------------

PT_fle_plot <- ggplot(data = PT_fle_plot_df) +
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
    y = "% knee flexion peak torque change"
  )

# Plot peak torque knee extension / body mass -----------------------------

PT_ext_body_mass_plot <- ggplot(data = PT_ext_body_mass_plot_df) +
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
    y = "% knee extension peak torque relative to body mass change"
  )

# Plot peak torque knee flexion / body mass -------------------------------

PT_fle_body_mass_plot <- ggplot(data = PT_fle_body_mass_plot_df) +
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
    y = "% knee flexion peak torque relative to body mass change"
  )