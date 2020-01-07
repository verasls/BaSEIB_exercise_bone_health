# Load packages -----------------------------------------------------------

library(tidyverse)

# Load and prepare data ---------------------------------------------------

# Body mass
body_mass_plot_df <- read.csv("output/interaction_body_mass_emm.csv")

body_mass_plot_df$time <- as.factor(body_mass_plot_df$time)
body_mass_plot_df$time <- recode(
  body_mass_plot_df$time,
  "1" = "Pre-BS",
  "2" = "1-month post-BS",
  "3" = "6-months post-BS",
  "4" = "12-months post-BS"
)

body_mass_plot_df$attend_cat <- factor(
  body_mass_plot_df$attend_cat,
  levels = c(
    "Control", 
    "Under 50% training attendance", 
    "Over 50% training attendance"
  )
)
body_mass_plot_df$attend_cat <- recode(
  body_mass_plot_df$attend_cat,
  "Control" = "Control group",
  "Under 50% training attendance" = "Exercise group (under 50% training attendance)",
  "Over 50% training attendance" = "Exercise group (over 50% training attendance)"
)

# BMI
BMI_plot_df <- read.csv("output/interaction_BMI_emm.csv")

BMI_plot_df$time <- as.factor(BMI_plot_df$time)
BMI_plot_df$time <- recode(
  BMI_plot_df$time,
  "1" = "Pre-BS",
  "2" = "1-month post-BS",
  "3" = "6-months post-BS",
  "4" = "12-months post-BS"
)

BMI_plot_df$attend_cat <- factor(
  BMI_plot_df$attend_cat,
  levels = c(
    "Control", 
    "Under 50% training attendance", 
    "Over 50% training attendance"
  )
)
BMI_plot_df$attend_cat <- recode(
  BMI_plot_df$attend_cat,
  "Control" = "Control group",
  "Under 50% training attendance" = "Exercise group (under 50% training attendance)",
  "Over 50% training attendance" = "Exercise group (over 50% training attendance)"
)

# Fat mass
fat_mass_plot_df <- read.csv("output/interaction_fat_mass_emm.csv")

fat_mass_plot_df$time <- as.factor(fat_mass_plot_df$time)
fat_mass_plot_df$time <- recode(
  fat_mass_plot_df$time,
  "1" = "Pre-BS",
  "2" = "1-month post-BS",
  "3" = "6-months post-BS",
  "4" = "12-months post-BS"
)

fat_mass_plot_df$attend_cat <- factor(
  fat_mass_plot_df$attend_cat,
  levels = c(
    "Control", 
    "Under 50% training attendance", 
    "Over 50% training attendance"
  )
)
fat_mass_plot_df$attend_cat <- recode(
  fat_mass_plot_df$attend_cat,
  "Control" = "Control group",
  "Under 50% training attendance" = "Exercise group (under 50% training attendance)",
  "Over 50% training attendance" = "Exercise group (over 50% training attendance)"
)

# Lean mass
lean_mass_plot_df <- read.csv("output/interaction_lean_mass_emm.csv")

lean_mass_plot_df$time <- as.factor(lean_mass_plot_df$time)
lean_mass_plot_df$time <- recode(
  lean_mass_plot_df$time,
  "1" = "Pre-BS",
  "2" = "1-month post-BS",
  "3" = "6-months post-BS",
  "4" = "12-months post-BS"
)

lean_mass_plot_df$attend_cat <- factor(
  lean_mass_plot_df$attend_cat,
  levels = c(
    "Control", 
    "Under 50% training attendance", 
    "Over 50% training attendance"
  )
)
lean_mass_plot_df$attend_cat <- recode(
  lean_mass_plot_df$attend_cat,
  "Control" = "Control group",
  "Under 50% training attendance" = "Exercise group (under 50% training attendance)",
  "Over 50% training attendance" = "Exercise group (over 50% training attendance)"
)

# Overall plots config
dodge <- position_dodge(0.2)

# Body mass plot ----------------------------------------------------------

body_mass_plot <- ggplot(data = body_mass_plot_df) +
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
    y = "% body mass change"
  )

# BMI plot ----------------------------------------------------------------

BMI_plot <- ggplot(data = BMI_plot_df) +
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
    y = "% body mass index change"
  )

# Fat mass plot -----------------------------------------------------------

fat_mass_plot <- ggplot(data = fat_mass_plot_df) +
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
    y = "% whole body fat mass change"
  )

# Lean mass plot ----------------------------------------------------------

lean_mass_plot <- ggplot(data = lean_mass_plot_df) +
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
    y = "% whole body lean mass change"
  )