# Load packages -----------------------------------------------------------

library(tidyverse)

# Load and prepare data ---------------------------------------------------

body_mass_plot_df <- read.csv("output/interaction_body_mass_emm.csv") %>% 
  mutate(variable = "% body mass change")
BMI_plot_df <- read.csv("output/interaction_BMI_emm.csv") %>% 
  mutate(variable = "% body mass index change")
fat_mass_plot_df <- read.csv("output/interaction_fat_mass_emm.csv") %>% 
  mutate(variable = "% whole body fat mass change")
lean_mass_plot_df <- read.csv("output/interaction_lean_mass_emm.csv") %>% 
  mutate(variable = "% whole body lean mass change")

# Combine into a single data frame
delta_BC_plot_df <- body_mass_plot_df %>% 
  rbind(
    BMI_plot_df,
    fat_mass_plot_df,
    lean_mass_plot_df
  ) %>% 
  filter(time == 4)

# Refactor variables
delta_BC_plot_df$variable <- factor(
  delta_BC_plot_df$variable,
  levels = c(
    "% body mass change", 
    "% body mass index change", 
    "% whole body fat mass change",
    "% whole body lean mass change"
  )
)
delta_BC_plot_df$attend_cat <- factor(
  delta_BC_plot_df$attend_cat,
  levels = c(
    "Control", 
    "Under 50% training attendance", 
    "Over 50% training attendance"
  )
)
delta_BC_plot_df$attend_cat <- recode(
  delta_BC_plot_df$attend_cat,
  "Control" = "CG: n  = 16",
  "Under 50% training attendance" = "EG<50%: n = 19",
  "Over 50% training attendance" = "EG≥50%: n = 15"
)

# Overall plots config
dodge <- position_dodge(0.2)

# Body mass plot ----------------------------------------------------------

body_mass_plot <- ggplot(data = filter(delta_BC_plot_df, variable == "% body mass change")) +
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
  scale_y_continuous(limits = c(-45, -25), breaks = seq(-45, 0, 5)) +
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

BMI_plot <- ggplot(data = filter(delta_BC_plot_df, variable == "% body mass index change")) +
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
  scale_y_continuous(limits = c(-45, -25), breaks = seq(-45, 0, 5)) +
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

fat_mass_plot <- ggplot(data = filter(delta_BC_plot_df, variable == "% whole body fat mass change")) +
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
  scale_y_continuous(limits = c(-65, -40), breaks = seq(-65, -40, 5)) +
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

lean_mass_plot <- ggplot(data = filter(delta_BC_plot_df, variable == "% whole body lean mass change")) +
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
  scale_y_continuous(limits = c(-25, -5), breaks = seq(-25, -5, 5)) +
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
  ) +
  annotate("text", x = 3, y = -11.5, label = "a")