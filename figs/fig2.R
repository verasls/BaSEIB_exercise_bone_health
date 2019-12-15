# Load packages -----------------------------------------------------------

library(tidyverse)

# Load and prepare data ---------------------------------------------------

delta_TH_plot_df <- read.csv("output/interaction_delta_TH_emm.csv") %>% 
  mutate(region = "Total hip")
delta_FN_plot_df <- read.csv("output/interaction_delta_FN_emm.csv") %>% 
  mutate(region = "Femoral neck")
delta_LS_plot_df <- read.csv("output/interaction_delta_LS_emm.csv") %>% 
  mutate(region = "Lumbar spine")
delta_TR_plot_df <- read.csv("output/interaction_delta_TR_emm.csv") %>% 
  mutate(region = "One-third radius")

# Combine into a single data frame
delta_plot_df <- delta_TH_plot_df %>% 
  rbind(
    delta_FN_plot_df,
    delta_LS_plot_df,
    delta_TR_plot_df
  ) %>% 
  filter(time == 4)

# Refactor variables
delta_plot_df$region <- factor(
  delta_plot_df$region,
  levels = c("Total hip", "Femoral neck", "Lumbar spine", "One-third radius")
)
delta_plot_df$attend_cat <- recode(
  delta_plot_df$attend_cat,
  "Control" = "Control group",
  "Under 50% training attendance" = "Exercise group (under 50% training attendance)",
  "Over 50% training attendance" = "Exercise group (over 50% training attendance)"
)

# Overall plots config
dodge <- position_dodge(0.4)

# Plot --------------------------------------------------------------------

delta_plot <- ggplot(data = delta_plot_df) +
  geom_point(
    aes(x = region, y = emmean, colour = attend_cat),
    position = dodge, size = 2
  ) +
  geom_errorbar(
    aes(x = region, ymin = lower.CL, ymax = upper.CL, colour = attend_cat),
    position = dodge, size = 1, width = 0.4
  ) +
  scale_color_manual(values = c("#0072B2", "#E69F00", "#D55E00")) +
  labs(
    x = "",
    y = "% bone mineral density change",
    colour = "Group"
  ) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  scale_y_continuous(breaks = seq(-10, 4, 2)) +
  theme_classic()