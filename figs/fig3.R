# Load packages -----------------------------------------------------------

library(tidyverse)

# Load and prepare data ---------------------------------------------------

delta_LS_plot_df <- read_csv("output/interaction_delta_LS_emm.csv") %>% 
  mutate(region = "Lumbar spine")
delta_TR_plot_df <- read_csv("output/interaction_delta_TR_emm.csv") %>% 
  mutate(region = "One-third radius")
delta_FN_plot_df <- read_csv("output/interaction_delta_FN_emm.csv") %>% 
  mutate(region = "Femoral neck")
delta_TH_plot_df <- read_csv("output/interaction_delta_TH_emm.csv") %>% 
  mutate(region = "Total hip")

# Combine into a single data frame
delta_plot_df <- delta_TH_plot_df %>%
  rbind(
    delta_FN_plot_df,
    delta_LS_plot_df,
    delta_TR_plot_df
  ) %>% 
  filter(time == 4) %>% 
  mutate(
    region = factor(
      region,
      levels = c("Lumbar spine", "One-third radius", "Femoral neck", "Total hip")
    ),
    attend_cat = recode(
      as.factor(attend_cat),
      "Control" = "Control group",
      "Under 50% training attendance" = "Exercise group (<50% training attendance)",
      "Over 50% training attendance" = "Exercise group (≥50% training attendance)"
    ),
    attend_cat = fct_relevel(
      attend_cat,
      c(
        "Control group",
        "Exercise group (<50% training attendance)",
        "Exercise group (≥50% training attendance)"
      )
    )
  )

# Overall plots config
dodge <- position_dodge(0.5)

# Plot --------------------------------------------------------------------

delta_plot <- ggplot(data = delta_plot_df) +
  geom_point(
    aes(x = region, y = emmean, shape = attend_cat, colour = attend_cat),
    position = dodge, size = 4
  ) +
  geom_errorbar(
    aes(x = region, ymin = lower.CL, ymax = upper.CL, group = attend_cat, colour = attend_cat),
    position = dodge, size = 1, width = 0.4
  ) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  scale_y_continuous(breaks = seq(-10, 4, 2)) +
  scale_x_discrete(
    labels = c(
      "Lumbar spine" = "Lumbar spine\n\nCG: n = 16         \nEG<50%: n = 17\nEG≥50%: n = 14",
      "One-third radius" = "One-third radius\n\nCG: n = 16         \nEG<50%: n = 18\nEG≥50%: n = 15",
      "Femoral neck" = "Femoral neck\n\nCG: n = 16         \nEG<50%: n = 19\nEG≥50%: n = 14",
      "Total hip" = "Total hip\n\nCG: n = 16         \nEG<50%: n = 19\nEG≥50%: n = 14"
    )
  ) +
  scale_color_manual(values = c("#0072B2", "#D55E00", "#009E73")) +
  theme_classic() +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    axis.title.y = element_text(size = 14),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12)
  ) +
  labs(
    x = "",
    y = "% bone mineral density change from pre-BS to 12-months post-BS"
  ) +
  annotate("text", x = 1.17, y = 1.40, label = "a'") +
  annotate("text", x = 3.00, y = - 2.70, label = "b") +
  annotate("text", x = 3.17, y = 2.45, label = "a")

ggsave(
  filename = "figs/fig4.tiff",
  plot = delta_plot, width = 30, height = 20, dpi = 150, units = "cm"
)