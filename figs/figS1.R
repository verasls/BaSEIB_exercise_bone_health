# Load packages -----------------------------------------------------------

library(tidyverse)
library(cowplot)

# Load and prepare data ---------------------------------------------------

source("figs/plot_body_composition.R")
source("figs/plot_physical_activity.R")

# Plot --------------------------------------------------------------------

legend <- get_legend(body_mass_plot)

grid_1 <- plot_grid(
  body_mass_plot + theme(legend.position = "none"),
  BMI_plot + theme(legend.position = "none"),
  fat_mass_plot + theme(legend.position = "none"),
  lean_mass_plot + theme(legend.position = "none"),
  steps_plot + theme(legend.position = "none"),
  SB_plot + theme(legend.position = "none"),
  LPA_plot + theme(legend.position = "none"),
  MVPA_plot + theme(legend.position = "none"),
  labels = c("A", "", "", "", "B", "", "", ""),
  ncol = 4, nrow = 2
)

grid <- plot_grid(legend, grid_1, ncol = 1, rel_heights = c(0.1, 1))

# Uncomment lines below to save plot
# ggsave(
#   filename = "figs/figS1.tiff",
#   plot = grid, width = 80, height = 40, dpi = 600, units = "cm"
# )