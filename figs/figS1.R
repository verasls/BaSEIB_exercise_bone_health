# Load packages -----------------------------------------------------------

library(ggplot2)
library(cowplot)
library(here)

# Source code that creates subplots ---------------------------------------

source(here("figs", "plot_body_composition.R"))
source(here("figs", "plot_strength.R"))
source(here("figs", "plot_physical_activity.R"))
source(here("figs", "plot_n_peaks.R"))

# Create the plot grid ----------------------------------------------------

legend <- get_legend(BM_plot)

grid_1 <- plot_grid(
  BM_plot + theme(legend.position = "none"),
  BMI_plot + theme(legend.position = "none"),
  FM_plot + theme(legend.position = "none"),
  LM_plot + theme(legend.position = "none"),
  ext_plot + theme(legend.position = "none"),
  fle_plot + theme(legend.position = "none"),
  rel_ext_plot + theme(legend.position = "none"),
  rel_fle_plot + theme(legend.position = "none"),
  steps_plot + theme(legend.position = "none"),
  SB_plot + theme(legend.position = "none"),
  LPA_plot + theme(legend.position = "none"),
  MVPA_plot + theme(legend.position = "none"),
  acc_plot + theme(legend.position = "none"),
  ncol = 2, nrow = 7
)

grid <- plot_grid(legend, grid_1, ncol = 1, rel_heights = c(0.02, 1))

ggsave(
  filename = "figs/figS1.tiff",
  plot = grid, width = 40, height = 140, dpi = 150, units = "cm",
  limitsize = FALSE
)
