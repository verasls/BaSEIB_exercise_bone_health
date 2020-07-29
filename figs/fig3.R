# Load packages -----------------------------------------------------------

library(tidyverse)
library(cowplot)

# Source code that creates subplots ---------------------------------------

source("figs/body_composition.R")
source("figs/acc_plot.R")

# Create the plot grid ----------------------------------------------------

legend <- get_legend(BM_plot)

grid_1 <- plot_grid(
  LM_plot + theme(legend.position = "none"),
  acc_plot + theme(legend.position = "none"),
  ncol = 2, nrow = 1
)

grid <- plot_grid(legend, grid_1, ncol = 1, rel_heights = c(0.1, 1))

ggsave(
  filename = "figs/fig3.tiff",
  plot = grid, width = 40, height = 20, dpi = 150, units = "cm"
)
