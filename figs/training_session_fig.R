# Load packages -----------------------------------------------------------

library(tidyverse)

# Load and prepare data ---------------------------------------------------

plot_df <- read_csv("data/training_session_data.csv", skip = 10) %>% 
  mutate(Resultant = sqrt(`Accelerometer X`^2 + `Accelerometer Y`^2 + `Accelerometer Z`^2))

max <- which(plot_df$Timestamp == "21-02-2018 19:37:59.990")
plot_df <- plot_df[1:max, ]

plot_df$Timestamp <- as.POSIXct(
  plot_df$Timestamp,
  format = "%d-%m-%Y %H:%M:%S", 
  tz = "GMT", 
  usetz = FALSE
)

for (i in 1:nrow(plot_df)) {
  print(i)
  if (plot_df$Resultant[i] < 1) {
    plot_df$Resultant[i] <- 1
  }
}

# Plot --------------------------------------------------------------------

plot <- ggplot(data = plot_df) +
  geom_line(mapping = aes(x = Timestamp, y = Resultant)) +
  geom_hline(yintercept = 4.9, linetype = "dashed") +
  scale_x_datetime(
    date_breaks = "5 min",
    date_labels = "%H:%M",
    expand = c(0, 0)
  ) +
  scale_y_continuous(breaks = seq(0, 10, 1), limits = c(1, 10)) +
  coord_cartesian(ylim = c(1, 9)) +
  theme_classic() +
  theme(
    axis.title.y = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.text.x = element_text(size = 14)
  ) +
  labs(
    x = "",
    y = "Acceleration (g)")
  )

# Uncomment lines below to save plot
# ggsave(
#   filename = "figs/training_session_fig.pdf",
#   plot = plot, width = 50, height = 40, dpi = 200, units = "cm"
# )