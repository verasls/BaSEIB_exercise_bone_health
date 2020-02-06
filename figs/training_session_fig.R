# Load packages -----------------------------------------------------------

library(tidyverse)

# Load and prepare data ---------------------------------------------------

plot_df <- read_csv("data/training_session_data.csv", skip = 10) %>% 
  mutate(Resultant = sqrt(`Accelerometer X`^2 + `Accelerometer Y`^2 + `Accelerometer Z`^2))

max <- which(plot_df$Timestamp == "21-02-2018 19:32:59.990")
plot_df <- plot_df[1:max, ]

plot_df$Timestamp <- as.POSIXct(
  plot_df$Timestamp,
  format = "%d-%m-%Y %H:%M:%S", 
  tz = "GMT", 
  usetz = FALSE
)

# Plot --------------------------------------------------------------------

plot <- ggplot(data = plot_df) +
  geom_line(mapping = aes(x = Timestamp, y = Resultant)) +
  geom_hline(yintercept = 4, linetype = "dashed") +
  scale_x_datetime(
    date_breaks = "5 min",
    date_labels = "%H:%M:%S",
    expand = c(0, 0)
  ) +
  scale_y_continuous(breaks = seq(0, 10, 1)) +
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
ggsave(
  filename = "figs/training_session_fig.pdf",
  plot = plot, width = 50, height = 40, dpi = 200, units = "cm"
)