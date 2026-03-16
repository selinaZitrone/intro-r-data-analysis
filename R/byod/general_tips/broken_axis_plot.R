# Excretion profile plot with a broken x-axis

library(tidyverse)

# Dummy dummy_dataa: 2 groups with unevenly spaced time points
dummy_data <- tibble(
  group = rep(c("A", "B"), each = 8),
  time = rep(c(1, 5, 10, 15, 20, 50, 100, 160), 2),
  value = c(
    0.5,
    0.3,
    0.15,
    0.08,
    0.04,
    0.01,
    0.002,
    0,
    0.2,
    0.18,
    0.1,
    0.05,
    0.02,
    0.005,
    0.001,
    0
  )
)

# Option 1: ggbreak - break the x-axis ---------------------------------------

# Check out the package here: https://cran.r-project.org/web/packages/ggbreak/vignettes/ggbreak.html
# install.packages("ggbreak")
library(ggbreak)

# First create the base plot with all data points
p <- ggplot(dummy_data, aes(x = time, y = value, color = group)) +
  geom_line() +
  geom_point(size = 2.5) +
  labs(x = "Time [h]", y = "Value") +
  theme_classic()

# Add a break in the x-axis between 25 and 45
# The space argument controls how wide the break region appears
p + scale_x_break(c(25, 45), space = 0.3)

# You can also add multiple breaks
p +
  scale_x_break(c(25, 45), space = 0.3) +
  scale_x_break(c(70, 90), space = 0.3)


# Option 2: patchwork --------------------------------------------------------
library(patchwork)

base_plot <- ggplot(dummy_data, aes(x = time, y = value, color = group)) +
  geom_line() +
  geom_point(size = 2.5) +
  labs(y = "Value") +
  theme_classic()

# Left panel: From 0-25 with limits
p_left <- base_plot +
  scale_x_continuous(limits = c(0, 25), breaks = seq(0, 25, by = 5)) +
  labs(x = NULL) +
  theme(legend.position = "none")

# Right panel: From 25-170
p_right <- base_plot +
  scale_x_continuous(limits = c(25, 170), breaks = seq(50, 150, by = 50)) +
  labs(x = NULL, y = NULL) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank()
  )

# Combine: widths control how much space each panel gets
p_left +
  p_right +
  plot_layout(widths = c(2, 1)) +
  plot_annotation(
    caption = "Time [h]",
    theme = theme(plot.caption = element_text(hjust = 0.5, size = 12))
  )
