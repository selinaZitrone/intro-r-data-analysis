# load the necessary libraries
library(ggplot2)
library(palmerpenguins)
library(dplyr)

# Create a summary of average bill length and body mass by species
# They will be plotted together in one plot
penguin_summary <- penguins |>
  summarise(
    avg_bill_length = mean(bill_length_mm, na.rm = TRUE),
    avg_body_mass = mean(body_mass_g, na.rm = TRUE),
    .by = species
  )

# Create a ggplot with a secondary y-axis
ggplot(penguin_summary, aes(x = species)) +
  # Bill length on primary y-axis
  geom_bar(aes(y = avg_bill_length, fill = species), stat = "identity", alpha = 0.7) +
  # Body mass on secondary y-axis
  # Divide by 50 to make the scales more comparable
  geom_point(aes(y = avg_body_mass / 50), color = "red", size = 3) +
  geom_line(aes(y = avg_body_mass / 50, group = 1), color = "red", size = 1.5) +

  # Add a secondary y-axis that is 50* the primary y-axis because before
  # we divided the body mass by 50
  scale_y_continuous(
    name = "Average Bill Length (mm)",
    sec.axis = sec_axis(~.*50, name = "Average Body Mass (g)")
  ) +
  scale_fill_manual(values = c("darkorange", "purple", "cyan4")) +

  labs(title = "Penguin Bill Length and Body Mass by Species",
       x = "Penguin Species",
       fill = "Species") +

  theme_minimal() +
  # Here we can change the theme for the the y-axis on the left and on the right
  theme(
    axis.title.y.left = element_text(color = "black", size = 13),
    axis.title.y.right = element_text(color = "red", size = 13),
    axis.text.y.right = element_text(color = "red"),
    legend.position = "bottom"
  )
