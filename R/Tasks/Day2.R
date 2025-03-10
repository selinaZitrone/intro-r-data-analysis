# 1. Data visualization in ggplot2 -------------------------------------------

# 1.1 and 1.2 Getting started with ggplot and palmerpenguins------------

# install.packages("tidyverse")
# install.packages("palmerpenguins")
library(tidyverse)
library(palmerpenguins)

penguins

# 1.3 Exploratory plotting ---------------------------------

# 1.3.1 Relationship between bill length and bill depth (scatterplot)

ggplot(
  penguins,
  aes(
    x = bill_length_mm,
    y = bill_depth_mm
  )
) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)


# or short
ggplot(penguins, aes(bill_length_mm, bill_depth_mm)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# color as aesthetic local to the point layer
ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point(aes(color = species)) +
  geom_smooth(method = "lm", se = FALSE)

# Option B: Define color aesthetic once globally
ggplot(penguins, aes(
  x = bill_length_mm,
  y = bill_depth_mm,
  color = species
)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# 1.3.2 Difference in flipper length between species (boxplot)

# Basic boxplot of flipper length with notches
ggplot(penguins, aes(species, flipper_length_mm)) +
  geom_boxplot(notch = TRUE) +
  geom_point()

# Add jittered points
ggplot(penguins, aes(x = species, y = flipper_length_mm)) +
  geom_boxplot() +
  geom_point(position = position_jitter(seed = 123, width = 0.2))

## 1.3.3 Differences between body mass of male and female penguins (boxplot)

# Basic boxplot of body mass for penguins of different sex
ggplot(penguins, aes(x = sex, y = body_mass_g)) +
  geom_boxplot()

# Species as color aesthetic:

ggplot(penguins, aes(
  x = sex,
  y = body_mass_g
)) +
  geom_boxplot(aes(color = species))

# Species as fill aesthetic
ggplot(penguins, aes(x = sex, y = body_mass_g)) +
  geom_boxplot(aes(fill = species))

# Species as facets:
ggplot(penguins, aes(x = sex, y = body_mass_g)) +
  geom_boxplot() +
  facet_wrap(~species)

# With geom_violin
ggplot(penguins, aes(x = sex, y = body_mass_g)) +
  geom_violin() +
  geom_boxplot(width = 0.4) +
  facet_wrap(~species)

## 1.3.4 Distribution of flipper length between species (histogram)

# Overlapping
ggplot(penguins, aes(
  x = flipper_length_mm,
  color = species
)) +
  geom_histogram(alpha = 0.5, position = "identity")

# Stacked
ggplot(penguins, aes(
  x = flipper_length_mm,
  fill = species
)) +
  geom_histogram()

ggplot(penguins, aes(
  x = flipper_length_mm,
  fill = species
)) +
  geom_histogram() +
  facet_wrap(~species, scales = "free_x", ncol = 1)

#### 1.3.5 Penguin flipper length by species and sex (heatmap)

ggplot(penguins, aes(
  x = species,
  y = sex,
  fill = flipper_length_mm
)) +
  geom_tile()

ggplot(
  data = penguins,
  aes(
    x = sex,
    y = body_mass_g,
    fill = species,
    color = species
  )
) +
  geom_boxplot(position = position_dodge(width = 0.9), notch = TRUE) +
  geom_point(
    alpha = 0.5,
    position = position_jitterdodge(
      seed = 123, jitter.width = 0.2,
      dodge.width = 0.9
    )
  )

# 1.4 Beautify the plots --------------------------------------------------

## 1.4.1 Beatuify plots from 1.3

# Example one: Boxplot of flipper length and species
library(paletteer)

ggplot(penguins, aes(species, flipper_length_mm, color = species)) +
  geom_boxplot(width = 0.3) +
  geom_point(
    alpha = 0.5,
    position = position_jitter(width = 0.2, seed = 123)
  ) +
  scale_color_paletteer_d("ggsci::default_uchicago") +
  labs(x = "Species", y = "Flipper length (mm)") +
  theme_minimal() +
  theme(legend.position = "none")

# Example two: Reproducing the plot from the presentation

# The following code is adapted from the palmerpengins package website
# (https://allisonhorst.github.io/palmerpenguins/articles/examples.html).

penguin_scatter <- ggplot(
  data = penguins,
  aes(
    x = bill_length_mm,
    y = bill_depth_mm,
    color = species,
    shape = species
  )
) +
  geom_point(size = 3, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values = c("darkorange", "purple", "cyan4")) +
  labs(
    title = "Penguin bill dimensions",
    subtitle = "Bill length and depth for Adelie, Chinstrap and Gentoo Penguins at Palmer Station LTER",
    x = "Bill length (mm)",
    y = "Bill depth (mm)",
    color = "Penguin species",
    shape = "Penguin species",
    caption = "Some caption"
  ) +
  theme_minimal() +
  theme(
    legend.position = c(0.85, 0.15),
    legend.background = element_rect(fill = "white", color = "white"),
    axis.title = element_text(size = 12),
    plot.caption.position = "plot"
  )
penguin_scatter

### 1.5 Save one of the plots on your machine
flipper_box <- ggplot(penguins, aes(species, flipper_length_mm, color = species)) +
  geom_boxplot(width = 0.3) +
  geom_jitter(alpha = 0.5, position = position_jitter(width = 0.2, seed = 123)) +
  ggsci::scale_color_uchicago() +
  labs(x = "Species", y = "Flipper length (mm)") +
  theme_minimal() +
  theme(legend.position = "none")

# save as png in /img directory of the project
ggsave(
  filename = "img/flipper_box.png",
  flipper_box,
  width = 8,
  height = 8,
  units = "cm",
  dpi = 600,
  scale = 1.3
)

# save as pdf in /img directory of the project
ggsave(filename = "img/flipper_box.pdf", flipper_box)

# heatmap example

heatmap <- ggplot(penguins, aes(
  x = species,
  y = sex,
  fill = flipper_length_mm
)) +
  geom_tile() +
  scale_fill_viridis_c()
heatmap

# or with another color scale

heatmap <- ggplot(penguins, aes(x = species, y = sex, fill = flipper_length_mm)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "steelblue")

# If you want to have an interactive plot
# install.packages("plotly")
plotly::ggplotly(heatmap)

# Combined plots using patchwork package ----------------------------------
# More info here: https://patchwork.data-imaginist.com/
library(patchwork)

# Simply combined the plots side by side
flipper_box + penguin_scatter

# Combine on top of each other
flipper_box / penguin_scatter
# Combined next to each other
flipper_box | penguin_scatter

# Collecting legends and defining a common theme
plot_1 <- ggplot(penguins, aes(
  x = bill_length_mm, y = bill_depth_mm,
  color = species
)) +
  geom_point()

plot_2 <- ggplot(penguins, aes(
  x = bill_length_mm, y = body_mass_g,
  color = species
)) +
  geom_point()

# Simple combination of 2 plots in patchwork
plot_1 + plot_2

# more complex combination with annotation and definition of shared layers
final_plot <- plot_1 + plot_2 +
  plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")") &
  theme_minimal() &
  scale_color_manual(values = c("darkorange", "purple", "cyan4")) &
  labs(
    color = "Penguin species"
  ) &
  theme(
    plot.tag.position = c(0.1, 0.95),
    plot.tag = element_text(face = "bold")
  )
final_plot

ggsave("img/patchwork.png", final_plot)


# Use esquisser for visual plotting ---------------------------------------
# Nice package for visually designing plots
# More here: https://github.com/dreamRs/esquisse
# install.packages("esquisse")

esquisse::esquisser(penguins)

# 2 dplyr -------------------------------------------------------------------

# library(tidyverse)
# library(palmerpenguins)
# … with abbreviations for the species (Adelie = A, Gentoo = G, Chinstrap = C).

# Filter tasks -------------------------------------------------------------------

# bill length between 40 and 45 mm.
filter(penguins, between(bill_length_mm, 40, 45))
# same as
filter(penguins, bill_length_mm < 45 & bill_length_mm > 40)

# for which we know the sex.

filter(penguins, !is.na(sex))

# which are of the species Adelie or Gentoo
filter(
  penguins,
  species %in% c("Adelie", "Gentoo")
)

filter(penguins, (species == "Adelie" | species == "Gentoo"))

# lived on Dream in 2007
# How many of them were from each of the 3 species?

filter(penguins, island == "Dream" & year == 2007) |>
  count(species)

filter(penguins, island == "Dream") |>
  filter(year == 2007) |>
  count(species)

# Count tasks -------------------------------------------------------------

# number of penguins on each island

count(penguins, island)

# number of penguins of each species on each island.

count(penguins, island, species)

# Select tasks --------------------------------------------------------------

# only the variables species, sex and year
select(penguins, species, sex, year)

# only columns that contain measurements in mm
select(penguins, ends_with("mm"))
# or
select(penguins, contains("_mm"))

# Mutate tasks ------------------------------------------------------------

# with the ratio of bill length to bill depth
mutate(penguins,
  ratio = bill_length_mm / bill_depth_mm
)

# abbreviations for the species (Adelie = A, Gentoo = G, Chinstrap = C)
mutate(penguins,
  species_short = case_when(
    species == "Adelie" ~ "A",
    species == "Gentoo" ~ "G",
    species == "Chinstrap" ~ "C"
  )
)

# Summary tasks -----------------------------------------------------------

# mean flipper length and body mass for the 3 species and
# male and female penguins separately

penguins |>
  summarize(
    mean_flipper = mean(flipper_length_mm, na.rm = TRUE),
    mean_body = mean(body_mass_g, na.rm = TRUE),
    .by = c(species, sex)
  )

# same but remove the penguins with unknown sex

penguins |>
  filter(!is.na(sex)) |>
  summarize(
    mean_flipper = mean(flipper_length_mm, na.rm = TRUE),
    mean_body = mean(body_mass_g, na.rm = TRUE),
    .by = c(species, sex)
  )

### Extras

# boxplot of penguin body mass with sex on the x-axis
# facets for the different species.
# remove the penguins with missing values for sex first

penguins |>
  filter(!is.na(sex)) |>
  ggplot(aes(x = sex, y = body_mass_g)) +
  geom_boxplot() +
  facet_wrap(~species) +
  labs(caption = "Some caption") +
  theme(plot.caption = element_text(face = "bold", hjust = 1))

# scatterplot with ratio of bill length to bill depth on the y axis
# and flipper length on the x axis
# distinguish between male and female penguins
# remove penguins with unknown sex before making the plot

penguins |>
  mutate(ratio = bill_length_mm / bill_depth_mm) |>
  filter(!is.na(sex)) |>
  ggplot(aes(x = flipper_length_mm, y = ratio, color = sex)) +
  geom_point() +
  scale_color_manual(values = c("cyan4", "darkorange")) +
  labs(
    x = "Flipper lenght (mm)",
    y = "Ratio bill length / bill depth (-)"
  ) +
  theme_minimal()

# Sort tasks --------------------------------------------------------------

# penguins with lowest body mass first

arrange(penguins, body_mass_g)

# penguins with highest body mass first

arrange(penguins, desc(body_mass_g))

# penguins by species and sex, with longest flippers first

arrange(penguins, species, sex, desc(flipper_length_mm))
