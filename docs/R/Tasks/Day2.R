# Task 1: Exploratory plots with ggplot2 ------------------------------------

## Getting started ---------------------------------------------------------

# install.packages("tidyverse")
library(tidyverse)

penguins

## Bill length vs. bill depth (scatterplot) --------------------------------

ggplot(
  penguins,
  aes(
    x = bill_len,
    y = bill_dep
  )
) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)


# or short
ggplot(penguins, aes(bill_len, bill_dep)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# color as aesthetic local to the point layer
ggplot(penguins, aes(x = bill_len, y = bill_dep)) +
  geom_point(aes(color = species)) +
  geom_smooth(method = "lm", se = FALSE)

# Option B: Define color aesthetic once globally
ggplot(
  penguins,
  aes(
    x = bill_len,
    y = bill_dep,
    color = species
  )
) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

## Flipper length by species (boxplot) -------------------------------------

# Basic boxplot of flipper length
ggplot(penguins, aes(species, flipper_len)) +
  geom_boxplot()

## Body mass by sex and species (boxplot) ----------------------------------

# Basic boxplot of body mass for penguins of different sex
ggplot(penguins, aes(x = sex, y = body_mass)) +
  geom_boxplot()

# Species as color aesthetic:
ggplot(
  penguins,
  aes(
    x = sex,
    y = body_mass
  )
) +
  geom_boxplot(aes(color = species))

# Species as fill aesthetic
ggplot(penguins, aes(x = sex, y = body_mass)) +
  geom_boxplot(aes(fill = species))

# Species as facets:
ggplot(penguins, aes(x = sex, y = body_mass)) +
  geom_boxplot() +
  facet_wrap(vars(species))

## Flipper length distribution by species (histogram) ----------------------

# Overlapping distributions
ggplot(
  penguins,
  aes(
    x = flipper_len,
    fill = species
  )
) +
  geom_histogram(alpha = 0.5, position = "identity")

# Stacked distributions
ggplot(
  penguins,
  aes(
    x = flipper_len,
    fill = species
  )
) +
  geom_histogram()

# Separated by facets
ggplot(
  penguins,
  aes(
    x = flipper_len,
    fill = species
  )
) +
  geom_histogram() +
  facet_wrap(vars(species), ncol = 1)

## For the fast ones -------------------------------------------------------

# Combine points and boxplots (with jitter)
ggplot(penguins, aes(x = species, y = flipper_len)) +
  geom_boxplot() +
  geom_point(position = position_jitter(seed = 123, width = 0.2, height = 0))

# Violin + boxplot combined
ggplot(penguins, aes(x = sex, y = body_mass)) +
  geom_violin() +
  geom_boxplot(width = 0.4) +
  facet_wrap(vars(species))

# Heatmap
ggplot(
  penguins,
  aes(
    x = species,
    y = sex,
    fill = flipper_len
  )
) +
  geom_tile()

# Task 2: Beautify and save plots -----------------------------------------

## Add labels --------------------------------------------------------------

ggplot(penguins, aes(species, flipper_len, color = species)) +
  geom_boxplot(width = 0.3) +
  geom_point(
    alpha = 0.5,
    position = position_jitter(width = 0.2, height = 0, seed = 123)
  ) +
  labs(x = "Species", y = "Flipper length (mm)")

## Change the colors -------------------------------------------------------

library(paletteer)

ggplot(penguins, aes(species, flipper_len, color = species)) +
  geom_boxplot(width = 0.3) +
  geom_point(
    alpha = 0.5,
    position = position_jitter(width = 0.2, height = 0, seed = 123)
  ) +
  scale_color_paletteer_d("ggsci::default_uchicago") +
  labs(x = "Species", y = "Flipper length (mm)")

## Apply a theme -----------------------------------------------------------

ggplot(penguins, aes(species, flipper_len, color = species)) +
  geom_boxplot(width = 0.3) +
  geom_point(
    alpha = 0.5,
    position = position_jitter(width = 0.2, height = 0, seed = 123)
  ) +
  scale_color_paletteer_d("ggsci::default_uchicago") +
  labs(x = "Species", y = "Flipper length (mm)") +
  theme_minimal() +
  theme(legend.position = "none")

## Reproduce the plot from the presentation --------------------------------

# The following code is adapted from the palmerpengins package website
# (https://allisonhorst.github.io/palmerpenguins/articles/examples.html).

penguin_scatter <- ggplot(
  data = penguins,
  aes(
    x = bill_len,
    y = bill_dep,
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
    caption = "Data from Gorman et al. (2014)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.85, 0.15),
    legend.background = element_rect(fill = "white", color = "white"),
    axis.title = element_text(size = 12),
    plot.caption.position = "plot"
  )
penguin_scatter

## Save your plot ----------------------------------------------------------

flipper_box <- ggplot(
  penguins,
  aes(species, flipper_len, color = species)
) +
  geom_boxplot(width = 0.3) +
  geom_jitter(
    alpha = 0.5,
    position = position_jitter(width = 0.2, seed = 123)
  ) +
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

## For the fast ones -------------------------------------------------------

# Beautified heatmap
heatmap <- ggplot(
  penguins,
  aes(
    x = species,
    y = sex,
    fill = flipper_len
  )
) +
  geom_tile() +
  scale_fill_viridis_c()
heatmap

# or with another color scale
heatmap <- ggplot(
  penguins,
  aes(x = species, y = sex, fill = flipper_len)
) +
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
plot_1 <- ggplot(
  penguins,
  aes(
    x = bill_len,
    y = bill_dep,
    color = species
  )
) +
  geom_point()

plot_2 <- ggplot(
  penguins,
  aes(
    x = bill_len,
    y = body_mass,
    color = species
  )
) +
  geom_point()

# Simple combination of 2 plots in patchwork
plot_1 + plot_2

# more complex combination with annotation and definition of shared layers
final_plot <- plot_1 +
  plot_2 +
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

# dplyr Task 1: Filter, select, and mutate ---------------------------------

## Filter penguins ---------------------------------------------------------

# 1. bill length between 40 and 45 mm
filter(penguins, bill_len >= 40 & bill_len <= 45)

# 2. species Adelie or Gentoo
filter(penguins, species %in% c("Adelie", "Gentoo"))
# or
filter(penguins, species == "Adelie" | species == "Gentoo")

# 3. lived on Dream in 2007
filter(penguins, island == "Dream" & year == 2007)

## Remove missing values ---------------------------------------------------

# 4. remove penguins with missing sex
drop_na(penguins, sex)

## Select columns ----------------------------------------------------------

# 5. only species, sex, and year
select(penguins, species, sex, year)

# 6. columns that start with "bill"
select(penguins, starts_with("bill"))

## Add new columns ---------------------------------------------------------

# 7. ratio of bill length to bill depth
mutate(penguins, ratio = bill_len / bill_dep)

# 8. abbreviations for the species
mutate(
  penguins,
  species_short = case_when(
    species == "Adelie" ~ "A",
    species == "Gentoo" ~ "G",
    species == "Chinstrap" ~ "C",
    .default = NA
  )
)

## Combine with the pipe ---------------------------------------------------

# 9. remove NA sex, keep Adelie, select species/sex/body_mass
penguins |>
  drop_na(sex) |>
  filter(species == "Adelie") |>
  select(species, sex, body_mass)

## For the fast ones -------------------------------------------------------

# filter_out penguins from Torgersen, then select columns
penguins |>
  filter_out(island == "Torgersen") |>
  select(species, island, flipper_len)

# size_category with case_when in a pipe
penguins |>
  drop_na(body_mass) |>
  mutate(
    size_category = case_when(
      body_mass < 3500 ~ "small",
      body_mass < 5000 ~ "medium",
      body_mass >= 5000 ~ "large"
    )
  ) |>
  select(species, body_mass, size_category)

# dplyr Task 2: Summarize and visualize -----------------------------------

## Count -------------------------------------------------------------------

# 1. number of penguins on each island
count(penguins, island)

# 2. number of penguins of each species on each island
count(penguins, island, species)

## Summarize ---------------------------------------------------------------

# 3. mean flipper length and body mass by species
penguins |>
  summarize(
    mean_flipper = mean(flipper_len, na.rm = TRUE),
    mean_body = mean(body_mass, na.rm = TRUE),
    .by = species
  )

# 4. mean flipper length and body mass by species and sex, remove unknown sex
penguins |>
  drop_na(sex) |>
  summarize(
    mean_flipper = mean(flipper_len, na.rm = TRUE),
    mean_body = mean(body_mass, na.rm = TRUE),
    .by = c(species, sex)
  )

## Combine dplyr and ggplot ------------------------------------------------

# 5. remove missing sex, boxplot of body mass by sex
penguins |>
  drop_na(sex) |>
  ggplot(aes(x = sex, y = body_mass)) +
  geom_boxplot()

# 6. remove missing sex, scatterplot bill length vs bill depth by species
penguins |>
  drop_na(sex) |>
  ggplot(aes(x = bill_len, y = bill_dep, color = species)) +
  geom_point()

## For the fast ones -------------------------------------------------------

# Tricky: summarize mean body mass by species, pipe into geom_col()
penguins |>
  summarize(
    mean_body = mean(body_mass, na.rm = TRUE),
    .by = species
  ) |>
  ggplot(aes(x = species, y = mean_body)) +
  geom_col()

# min, max, and mean flipper length per species
penguins |>
  summarize(
    min_flipper = min(flipper_len, na.rm = TRUE),
    max_flipper = max(flipper_len, na.rm = TRUE),
    mean_flipper = mean(flipper_len, na.rm = TRUE),
    .by = species
  )

# sort by mean flipper length
penguins |>
  summarize(
    min_flipper = min(flipper_len, na.rm = TRUE),
    max_flipper = max(flipper_len, na.rm = TRUE),
    mean_flipper = mean(flipper_len, na.rm = TRUE),
    .by = species
  ) |>
  arrange(mean_flipper)
