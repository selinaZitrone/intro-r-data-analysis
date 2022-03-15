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
  data = penguins,
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
ggplot(penguins, aes(bill_length_mm, bill_depth_mm)) +
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
  geom_boxplot(notch = TRUE)

# Add jittered points
ggplot(penguins, aes(species, flipper_length_mm)) +
  geom_boxplot() +
  geom_point(position = position_jitter(seed = 123))


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


# 1.4 Beautify the plots --------------------------------------------------

## 1.4.1 Beatuify plots from 1.3

# Example one: Boxplot of flipper length and species

ggplot(penguins, aes(species, flipper_length_mm, color = species)) +
  geom_boxplot(width = 0.3) +
  geom_point(
    alpha = 0.5,
    position = position_jitter(width = 0.1, seed = 123)
  ) +
  ggsci::scale_color_uchicago() +
  labs(x = "Species", y = "Flipper length (mm)") +
  theme_minimal() +
  theme(legend.position = "none")

# Example two: Reproducing the plot from the presentation

# The following code is adapted from the palmerpengins package website
# (https://allisonhorst.github.io/palmerpenguins/articles/examples.html).

ggplot(
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
    shape = "Penguin species"
  ) +
  theme_minimal() +
  theme(
    legend.position = c(0.85, 0.15),
    legend.background = element_rect(fill = "white", color = NA)
  )

### 1.5 Save one of the plots on your machine
flipper_box <- ggplot(penguins, aes(species, flipper_length_mm, color = species)) +
  geom_boxplot(width = 0.3) +
  geom_jitter(alpha = 0.5, position = position_jitter(width = 0.2, seed = 2)) +
  ggsci::scale_color_uchicago() +
  labs(x = "Species", y = "Flipper length (mm)") +
  theme_minimal() +
  theme(legend.position = "none")

# save as png in /img directory of the project
ggsave(filename = "./img/flipper_box.png", flipper_box)
# save as pdf in /img directory of the project
ggsave(filename = "./img/flipper_box.pdf", flipper_box)

ggplot(
  data = penguins,
  aes(
    x = sex,
    y = body_mass_g,
    color = species
  )
) +
  geom_boxplot() +
  geom_point(
    alpha = 0.5,
    position = position_jitterdodge(jitter.width = 0.2)
  )

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
#install.packages("plotly")
plotly::ggplotly(heatmap)

# 2 dplyr -------------------------------------------------------------------

# library(tidyverse)
# library(palmerpenguins)


# Filter tasks -------------------------------------------------------------------

# bill length between 40 and 45 mm.
filter(penguins, between(bill_length_mm, 40, 45))
# same as
filter(penguins, bill_length_mm < 45 & bill_length_mm > 40)

# for which we know the sex.

filter(penguins, !is.na(sex))

# which are of the species Adelie or Gentoo and live either on Dream or Torgersen
filter(penguins,
       species %in% c("Adelie", "Gentoo") &
  island %in% c("Dream", "Torgersen"))

filter(penguins, (species == "Adelie" | species == "Gentoo") &
  (island == "Dream" | island == "Torgersen"))

# lived on Dream in 2007
# How many of them were from each of the 3 species?

filter(penguins, island == "Dream" & year == 2007) %>%
  count(species)

# Count tasks -------------------------------------------------------------

# number of penguins on each island

count(penguins, island)

# number of penguins of each species on each island.

count(penguins, island, species)

# Sort tasks --------------------------------------------------------------

# penguins with lowest body mass first

arrange(penguins, body_mass_g)

# penguins with highest body mass first

arrange(penguins, desc(body_mass_g))

# penguins by species and sex, with longest flippers first

arrange(penguins, species, sex, desc(flipper_length_mm))

penguins %>%
  group_by(species, sex) %>%
  arrange(desc(flipper_length_mm))

# Select tasks --------------------------------------------------------------

# only the variables species, sex and year

select(penguins, species, sex, year)

# variables based on the following vector
cols <- c("species", "bill_length_mm", "flipper_length_mm", "body_mass_kg")

select(penguins, any_of(cols))
# this would return  an error
select(penguins, all_of(cols))

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

penguins %>%
  group_by(species, sex) %>%
  summarize(
    mean_flipper = mean(flipper_length_mm, na.rm = TRUE),
    mean_body = mean(body_mass_g, na.rm = TRUE)
  )

# same but remove the penguins with unknown sex

penguins %>%
  filter(!is.na(sex)) %>%
  group_by(species, sex) %>%
  summarize(
    mean_flipper = mean(flipper_length_mm, na.rm = TRUE),
    mean_body = mean(body_mass_g, na.rm = TRUE)
  )

### Extras

# boxplot of penguin body mass with sex on the x-axis
# facets for the different species.
# remove the penguins with missing values for sex first

penguins %>%
  filter(!is.na(sex)) %>%
  ggplot(aes(x = sex, y = body_mass_g)) +
  geom_boxplot() +
  facet_wrap(~species)

# scatterplot with ratio of bill length to bill depth on the y axis
# and flipper length on the x axis
# distinguish between male and female penguins
# remove penguins with unknown sex before making the plot

penguins %>%
  mutate(ratio = bill_length_mm / bill_depth_mm) %>%
  filter(!is.na(sex)) %>%
  ggplot(aes(x = flipper_length_mm, y = ratio, color = sex)) +
  geom_point() +
  scale_color_manual(values = c("cyan4", "darkorange")) +
  labs(
    x = "Flipper lenght (mm)",
    y = "Ratio bill length / bill depth (-)"
  ) +
  theme_minimal()


# Adding a new column from another tibble:
another_tibble <- tibble(
  some_var = rnorm(n = nrow(penguins), mean = 4, sd = 3)
)

mutate(penguins,
       col_from_other_tibble = another_tibble$some_var)


# 3. Tidyr ----------------------------------------------------------------


# 1. relig_income

relig_income

pivot_longer(relig_income,
             cols = !religion, #2:11, `<$10k`:`>150k`
             names_to = "income",
             values_to = "count")


# 2. billboard

billboard

pivot_longer(billboard,
             cols = wk1:wk76,
             names_to = "week",
             values_to = "rank")

#### Extras

charts <- pivot_longer(billboard,
             cols = wk1:wk76,
             names_to = "week",
             names_prefix = "wk",
             values_to = "rank",
             values_drop_na = TRUE)

separate(
  charts,
  date.entered, # column to separate
  sep = "-",
  into = c("year","month", "day")
)

# 3. fish_encounters

fish_encounters

summary(fish_encounters)

pivot_wider(fish_encounters,
            names_from = station,
            values_from = seen)
#### Extra

pivot_wider(fish_encounters,
            names_from = station,
            values_from = seen,
            values_fill = 0)

# Transpose a tibble with a combination of pivot_longer and pivot_wider

relig_income %>%
  pivot_longer(-religion) %>%
  pivot_wider(names_from = religion, values_from = value)


# Joining tibbles ---------------------------------------------------------

# Join rows (tibbles of different length) and add a category

tibbleA <- tibble(category = rep(c("a", "b", "c"), each = 2), values = rnorm(6))
tibbleB <- tibble(category = c("a", "b", "c"), values = rnorm(3))

bind_rows(A = tibbleA,
          B = tibbleB,
          .id = "class")

# Join columns

tibbleA <- tibble(category = rep(c("a", "b", "c"), each = 2), values = rnorm(6))
tibbleB <- tibble(category = c("a", "b", "c"), price = rnorm(3))

left_join(tibbleA, tibbleB, by = "category")
