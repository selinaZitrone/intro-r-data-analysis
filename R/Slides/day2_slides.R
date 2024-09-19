# Code for slides from day 3
# Author: Selina Baldauf (selina.baldauf@fu-berlin.de)
# Hint: navigate the script using the document outline (next to Source button)

# Data visualization with ggplot2 -----------------------------------------
library(ggplot2)
library(lterdatasampler)

# Remove one species with only few datapoints
and_vertebrates <- and_vertebrates %>%
  select(year, section, unittype, species, length_1_mm, weight_g) %>%
  filter(species != "Cascade torrent salamander")

# data
str(and_vertebrates)
?and_vertebrates


# Exploratory -------------------------------------------------------------

# Scatter plot ------------------------------------------------------------

# basic scatter plot
ggplot(data = and_vertebrates,
       aes(x = length_1_mm,
           y = weight_g)) +
  geom_point()

# add color aesthetic
ggplot(data = and_vertebrates,
       aes(x = length_1_mm,
           y = weight_g,
           color = species)) +
  geom_point()

# add size aesthetic
ggplot(data = and_vertebrates,
       aes(x = length_1_mm,
           y = weight_g,
           size = species)) +
  geom_point()

# add shape aesthetic
ggplot(data = and_vertebrates,
       aes(x = length_1_mm,
           y = weight_g,
           shape = species)) +
  geom_point()

# combine color, size and shape aesthetic
ggplot(data = and_vertebrates,
       aes(x = length_1_mm,
           y = weight_g,
           color = unittype,
           shape = species,
           size = year)) +
  geom_point()

# add log 10 x- and y-axis
ggplot(data = and_vertebrates,
       aes(x = length_1_mm,
           y = weight_g,
           color = species)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10()

# add linear regression line
ggplot(data = and_vertebrates,
       aes(x = length_1_mm,
           y = weight_g,
           color = species)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm") +
  scale_x_log10() +
  scale_y_log10()


# Boxplot -----------------------------------------------------------------

# basic boxplot
ggplot(and_vertebrates,
       aes(x = species,
           y = length_1_mm)) +
  geom_boxplot()

# boxplot with notches
ggplot(and_vertebrates,
       aes(x = species,
           y = length_1_mm)) +
  geom_boxplot(notch = TRUE)

# color aesthetic
ggplot(and_vertebrates,
       aes(x = species,
           y = length_1_mm,
           color = unittype)) +
  geom_boxplot()

# fill aesthetic
ggplot(and_vertebrates,
       aes(x = species,
           y = length_1_mm,
           fill = unittype)) +
  geom_boxplot(notch = TRUE)


# Histograms --------------------------------------------------------------

# stacked
ggplot(and_vertebrates,
       aes(x = length_1_mm,
           fill = section)) +
  geom_histogram()

# Overlapping
ggplot(and_vertebrates,
       aes(x = length_1_mm,
           fill = section)) +
  geom_histogram(
    position = "identity",
    alpha = 0.5)


# Heat map ----------------------------------------------------------------
ggplot(and_vertebrates,
       aes(x = section,
           y = species,
           fill = weight_g)) +
  geom_tile()


# Small multiples ---------------------------------------------------------

# facet wrap basic
ggplot(data = and_vertebrates,
       aes(x = length_1_mm,
           y = weight_g,
           color = species)) +
  geom_point() +
  facet_wrap(~section)

# facet wrap with options
ggplot(data = and_vertebrates,
       aes(x = length_1_mm,
           y = weight_g,
           color = species)) +
  geom_point() +
  facet_wrap(~section,
             nrow = 2,
             scales = "free")

# facet grid
ggplot(data = and_vertebrates,
       aes(x = length_1_mm,
           y = weight_g,
           color = unittype)) +
  geom_point() +
  facet_grid(section ~ species)


# Data master piece -------------------------------------------------------

# Change color, shape and size of ALL points
ggplot(and_vertebrates, aes(
  x = length_1_mm,
  y = weight_g
)) +
  geom_point(
    size = 4, #<<
    shape = 17, #<<
    color = "blue", #<<
    alpha = 0.5 #<<
  )

# map color as an aesthetic
g <- ggplot(and_vertebrates, aes(
  x = length_1_mm,
  y = weight_g,
  color = species
)) +
  geom_point()

# map size as an aesthetic
g +
  scale_color_viridis_d()
g +
  scale_color_manual(values = c(
    "darkolivegreen4",
                     "darkorchid3"
  ))

library(paletteer)
g <- g +
  scale_color_paletteer_d(
    palette = "ggsci::default_uchicago"
  )
g

# Fill versus color
ggplot(
  and_vertebrates,
  aes(
    x = section,
    y = length_1_mm,
    color = unittype )) +#<<
  geom_boxplot() +
  paletteer::scale_color_paletteer_d( #<<
    palette = "ggsci::default_uchicago"
  )

ggplot(
  and_vertebrates,
  aes(
    x = section,
    y = length_1_mm,
    fill = unittype )) + #<<
  geom_boxplot() +
  paletteer::scale_fill_paletteer_d( #<<
    palette = "ggsci::default_uchicago"
  )


# Labels ------------------------------------------------------------------
g <- g +
  labs(
    x = "Length [mm]", #<<
    y = "Weight [g]", #<<
    color = "Species", #<<
    title = "Length-Weight relationship", #<<
    subtitle = "There seems to be an exponential relationship", #<<
    caption = "Data from the `lterdatasampler` package") #<<
g


# Themes ------------------------------------------------------------------
g +
  theme_classic()
g +
  theme_bw()
g +
  theme_minimal()
g +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text = element_text(face = "bold"),
    plot.background = element_rect(
      fill = "lightgrey",
      color = "darkgrey"
    )
  )

# set a global theme for all ggplots in the session
theme_set(theme_minimal(base_size = 16))

# Save plot ---------------------------------------------------------------
# saving plots ------------------------------------------------------------
# save plot g in img as my_plot.pdf
#ggsave(filename = "img/my_plot.pdf", plot = g)
# save plot g in img as my_plot.png
#ggsave(filename = "img/my_plot.png", plot = g)
# ggsave(filename = "img/my_plot.png",
#        plot = g,
#        width = 16,
#        heigth = 9,
#        units = "cm")


# transformation dplyr -----------------------------------------------------
library(dplyr)

and_vertebrates

# filter ------------------------------------------------------------------

filter(and_vertebrates, species == "Cutthroat trout")
filter(and_vertebrates, species == "Cutthroat trout" & year == 1987)
unittype_select <- c("R", "C", "S")
filter(and_vertebrates, unittype %in% unittype_select)
filter(and_vertebrates, is.na(weight_g))
filter(and_vertebrates, !is.na(weight_g))
filter(and_vertebrates, between(year, 2000, 2005))

# select ------------------------------------------------------------------

select(soybean_use, ends_with("d"))

select(and_vertebrates, species, length_1_mm, year)
select(and_vertebrates, -species, -length_1_mm, -year)
select(and_vertebrates, starts_with("s"))

# this does not make sense for our data specifically
# but combinations like this are helpful for research data
select(and_vertebrates, starts_with("sample_"))
select(and_vertebrates, contains("_id_"))

select(soybean_use, 1:3)
select(soybean_use, code:animal_feed)

select(and_vertebrates, 1:3)
select(and_vertebrates, year:unittype)

# arrange -----------------------------------------------------------------

arrange(and_vertebrates, length_1_mm)
arrange(and_vertebrates, desc(length_1_mm))

# mutate ------------------------------------------------------------------

mutate(and_vertebrates, weight_kg = weight_g/1000)
mutate(and_vertebrates,
       weight_kg = weight_g/1000,
       length_m = length_1_mm/1000)

mutate(and_vertebrates,
       type = case_when(
         species == "Cutthroat trout" ~ "Fish",
         species == "Coastal giant salamander" ~ "Amphibian",
         .default = NA # all other cases
))

# summarize ---------------------------------------------------------------

summarize(soybean_use,
          total_animal = sum(animal_feed, na.rm = TRUE),
          total_human = sum(human_food, na.rm = TRUE))
soybean_use_group <- group_by(soybean_use, year)
summarize(soybean_use_group,
          total_animal = sum(animal_feed, na.rm = TRUE),
          total_human = sum(human_food, na.rm = TRUE))

summarize(and_vertebrates,
          mean_length = mean(length_1_mm, na.rm = TRUE),
          mean_weight = mean(weight_g, na.rm = TRUE))
# by species
summarize(and_vertebrates,
    mean_length = mean(length_1_mm, na.rm = TRUE),
    mean_weight = mean(weight_g, na.rm = TRUE),
    .by = species
  )

# count -------------------------------------------------------------------

# count rows (observations) grouped by year
count(and_vertebrates, year)

# The pipe operator  ------------------------------------------------------

# OPTION 1: step by step

# 1: filter rows that have don't have NA in the unittype column
and_vertebrates_new <- filter(and_vertebrates, !is.na(unittype))

# 3: summarize mean values by year
and_vertebrates_new <- count(and_vertebrates, year, species, section)

# OPTION 2: nested function

and_vertebrates_new <- count(
  filter(and_vertebrates, !is.na(unittype)),
  year, species, section
)

# OPTION 3: pipe |>
and_vertebrates_new <- and_vertebrates |>
  filter(!is.na(unittype)) |>
  count(year, species, section)

# pipe with ggplot2
and_vertebrates |>
  filter(!is.na(unittype)) |>
  count(year, species, section) |>
  ggplot(aes(x = year, y = n, color = species)) +
  geom_line() +
  facet_wrap(~section)

# tidyr -------------------------------------------------------------------
library(tidyr)

# list of 10 biggest cities in Europe
cities <- c("Istanbul", "Moscow", "London", "Saint Petersburg", "Berlin", "Madrid", "Kyiv", "Rome", "Bucharest", "Paris")
population <- c(15.1e6, 12.5e6, 9e6, 5.4e6, 3.8e6, 3.2e6, 3e6, 2.8e6, 2.2e6, 2.1e6)
area_km2 <- c(2576, 2561, 1572, 1439,891,604, 839, 1285, 228, 105 )
# tibble
cities_tbl <- tibble(
  city = cities,
  population = population,
  area_km2 = area_km2,
  country = c("Turkey", "Russia", "UK", "Russia", "Germany", "Spain", "Ukraine", "Italy", "Romania", "France")
)

# first create an untidy tibble from the tidy cities tbl
cities_untidy <- unite(cities_tbl, col = "location", c("country", "city")) %>%
  pivot_longer(c("population", "area_km2"), names_to = "type") %>%
  pivot_wider(names_from = "location", values_from = "value")

cities_untidy


# pivot_longer ------------------------------------------------------------

step1 <- pivot_longer(
  cities_untidy,                         # the tibble
  cols = Turkey_Istanbul:France_Paris,   # the columns to pivot from:to
  names_to = "location",                 # name of the new column
  values_to = "value")                   # name of the value column

# or
step1 <- pivot_longer(
  cities_untidy,           # the tibble
  cols = !type,            # All columns except type#<<
  names_to = "location",   # name of the new column
  values_to = "value")

step1

# seperate ----------------------------------------------------------------

step2 <- separate(
  step1,                       # the tibble
  location,                    # the column to separate
  sep = "_",                   # the separator
  into = c("country", "city")) # names of new columns

step2

# pivot_wider -------------------------------------------------------------

step3 <- pivot_wider(
  step2,                      # the tibble
  names_from = type,          # the variables
  values_from = value)        # the values

step3

# all in one --------------------------------------------------------------

cities_untidy %>%
  pivot_longer(Turkey_Istanbul:France_Paris, names_to = "location") %>%
  separate(location, sep = "_", into = c("country", "city")) %>%
  pivot_wider(names_from = type, values_from = value)
