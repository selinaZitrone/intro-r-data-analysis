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

soybean_use <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-06/soybean_use.csv')


# filter ------------------------------------------------------------------

filter(soybean_use, entity == "Germany")
countries_select <- c("Germany", "Austria", "Switzerland")
filter(soybean_use, entity %in% countries_select)
filter(soybean_use, is.na(code))
filter(soybean_use, !is.na(code))
filter(soybean_use, between(year, 1970, 1980) & entity == "Germany")


# select ------------------------------------------------------------------

select(soybean_use, entity, year, human_food)
select(soybean_use, -entity, -year, -human_food)
select(soybean_use, ends_with("d"))

# this does not match any rows in the soy bean data set
# but combinations like this are helpful for research data
select(soybean_use, starts_with("sample_"))
select(soybean_use, contains("_id_"))

cols <- c("sample_", "year", "processed", "entity")
select(soybean_use, any_of(cols))
# error because sample_ does not exist
select(soybean_use, all_of(cols))

select(soybean_use, 1:3)
select(soybean_use, code:animal_feed)

# arrange -----------------------------------------------------------------

arrange(soybean_use, processed)
arrange(soybean_use, desc(processed))
arrange(soybean_use, year, entity)

# mutate ------------------------------------------------------------------

mutate(soybean_use, sum_human_animal = human_food + animal_feed)
mutate(soybean_use,
       sum_human_animal = human_food + animal_feed,
       total = human_food + animal_feed + processed
)
mutate(soybean_use,
       legislation = case_when(
         year < 2000 & year >= 1980 ~ "legislation_1",
         year >= 2000 ~ "legislation_2",
         TRUE ~ "no_legislation"
       )
)
transmute(soybean_use,
          ratio_processed_animal = processed/animal_feed,
          ratio_human_animal = human_food/animal_feed)

# summarize ---------------------------------------------------------------

summarize(soybean_use,
          total_animal = sum(animal_feed, na.rm = TRUE),
          total_human = sum(human_food, na.rm = TRUE))
soybean_use_group <- group_by(soybean_use, year)
summarize(soybean_use_group,
          total_animal = sum(animal_feed, na.rm = TRUE),
          total_human = sum(human_food, na.rm = TRUE))

# count -------------------------------------------------------------------

# count rows grouped by year
count(soybean_use, year)

# or if the data is already grouped by year
count(soybean_use_group)


# The pipe operator  ------------------------------------------------------

# OPTION 1: step by step

# 1: filter rows that actually represent a country
soybean_new <- filter(soybean_use, !is.na(code))

# 2: group the data by year
soybean_new <- group_by(soybean_new, year)

# 3: summarize mean values by year
soybean_new <- summarize(soybean_new,
  mean_processed = mean(processed, na.rm = TRUE),
  sd_processed = sd(processed, na.rm = TRUE)
)

# 4: reorder the observation with newest first
soybean_new <- arrange(soybean_new, desc(year))

# OPTION 2: nested function

soybean_new <- arrange(
  summarize(
    group_by(
      filter(soybean_use, !is.na(code)),
      year
    ),
    mean_processed = mean(processed, na.rm = TRUE),
    sd_processed = sd(processed, na.rm = TRUE)
  ),
  desc(year)
)

# OPTION 3: pipe %>%

soybean_new <- soybean_use %>%
  filter(!is.na(code)) %>%
  group_by(year) %>%
  summarize(
    mean_processed = mean(processed, na.rm = TRUE),
    sd_processed = sd(processed, na.rm = TRUE)
  ) %>%
  arrange(desc(year))

# pipe with ggplot2

soybean_use %>%
  filter(!is.na(code)) %>%
  select(year, processed) %>%
  group_by(year) %>%
  summarize(
    processed = sum(processed, na.rm = TRUE)
  ) %>%
  ggplot(aes(
    x = year,
    y = processed
  )) +
  geom_line()


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
