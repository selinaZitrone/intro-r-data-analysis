# Code for slides from day 3
# Author: Selina Baldauf (selina.baldauf@fu-berlin.de)
# Hint: navigate the script using the document outline (next to Source button)

# Data visualization with ggplot2 -----------------------------------------
library(ggplot2)

# data
str(msleep)
?msleep

# basic scatter plot
ggplot(data = msleep,
       aes(x = brainwt,
           y = sleep_total)) +
  geom_point()

# add log 10 x-axis
ggplot(data = msleep,
       aes(x = brainwt,
           y = sleep_total)) +
  geom_point() +
  scale_x_log10()

# add linear regression line
ggplot(data = msleep,
       aes(x = brainwt,
           y = sleep_total)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_x_log10()

# Change color, shape and size of ALL points
ggplot(data = msleep,
       aes(x = brainwt,
           y = sleep_total)) +
  geom_point(size = 4,
             shape = 17,
             color = "blue") +
  scale_x_log10()

# map color as an aesthetic
g <- ggplot(
  data = msleep,
  aes(
    x = brainwt,
    y = sleep_total,
    color = vore
  )
) +
  geom_point(size = 4) +
  scale_x_log10()
g

# map size as an aesthetic
ggplot(
  data = msleep,
  aes(
    x = brainwt,
    y = sleep_total,
    color = vore,
    size = bodywt
  )
) +
  geom_point() +
  scale_x_log10()

# map shape as an aesthetic
ggplot(
  data = msleep,
  aes(
    x = brainwt,
    y = sleep_total,
    color = vore,
    shape = conservation
  )
) +
  geom_point() +
  scale_x_log10()


# Changing colors ---------------------------------------------------------

g +
  scale_color_viridis_d(option = "inferno", na.value = "grey")

g <- g +
  scale_color_manual(
    values = c("dodgerblue4",
               "darkolivegreen4",
               "darkorchid3",
               "orange",
               "grey"))
g

g + ggsci::scale_color_npg()
g + ggsci::scale_color_rickandmorty()
g + ggthemes::scale_color_excel_new()
g + ggthemes::scale_color_economist()


# facets ------------------------------------------------------------------

g + facet_wrap(~conservation)


# labs --------------------------------------------------------------------

g <- g +
  labs(
    x = "log brain weight [kg]",#<<
    y = "total sleep [h/day]", #<<
    color = "Diet", #<<
    title = "Brain weight and sleep time in mammals",#<<
    subtitle = "Larger brains seem to sleep less",#<<
    caption = "Data from the ggplot2 package")#<<
g


# themes ------------------------------------------------------------------

g + theme_classic()
g + theme_linedraw()
g + theme_dark()
g + theme_minimal()

g +
  theme_minimal() +
  theme(
    axis.text = element_text(face = "bold"),
    axis.title = element_text(face = "italic"),
    legend.position = "bottom",
    plot.background = element_rect(
      fill = "lightblue",
      color = "yellow")
  )

# set a global theme for all ggplots in the session
theme_set(theme_minimal(base_size = 16))


# boxplots ----------------------------------------------------------------

ggplot(msleep, aes(x = vore, y = bodywt)) +
  geom_boxplot() +
  scale_y_log10()

ggplot(msleep, aes(x = vore, y = bodywt)) +
  geom_boxplot(
    aes(fill = vore)
  ) +
  scale_y_log10() +
  scale_fill_brewer(
    palette = "Set1",
    na.value = "gray",
    guide = "none")

ggplot(msleep, aes(x = vore, y = bodywt)) +
  geom_boxplot(
    aes(color = vore)
  ) +
  scale_y_log10() +
  scale_color_brewer(
    palette = "Set1",
    na.value = "gray",
    guide = "none")


# Histogram ---------------------------------------------------------------

ggplot(msleep, aes(x=sleep_total)) +
  geom_histogram() + #<<
  labs(x = "Total sleep time [h]",
       y = "Frequency")


# saving plots ------------------------------------------------------------
# save plot g in img as my_plot.pdf
#ggsave(filename = "./img/my_plot.pdf", plot = g)
# save plot g in img as my_plot.png
#ggsave(filename = "./img/my_plot.png", plot = g)
# ggsave(filename = "./img/my_plot.png",
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
