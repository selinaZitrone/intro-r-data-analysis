# Data set 2: Paralympic games from 1980-2016
# Data from International Paralympic Committee provided by tidytuesday
# https://github.com/rfordatascience/tidytuesday/edit/master/data/2021/2021-08-03/readme.md

library(tidyverse)

# Data preparation --------------------------------------------------------
olympics <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-27/olympics.csv")

# Filter only the medal winners to simplify the data
olympics_medal <- filter(olympics, !is.na(medal))

# Analyse the age distribution of athletes --------------------------------

# Medal count by age
olympics_medal |>
  ggplot(aes(x = age)) +
  geom_histogram()

# Frequency plot instead of count for age distribution
olympics_medal |>
  ggplot(aes(x = age, y = ..density..)) +
  geom_histogram()

# distinguished by sex and medal
olympics_medal |>
  ggplot(aes(x = age)) +
  geom_histogram() +
  facet_grid(sex ~ medal, scales = "free_y")

# make the plot nice
olympics_medal |>
  mutate(medal = factor(medal, levels = c("Gold","Silver", "Bronze"))) |>
  ggplot(aes(x = age, fill = medal)) +
  geom_histogram() +
  facet_grid(sex ~ medal, scales = "free_y") +
  scale_fill_manual(values = c("gold", "#BCBCBC", "#E2A863")) +
  theme_minimal() +
  theme(legend.position = "none")

# Looks like age distribution is the same for the medals
# Make the same plot but this time layer the medals on top of each other
olympics_medal |>
  mutate(medal = factor(medal, levels = c("Gold","Silver", "Bronze"))) |>
  ggplot(aes(x = age, fill = medal)) +
  geom_histogram(alpha = 0.5, position = "identity") +
  facet_wrap(~sex, scales = "free_y") +
  scale_fill_manual(values = c("gold", "#BCBCBC", "#E2A863")) +
  theme_minimal() +
  theme(legend.position = "none")

# Look at the count of medals and compare male and female athletes
# Looks like male athletes win much more medals than female athletes
olympics_medal |>
  mutate(medal = factor(medal, levels = c("Gold","Silver", "Bronze"))) |>
  ggplot(aes(x = age, fill = sex)) +
  geom_histogram(alpha = 0.5, position = "identity") +
  facet_wrap(~medal, scales = "free_y") +
  scale_fill_manual(values = c("darkorange","cyan4")) +
  theme_minimal()

# Compare medals for male and female athletes -----------------------------

# Male athletes win much more medals than female athletes
olympics |>
  summarize(
    medals = n(), .by = sex
  )

# Did this ratio improve over the years?
olympics_medal |>
  summarize(
    medals = n(), .by = c(sex, year, season)
  ) |>
  ggplot(aes(x = year, y = medals, color = sex)) +
  geom_line() +
  facet_wrap(~season)

# Plot the actual ratio (should be 1 in a perfect world)
olympics_medal |>
  count(sex,year) |>
  pivot_wider(names_from = sex, values_from = n) |>
  mutate(ratio_m_f = F / M) |>
  ggplot(aes(x = year, y = ratio_m_f)) +
  geom_point() +
  geom_line()


# What are the teams with the most medals? --------------------------------
# simple plot of states with more than 1000 medals
olympics_medal |>
  summarize(
    count = n(), .by = team
  ) |>
  arrange(
    desc(count)
  ) |>
  filter(count > 1000) |>
  mutate(team = as.factor(team)) |>
  mutate(team = fct_reorder(team, count)) |>
  ggplot(aes(x = team, y = count)) +
  geom_col() +
  coord_flip()

# Teams with the most medals evolution off ggplot style
# https://www.cedricscherer.com/2019/05/17/the-evolution-of-a-ggplot-ep.-1/

# First count medals by team and filter only the top 10 teams
medal_sums_total <-
  olympics_medal |>
  count(team) |>
  arrange(desc(n)) |>
  slice(1:10)

# Medal counts per year for the teams with the most medals
year_count <- olympics_medal |>
  filter(team %in% medal_sums_total$team) |>
  count(team, year)

# average medal count by year (for all teams)
avg <- mean(year_count$n)

# Plot medal count per country including it's mean and the overall mean
# of all countries
top_medals <- year_count |>
  mutate(team = fct_reorder(team, -n)) |>
  ggplot(aes(x = n, y = team, color = team)) +
  geom_vline(xintercept = avg, color = "gray70") +
  geom_point(
    position = position_jitter(
      seed = 123,
      height = 0.2
    ),
    alpha = 0.25
  ) +
  stat_summary(size = 0.9, linewidth = 1) +
  ggthemes::scale_color_tableau() +
  labs(x = "Yearly medal count") +
  theme_light(base_size = 18) +
  theme(
    legend.position = "none",
    axis.title.y = element_blank(),
    axis.title = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    plot.caption = element_text(size = 9, color = "gray50"),
    panel.grid = element_blank()
  )

ggsave("R/byod/img/top_medals.png", top_medals, width = 12, height = 8)

# Compare medal counts in winter and summer -------------------------------

# There seem to be more and more medals both in winter and in summer
olympics_medal |>
  summarize(
    medals = n(), .by = c(year, season, medal)
  ) |>
  ggplot(aes(x = year, y = medals, color = medal)) +
  geom_line() +
  facet_wrap(~season)

# Look at the sports type with the most medals ----------------------------

# sport with the most medals
olympics_medal |>
  group_by(sport) |>
  summarize(
    medals = n()
  ) |>
  arrange(desc(medals))

# Which athletes have competed in the most Olympic Games? ------------------

top_participants <- olympics |>
  group_by(name, id) |>
  summarise(games_count = n_distinct(games)) |>
  arrange(desc(games_count)) |>
  head(20)

ggplot(top_participants, aes(x = reorder(name, games_count), y = games_count)) +
  geom_col() +
  coord_flip() +
  labs(title = "Top 20 Athletes by Number of Olympic Games Participated",
       x = "Athlete Name", y = "Number of Olympic Games")
