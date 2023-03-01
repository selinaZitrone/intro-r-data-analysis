library(tidyverse)
olympics <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-27/olympics.csv")

# look only at the medal winners
olympics_medal <- filter(olympics, !is.na(medal))

# Age distribution --------------------------------------------------------

# count
olympics_medal %>%
  ggplot(aes(x=age)) +
  geom_histogram()

# frequency
olympics_medal %>%
  ggplot(aes(x=age, y = ..density..)) +
  geom_histogram()

# distinguished by sex and medal
olympics_medal %>%
  ggplot(aes(x = age)) +
  geom_histogram() +
  facet_grid(sex ~ medal, scales = "free_y")

# make the plot nice
olympics_medal %>%
  mutate(medal = factor(medal, levels = c("Gold","Silver", "Bronze"))) %>%
  ggplot(aes(x = age, fill = medal)) +
  geom_histogram() +
  facet_grid(sex ~ medal, scales = "free_y") +
  scale_fill_manual(values = c("gold", "#BCBCBC", "#E2A863")) +
  theme_minimal() +
  theme(legend.position = "none")

# Looks like age distribution is the same for the medals
olympics_medal %>%
  mutate(medal = factor(medal, levels = c("Gold","Silver", "Bronze"))) %>%
  ggplot(aes(x = age, fill = medal)) +
  geom_histogram(alpha = 0.5, position = "identity") +
  facet_wrap(~sex, scales = "free_y") +
  scale_fill_manual(values = c("gold", "#BCBCBC", "#E2A863")) +
  theme_minimal() +
  theme(legend.position = "none")

olympics_medal %>%
  mutate(medal = factor(medal, levels = c("Gold","Silver", "Bronze"))) %>%
  ggplot(aes(x = age, fill = sex)) +
  geom_histogram(alpha = 0.5, position = "identity") +
  facet_wrap(~medal, scales = "free_y") +
  scale_fill_manual(values = c("darkorange","cyan4")) +
  theme_minimal() +
  theme(legend.position = "none")

# There seem to be more and more medals both in winter and in summer
olympics_medal %>%
  group_by(year, season, medal) %>%
  summarize(
    medals = n()
  ) %>%
  ggplot(aes(x=year, y=medals, color = medal))+
  geom_line() +
  facet_wrap(~season)

# Do male or female win more medals
olympics %>%
  group_by(sex) %>%
  summarize(
    medals = n()
  )

# Did this ratio improve over the years?
olympics_medal %>%
  group_by(sex, year,season) %>%
  summarize(
    medals = n()
  ) %>%
  ggplot(aes(x= year, y= medals, color = sex)) +
  geom_line() +
  facet_wrap(~season)

# yeah it improved
olympics_medal %>%
  group_by(sex, year, season) %>%
  summarize(
    medals = n()
  ) %>%
  pivot_wider(names_from = sex, values_from = medals) %>%
  mutate(ratio_m_f = F/M) %>%
  ggplot(aes(x=year, y=ratio_m_f)) +
  geom_point() +
  geom_line() +
  facet_wrap(~season)


# Teams with the most medals
olympics_medal %>%
  group_by(team) %>%
  summarize(
    count = n()
  ) %>%
  arrange(
    desc(count)
  ) %>%
  filter(count > 1000) %>%
  mutate(team = as.factor(team)) %>%
  mutate(team = fct_reorder(team, count)) %>%
  ggplot(aes(x = team, y = count)) +
  geom_col() +
  coord_flip()

# Teams with the most medals evolution off ggplot style
# https://www.cedricscherer.com/2019/05/17/the-evolution-of-a-ggplot-ep.-1/
medal_sums_total <-
  olympics_medal %>%
  group_by(team) %>%
  count() %>%
  arrange(desc(n)) %>%
  filter(n>1000)

year_count <- olympics_medal %>%
  filter(team %in% medal_sums_total$team) %>%
  group_by(team,year) %>%
  count()

# average medal count by year
avg <- mean(year_count$n)

year_count %>%
  mutate(team = fct_reorder(team, -n)) %>%
  ggplot(aes(x = n, y = team, color = team)) +
  geom_vline(xintercept = avg, color = "gray70") +
  geom_point(
    position = position_jitter(
      seed = 123,
      width = 0.2
    ),
    alpha = 0.25
  ) +
  geom_segment(
    aes(x = avg, xend = n, y = team, yend = team)
  )+
  stat_summary(fun = mean, geom = "point", size = 5) +
  ggsci::scale_color_uchicago() +
  labs(x = "Yearly medal count") +
  theme_light(base_size = 18, base_family = "Poppins") +
  theme(
    legend.position = "none",
    axis.title = element_text(size = 12),
    axis.text.x = element_text(family = "Roboto Mono", size = 10),
    plot.caption = element_text(size = 9, color = "gray50"),
    panel.grid = element_blank()
  )



# sport with the most medals
olympics_medal %>%
  group_by(sport) %>%
  summarize(
    medals = n()
  ) %>%
  arrange(medals)


# Heatmaps -----------------------------------------------------------------
# heat map showing the number of medals by medal type and sport/country/gender...
olympics_medal %>%
  group_by(medal, sex) %>%
  count() %>%
  ggplot(aes(x=medal, y=sex, fill = n))+
  geom_tile() +
  scale_fill_gradient(low = "#00AFBB", high = "#E7B800")

olympics_medal %>%
  group_by(medal, noc) %>%
  count() %>%
  arrange(desc(n)) %>%
  filter(n>)
  ggplot(aes(x=medal, y=sex, fill = n))+
  geom_tile() +
  scale_fill_gradient(low = "#00AFBB", high = "#E7B800")

