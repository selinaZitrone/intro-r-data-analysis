olympics <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-27/olympics.csv")
olympics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-27/olympics.csv')

olympics %>%
  group_by(age) %>%
  summarize(
    medals = n()
  ) %>%
  ggplot(aes(x=age, y=medals))+
  geom_line()

olympics %>%
  group_by(year) %>%
  summarize(
    medals = n()
  ) %>%
  ggplot(aes(x=year, y=medals))+
  geom_line()

# Do male or female win more medals
olympics %>%
  group_by(sex) %>%
  summarize(
    medals = n()
  )

# Did this ratio improve over the years?
olympics %>%
  group_by(sex, year) %>%
  summarize(
    medals = n()
  ) %>%
  ggplot(aes(x= year, y= medals, color = sex)) +
  geom_line()

# yeah it improved
olympics %>%
  group_by(sex, year) %>%
  summarize(
    medals = n()
  ) %>%
  pivot_wider(names_from = sex, values_from = medals) %>%
  mutate(ratio_m_f = F/M) %>%
  ggplot(aes(x=year, y=ratio_m_f)) +
  geom_point() +
  geom_line()


# number of different sports
unique(olympics$sport)
# Sport with at leas 10 medals
olympics %>%
  group_by(sport) %>%
  summarise(
    count = n()
  ) %>%
  ungroup() %>%
  summarise(
    mean = mean(count)
  )


# Teams with the most medals
olympics %>%
  group_by(team) %>%
  summarize(
    count = n()
  ) %>%
  arrange(
    desc(count)
  ) %>%
  filter(count > 3000) %>%
  mutate(team = as.factor(team)) %>%
  mutate(team = fct_reorder(team, count)) %>%
  ggplot(aes(x=team, y=count))+
  geom_col() +
  coord_flip()

# Teams with the most medals
olympics %>%
  group_by(team, sex) %>%
  summarize(
    count = n()
  ) %>%
  arrange(
    desc(count)
  ) %>%
  filter(count > 1000) %>%
  mutate(team = as.factor(team)) %>%
  mutate(team = fct_reorder(team, count)) %>%
  ggplot(aes(x=team, y=count))+
  geom_col() +
  coord_flip()+
  facet_wrap(~sex)


# sport with the most medals
olympics %>%
  group_by(sport) %>%
  summarize(
    medals = n()
  ) %>%
  arrange(medals)

# What was the Aeoronautics
olympics %>%
  filter(sport == "Aeronautics")

olympics %>%
  group_by(age) %>%
  summarize(
    medals = n()
  ) %>%
  ggplot(aes(x = age, y = medals)) +
  geom_line()

olympics %>%
  group_by(year) %>%
  summarize(
    medals = n()
  ) %>%
  ggplot(aes(x = year, y = medals)) +
  geom_line()

# Do male or female win more medals
olympics %>%
  group_by(sex) %>%
  summarize(
    medals = n()
  )

# Did this ratio improve over the years?
olympics %>%
  group_by(sex, year) %>%
  summarize(
    medals = n()
  ) %>%
  ggplot(aes(x = year, y = medals, color = sex)) +
  geom_line()

# yeah it improved
olympics %>%
  group_by(sex, year) %>%
  summarize(
    medals = n()
  ) %>%
  pivot_wider(names_from = sex, values_from = medals) %>%
  mutate(ratio_m_f = F / M) %>%
  ggplot(aes(x = year, y = ratio_m_f)) +
  geom_point() +
  geom_line()


# number of different sports
unique(olympics$sport)
# Sport with at leas 10 medals
olympics %>%
  group_by(sport) %>%
  summarise(
    count = n()
  ) %>%
  ungroup() %>%
  summarise(
    mean = mean(count)
  )


# Teams with the most medals
olympics %>%
  group_by(team) %>%
  summarize(
    count = n()
  ) %>%
  arrange(
    desc(count)
  ) %>%
  filter(count > 3000) %>%
  mutate(team = as.factor(team)) %>%
  mutate(team = fct_reorder(team, count)) %>%
  ggplot(aes(x = team, y = count)) +
  geom_col() +
  coord_flip()

# Teams with the most medals
olympics %>%
  group_by(team, sex) %>%
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
  coord_flip() +
  facet_wrap(~sex)


# sport with the most medals
olympics %>%
  group_by(sport) %>%
  summarize(
    medals = n()
  ) %>%
  arrange(medals)

# What was the Aeoronautics
olympics %>%
  filter(sport == "Aeronautics")
