# Kenya census ------------------------------------------------------------

gender <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-19/gender.csv')
crops <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-19/crops.csv')
households <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-19/households.csv')

# Combine data
crops <- rename(crops, County = SubCounty)

crops <- mutate(crops, County = tolower(County))
households <- mutate(households, County = tolower(County))
kenya <- merge(crops, households, by = "County")

arrange(kenya, desc(Farming))

filter(kenya, County != "kenya") %>%
ggplot(aes(x = Farming, y = AverageHouseholdSize))+
  geom_point() +
  geom_smooth(method = "lm")

filter(kenya, County != "kenya") %>%
  ggplot(aes(x = Population, y = NumberOfHouseholds))+
  geom_point() +
  geom_smooth(method = "lm")


# Plastic -----------------------------------------------------------------
plastics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-26/plastics.csv')

ggplot(plastics, aes(x=factor(year), y=grand_total))+
  geom_boxplot()

# Countries with most plastic
plastics %>%
  group_by(country) %>%
  summarize(
    sum_total = sum(grand_total)
) %>%
  arrange(desc(sum_total))

select(plastics, pet,pp,pvc,ps) %>%
  pivot_longer(pet:ps, names_to  = "type", values_to = "amount") %>%
  filter(amount!=0) %>%
  ggplot(aes(x=type, y=amount))+
  geom_boxplot()+
  scale_y_log10()


# Droughts ----------------------------------------------------------------
drought <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-20/drought.csv')


rainfall <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-07/rainfall.csv')
temperature <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-07/temperature.csv')

# IF YOU USE THIS DATA PLEASE BE CAUTIOUS WITH INTERPRETATION
nasa_fire <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-07/MODIS_C6_Australia_and_New_Zealand_7d.csv')

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


# Other idea:
# more penguinplots
# Cédrics nice plot -> try to reproduce



# Ecodata package ---------------------------------------------------------

#https://theoreticalecology.github.io/ecodata/

EcoData::birdabundance
?EcoData::melanoma
str(EcoData::titanic) # Could be used to explore GLMS with material from slides
