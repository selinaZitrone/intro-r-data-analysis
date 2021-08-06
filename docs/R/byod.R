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
athletes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-03/athletes.csv')



# Ecodata package ---------------------------------------------------------

#https://theoreticalecology.github.io/ecodata/

EcoData::birdabundance
EcoData::pol
#The idea is pollinators interact with plants when their traits fit (e.g. the tongue of a bee needs to match the shape of a flower). "
?EcoData::plantPollinator_df
?EcoData::plantPollinators
?EcoData::melanoma
# Überlebt dr. eher?
# https://rpubs.com/shivam2503/predictsurvival
str(EcoData::titanic) # Could be used to explore GLMS with material from slides

data = EcoData::titanic

## step 1: data exploration and cleaning----
str(data)
summary(data)
head(data)
# let's explore the names of the passengers. they contain more info: titles.
# so we extract that info from the name and use it as its own variable
# first split after the comma, then after the dot
first_split = sapply(data$name, function(x) stringr::str_split(x, pattern = ",")[[1]][2])
titles = sapply(first_split, function(x) strsplit(x, ".",fixed = TRUE)[[1]][1])
table(titles)
titles = stringr::str_trim((titles)) # this step is important. it gets rid of the spaces
titles %>%
  fct_count()
# now there'S 18 titles. some of them appear only a few times. we can group them!
titles2 =
  forcats::fct_collapse(titles,
                        officer = c("Capt", "Col", "Major", "Dr", "Rev"),
                        royal = c("Jonkheer", "Don", "Sir", "the Countess", "Dona", "Lady"),
                        miss = c("Miss", "Mlle"),
                        mrs = c("Mrs", "Mme", "Ms")
  )
# and add them to our dataset
data =
  data %>%
  mutate(title = titles2)
summary(data)

EcoData::wine # what does it need to make a good wine? https://www.kaggle.com/uciml/red-wine-quality-cortez-et-al-2009
# https://pierremary.com/datascience/r/2018/05/15/data-analysis-wine-datasets-with-r.html
# https://github.com/sagarnildass/Red-Wine-Data-Analysis-by-R/blob/master/redWineAnalysis.md

write_csv(birds, file = "./data/birdabundance.csv")
write_csv(wine, file = "./data/wine.csv")



# arranging plots ---------------------------------------------------------
library(cowplot)
library(grid)
library(gridExtra)
library(tidyverse)
# dummy data
tbl <- tibble(a = 1:10, b = 1:10)
# dummy plot
aplot <- ggplot(tbl, aes(a, b)) +
  geom_point() +
  theme(
    plot.background = element_rect(fill = "lightblue"),
    plot.margin = margin(0, 0, 0, 0, "cm"),
    legend.title = element_blank(),
    axis.title = element_blank()
  ) +
  labs(title = "This is some plot title")

# common axis labels as textGrob
y.grob <- textGrob("Common Y",
  gp = gpar(fontface = "bold", col = "blue", fontsize = 15), rot = 90
)

x.grob <- textGrob("Common X",
  gp = gpar(fontface = "bold", col = "blue", fontsize = 15)
)

# Creating a 2x2 grid with 4 times the aplot
dplot <- plot_grid(plotlist = list(aplot, aplot, aplot, aplot), nrow = 2)

# Arranging the dplot with the two test grobs
eplot <- grid.arrange(arrangeGrob(dplot, left = y.grob, bottom = x.grob))

# save the plot as png
 ggsave(filename = "./img/cowplot_grob2.png", eplot)



# Franz --------------------------------------------------------------------
 tbl <- tibble(value = rep(c(1,NA), each = 5), id = rep(c("4", "12"), each = 5))

 tbl[tbl$id == "12", ]$value <- unique(tbl[tbl$id == "4",]$value)

 # oder dplyr mäßig
 height_4 <- unique(tbl[tbl$id == "4",]$value)
 mutate(tbl, HEIGHT = ifelse(id == "4", height_4, ))

 # oder anders

 height_4 <- unique(tbl[tbl$id == "4",]$value)


