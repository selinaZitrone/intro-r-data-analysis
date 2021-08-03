library(ggplot2)


# Exploratory data analysis

# Does brainweight influence the hours that a mammal need to sleep?

g <- ggplot(data = msleep)+
  geom_point(aes(x=brainwt, y=sleep_total))

# many mammals with small brains and very few with large brains -> log of x axis

g <- g + scale_x_log10()

# Are there differences depending on type of food source
# Does not seem that way
# We can also
g2 <- ggplot(data = msleep)+
  geom_point(aes(x=brainwt, y=sleep_total, color = vore)) +
  scale_x_log10()

# If we don't like the colors used for the color aesthetic, we can change it:
g2 + scale_color_ordinal(na.value = "grey")
g2 + ggsci::scale_color_rickandmorty()
g2 + ggsci::scale_colour_npg()
g2 <- g2 + scale_color_manual(values = c("dodgerblue4",
                                   "darkolivegreen4",
                                   "darkorchid3",
                                   "orange", "grey"))

# does this relationship depend on conservation status?
g2 +
  facet_grid(conservation~.)

# Can we add a title to this graph?
g2 +
  labs(x = "a", y="b", title = "t")

# change the appearance
g2<- g2 + theme_bw()+theme(axis.text = element_text(face = "bold"))



# Boxplots

plastics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-26/plastics.csv')

ggplot(plastics, aes(x=year, y=grand_total))+
  geom_boxplot()

# Oh no what happened here?
# Alison continous vs. categorical data

ggplot(plastics, aes(x=factor(year), y=grand_total))+
  geom_boxplot()+
  geom_violin()+
  scale_y_log10()






ggplot(data = msleep, aes(x=vore, y = sleep_rem, color = vore)) +
  geom_boxplot(outlier.alpha = 0, color = "gray") +
  geom_point(alpha = 0.8, size = 3, position = position_jitter(width = 0.1)) +
  scale_fill
  labs(y = "sleep rem (h)", x = "Eating behaviour",
       title = "Do carnivores sleep deeper than herbivores?")+
  theme_light()+
  theme(
    legend.position = "none"
)



# Barcharts

# Histograms


# test other data ---------------------------------------------------------
# This could be group exercises at the end of the day
# Each group with a different dataset

forest <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-06/forest.csv')
forest_area <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-06/forest_area.csv')
brazil_loss <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-06/brazil_loss.csv')
soybean_use <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-06/soybean_use.csv')
vegetable_oil <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-06/vegetable_oil.csv')


ggplot(brazil_loss, aes(x=year, y=commercial_crops))+
  geom_line()


# And still other data ----------------------------------------------------

plastics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-26/plastics.csv')

ggplot(plastics, aes(x=year, y=grand_total))+
  geom_boxplot()



# other -------------------------------------------------------------------

plants <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-18/plants.csv')


# linear models - penguin data --------------------------------------------
# might be that you have to install the packages see and qqplotr if you get
# errors when checking the model
library(palmerpenguins)
library(ggplot2)
library(performance)
# do larger penguins generally have a longer bill? -> regression

ggplot(penguins, aes(x=body_mass_g, y=bill_length_mm)) +
  geom_point() +
  geom_smooth(method = "lm", alpha = 0)

lm1 <- lm(bill_length_mm ~ body_mass_g, data = penguins)

# looks not perfect
# Some bariance not really constant and residuals are higher than expected from a
# normal distribution for high and low values respectively (see also distribution)
# think about possible reasons -> is it just the data that looks like this or might we need
# another explanatory variable?

check_model(lm1)

# we might have to consider species as well? -> Ancova
# The green species has considerably higher residuals than the others

ggplot(penguins, aes(x=body_mass_g, y=bill_length_mm)) +
         geom_point(aes(color = species)) +
  geom_smooth(method = "lm", alpha = 0, formula = y~x)


# is this effect different between species? -> Ancova
# note that if we put color in the top level aesthetics, also the linear model
# lines are added by group
ggplot(penguins, aes(x=body_mass_g, y=bill_length_mm, color = species)) +
  geom_point() +
  geom_smooth(method = "lm", alpha = 0)

# without interaction
lm2 <- lm(bill_length_mm ~ body_mass_g + species, data = penguins)

# Hey now the assumptions look pretty good if we add species as explanatory variable
check_model(lm2)

anova(lm2)
summary(lm2)

# with interaction
lm2b <- lm(bill_length_mm ~ body_mass_g * species, data = penguins)

# Hey now the assumptions look pretty good if we add species as explanatory variable
# colinearity in this case does not matter because it is the interaction that is of course related
check_model(lm2b)

anova(lm2b)
summary(lm2b)

# Do penguins differ in weight between sex and does this depend on species?

ggplot(penguins, aes(x=sex, y=body_mass_g, color = species))+
  geom_boxplot(notch = TRUE)

lm3 <- lm(body_mass_g ~ sex * species, data = penguins)

# looks pretty good
check_model(lm3)

anova(lm3)

# Add model results to the plot
ggplot(penguins, aes(x=sex, y=body_mass_g, color = species))+
  geom_boxplot(notch = TRUE, position = position_dodge(width = 1)) +
  stat_summary(position = position_dodge(width = 1),
               fun = mean)


# linear models - slides --------------------------------------------------
library(tidyverse)
prod <- readr::read_tsv(here::here("day_03/data/05_productivity.txt"))

# make site a factor
prod <- mutate(prod, across(site, as_factor))

# Hypothesis 1: Productivity increases with species richness:
ggplot(prod, aes(x=richness, y=productivity))+
  geom_point()

# looks like yes
# fit a linear regression model
lm1 <- lm(productivity ~ richness, data = prod)
lm1a <- lm(productivity ~ richness, data = prod[prod$site == "site1",])
lm1b <- lm(productivity ~ richness, data = prod[prod$site == "site2",])
# Test whether the model is better than the null model (mean)
anova(lm1)
# Want to see the coefficients?
summary(lm1)
# extract coefficients
intercept <- lm1$coefficients[1]
slope <- lm1$coefficients[2]

# add to plot
# option 1: ab line with slope and intercept
ggplot(prod, aes(x=richness, y=productivity))+
  geom_point()+
  geom_abline(intercept = intercept, slope = slope)
#option 2 geom_smooth
ggplot(prod, aes(x=richness, y=productivity))+
  geom_point()+
  geom_smooth(method ="lm") # default formula = y~x

# Test assumptions
# install.packages("performance")
library(performance)
library(lindia)
check_model(lm1)
#---

# Hypothesis 2: Maybe both factors influence productivity?
ggplot(prod, aes(x=richness, y=productivity, color = site))+
  geom_point()

# make model:
lm3 <- lm(productivity ~ richness + site + richness:site, data = prod)
lm4 <- lm(productivity ~ richness + site, data = prod) # whats the difference

anova(lm3)
anova(lm4)
drop1(lm3, test = "F")

summary(lm3)
summary(lm4)

# make a plot
ggplot(prod, aes(x=richness, y=productivity, color = site))+
  geom_point() +
  geom_smooth(method = "lm")

# but: This is with interaction. Maybe we want to plot without the interaction?
# extract coefficients and plot ab lines

# Check assumptions
check_model(lm3) # collinearity makes sense
check_model(lm4) #  are okay

# plot lm4 with slopes and intercepts

lm4_slope <- lm4$coefficients[2]
lm4_intercept1 <- lm4$coefficients[1]
lm4_intercept2 <- lm4$coefficients[1] + lm4$coefficients[3]

ggplot(prod, aes(x=richness, y=productivity, color= site))+
  geom_point() +
  geom_abline(slope = lm4_slope, intercept = lm4_intercept1 , color = "red")+
  geom_abline(slope = lm4_slope, intercept = lm4_intercept2 , color = "blue")

# or using the predict function
pred_data <- expand_grid(
  richness = min(prod$richness):max(prod$richness),
  site = c("site1", "site2")
)
predictions <- predict(lm4, newdata = pred_data)
pred_data$productivity <- predictions

ggplot(prod, aes(x=richness, y=productivity, color= site))+
  geom_point() +
  geom_line(data= pred_data)



# chickwts lm -------------------------------------------------------------

chicks_lm <- lm(weight~feed, data = chickwts)

ggplot(chickwts, aes(x=feed, y=weight))+
  geom_boxplot()

# Cédric version

chickwts %>%
  mutate(feed = as.factor(feed)) %>%
  mutate(feed = fct_reorder(feed, -weight)) %>%
  ggplot(aes(x=feed, y=weight, color = feed))+
  geom_boxplot()+
  #geom_point(size = 3, alpha = 0.15)+
  geom_jitter(size = 3, alpha = 0.25, width = 0.2)+
  coord_flip()+
  ggsci::scale_color_uchicago()+
  labs(y = "weight [g]", x = "Diet")+
  theme(legend.position = "none")


# npp data ----------------------------------------------------------------

npp <- read_tsv(here::here("day_03/data/06_npp.txt"))

ggplot(npp, aes(x=fertilized, y = npp, fill = flooding))+
  geom_boxplot()+
  facet_wrap(~beaver)

# Hypothesis??? Seems to be an interaction between fertilized and flooding

npp_lm <- lm(npp ~ fertilized + flooding + beaver + fertilized : flooding, data = npp)

check_model(npp_lm)

predict(npp_lm, newdata = data.frame(fertilized = "no", flooding = "rarely", beaver = "no"))



# log-transformation ------------------------------------------------------

msleep
ggplot(msleep, aes(x = awake, y = brainwt))+
  geom_point()+
  scale_y_log10()

sleep_mod <- lm(brainwt ~ awake, data = msleep)
sleep_mod2 <- lm(log(brainwt) ~ awake, data = msleep)
check_model(sleep_mod) # bad -> pattterns in residuals
check_model(sleep_mod2) # much better


# add preditions to plot -> on the log scale

ggplot(msleep, aes(x = sleep_total, y = brainwt))+
  geom_point()+
  geom_smooth(method = "lm") + # careful if you have models with or withour interaction
  scale_y_log10()

# but what if we want to plot our data on the normal scale?
ggplot(msleep, aes(x = sleep_total, y = brainwt))+
  geom_point()+
  geom_smooth(method = "lm")  # careful if you have models with or withour interaction

# use predict function

pred_data <- tibble(sleep_total = min(msleep$sleep_total):max(msleep$sleep_total))
predictions <- predict(sleep_mod2, newdata = pred_data)
# backtransform the predictions and add them to the pred_data tibble
pred_data$brainwt <- exp(predictions)

ggplot(msleep, aes(x=sleep_total, y = brainwt))+
  geom_point()+
  geom_line(data = pred_data, color = "red")



# log-transform decay -----------------------------------------------------

decay <- read_tsv("day_03/data/Decay.txt")
# no transformation
ggplot(decay, aes(x= time, y=amount))+
  geom_point()
# log transformed response
# no transformation
ggplot(decay, aes(x= time, y=amount))+
  geom_point()+
  scale_y_log10()

mod <- lm(log(amount)~time, data = decay)

# Add model to data
# on transformed scale:
ggplot(decay, aes(x= time, y=amount))+
  geom_point()+
  scale_y_log10()+
  geom_smooth(method = "lm")

# on the original scale using predict
pred_data <- tibble(time = 0:30)
amount_pred <- predict(mod, newdata = pred_data)
# backtransform to original scale
pred_data$amount <- exp(amount_pred)

ggplot(decay, aes(x= time, y=amount))+
  geom_point()+
  geom_line(data = pred_data)

# or using the fancy thing (which does not produce the same results)
ggplot(decay, aes(x= time, y=amount))+
  geom_point()+
  geom_smooth(method = "glm", method.args = list(family = gaussian(link = "log")))


# tidyr -------------------------------------------------------------------

library(tidyverse)

table1 <- tibble(
  country = rep(c("Afghanistan", "Brazil", "China"), each = 2),
  year = rep(c(1999L, 2000L), 3),
  cases = c(745L, 2666L, 37737L, 80488L, 212258L, 213768L),
  population = c(19987071L, 20595360L, 172006362L, 174504898L, 1272915272L, 1280428583L)
)

table2 <- pivot_longer(table1, cols = c("cases", "population"),
                       names_to = "type", values_to = "count")

table3<- unite(table1, col = "rate", c("cases", "population"), sep = "/")
# cases
table4a <- table1 %>%
  select(-population) %>%
  pivot_wider(names_from = "year", values_from = "cases")
# population
table4b <- table1 %>%
  select(-cases) %>%
  pivot_wider(names_from = "year", values_from = "population")

# Problem 1: variable split between columns -------------------------------

cities <- c("Istanbul", "Moscow", "London", "Saint Petersburg", "Berlin", "Madrid", "Kyiv", "Rome", "Bucharest", "Paris")
population <- c(15.1e6, 12.5e6, 9e6, 5.4e6, 3.8e6, 3.2e6, 3e6, 2.8e6, 2.2e6, 2.1e6)
area_km2 <- c(2576, 2561, 1572, 1439,891,604, 839, 1285, 228, 105 )

country <- c("Turkey", "Russia", "UK", "Russia", "Germany", "Spain",
                        "Ukraine", "Italy", "Romania", "France")
# tidy
tidy <- tibble(city = cities,
           population = population,
           area_km2 = area_km2,
           country = country)

# untidy
# Problems:
# each row has multiple observations
# at the same time each observation is split across multiple rows (cases and population)
# the variable location is split into multiple columns
# variable country and city are united

untidy1 <- unite(tidy, col = "location", c("country", "city")) %>%
  pivot_longer(c("population", "area_km2"), names_to = "type") %>%
  pivot_wider(names_from = "location", values_from = "value")

# fix untidy 1:
# step 1: fix location in multiple columns
step1 <- untidy1 %>%
  pivot_longer(c(Turkey_Istanbul:France_Paris), names_to = "location")

# step2: split location into the variables country and city
step2 <- step1 %>%
  separate(location, sep = "_", into = c("country", "city"))

# step3: observation and population into one row
step3 <- step2 %>%
  pivot_wider(names_from = type, values_from = value)
# Tada


# dplyr -------------------------------------------------------------------

library(tidyverse)
library(jsonlite)

soybean_use <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-06/soybean_use.csv')

# Filter

# filter Germany
filter(soybean_use, entity == "Germany")
# filter Germany, Austria and Switzerland
countries_select <- c("Germany", "Austria", "Switzerland")
filter(soybean_use, entity %in% countries_select)
# only filter countries with a country code:
filter(soybean_use, is.na(code))
filter(soybean_use, !is.na(code))
# with relational and logical operators:
filter(soybean_use, between(year, 1970, 1980) & entity == "Germany")

# Select
# select entity, year and human_food:
select(soybean_use, entity, year, human_food)
# remove variables
select(soybean_use, -entity, -year, -human_food)
# starts_with
select(soybean_use, starts_with("p"))
select(soybean_use, ends_with("d"))
# contains
select(soybean_use, contains("code") | contains("year"))
# select multiple cols
select(soybean_use, 1:3)
select(soybean_use, code:animal_feed)

# mutate
# create a new variable with ifelse
mutate(soybean_use, single_country = ifelse(is.na(code), FALSE, TRUE))
# case when -> like if else but more complex
# assume in 180 they passed leg1, and in 2000 they changed it to leg2
mutate(soybean_use, legislation = case_when(
  year < 2000 & year >=1980 ~ "leg_1",
  year >= 2000 ~ "leg_2",
  TRUE ~ "no_leg"
))


# transmute
# create new columns and only keep the new columns
transmute(soybean_use,
          ratio_processed_animal = processed/animal_feed,
          ratio_human_animal = human_food/animal_feed)


# Arrange
# reorder data
# lowest processed first
arrange(soybean_use, processed)
# highest
arrange(soybean_use, desc(year), desc(processed))


# Summarise alone
# what happened here?
summarise(soybean_use,
          total_animal = sum(animal_feed),
          total_human = sum(human_food))

# collapse the entire data into one row
summarise(soybean_use,
          total_animal = sum(animal_feed, na.rm = TRUE),
          total_human = sum(human_food, na.rm = TRUE))

# With group by
soybean_use_group <- group_by(soybean_use, year)

summarise(soybean_use_group,
          total_animal = sum(animal_feed, na.rm = TRUE),
          total_human = sum(human_food, na.rm = TRUE))

# count

count(soybean_use, entity)

# Pipe
# combine operations
soybean_use %>%
  filter(!is.na(code)) %>%
  group_by(year) %>%
  summarise(
    mean_processed = mean(processed, na.rm=TRUE),
    sd_processed = sd(processed, na.rm = TRUE)
  ) %>%
  arrange(desc(year))

# simple pipe
soybean_use %>%
  count(year)

plot <-data.frame(x = c(1,2,3), y= c(1,2.5,2))%>%
  ggplot(aes(x=x, y=y)) +
  geom_point(size=3)+
  geom_abline(intercept = 0.2, slope= .7, color = "red")+
  scale_y_continuous(expand = c(0,0), limits = c(0,3.5)) +
  scale_x_continuous(expand = c(0,0), limits = c(0,3.5)) +
  geom_segment(x=1, y=0.9, xend = 1, yend=1,linetype = "dashed")+
  geom_segment(x=2, y=1.6, xend=2, yend=2.5,linetype = "dashed")+
  geom_segment(x=3, y=2.3, xend=3, yend=2,linetype = "dashed")+
  annotate(geom = "text", x = 2.1, y= 2.5, label = "Y[i]",parse = TRUE, size = 8)+
  annotate(geom = "text", x = 2.1, y= 1.4, label = latex2exp::TeX("$\\hat{Y}_i$"),
           parse = TRUE, size = 8, color = "red")+
  theme(axis.text = element_text(size = 20),
        axis.title = element_text(size = 20))

ggsave("slides/img/residual.png", plot)



# GLMs --------------------------------------------------------------------


# poisson -----------------------------------------------------------------
library(tidyverse)
specdat <- read_tsv("day_03/data/species.txt")

ggplot(specdat, aes(y=Species, x=Biomass, color = pH))+
  geom_point()

mod1 <- glm(Species~Biomass + pH + Biomass:pH, data = specdat,
         family = "poisson")
# can't interpret the coefficients anymore because on scale of link function
summary(mod1)

# Hypothesis testing with Chisq test
drop1(mod1, test = "Chisq")

# plot results
pred_dat <- expand_grid(
  Biomass = seq(from = min(specdat$Biomass), to = max(specdat$Biomass), length.out = 200),
  pH = unique(specdat$pH)
)
# predict species with the new data
pred_dat$Species <- predict(mod1, newdata = pred_dat,
                            type = "response") #<<


ggplot(specdat, aes(y=Species, x=Biomass, color = pH))+
  geom_point()+
  geom_line(data = pred_dat)

# or with geom_smooth
ggplot(specdat, aes(y=Species, x=Biomass, color = pH))+
  geom_point()+
  geom_smooth(method = "glm", method.args = list(family = "poisson"))



# Tests -------------------------------------------------------------------
library(tidyverse)
set.seed(123)

mydata <- tibble(
  normal = rnorm(n = 400, mean = 50, sd = 5),
  non_normal = runif(n = 400, min = 45, max = 55)
)

mydata %>%
  pivot_longer(cols = 1:2, names_to = "type", values_to = "value") %>%
  ggplot()+
  geom_histogram(alpha = 0.5, aes( x = value, fill = type,y = ..density..)) + # plot probability instead of count on y axis
  stat_function(fun = dnorm,
                args = list(mean = 50,
                            sd = 5),
                color = "darkorange", size = 1)+
  stat_function(fun = dnorm,
                args = list(mean = mean(mydata$non_normal),
                            sd = sd(mydata$non_normal)),
                color = "cyan4", size = 1)+
    scale_fill_manual(values = c("cyan4", "darkorange"))+
  theme(legend.position = c(0.85,0.85))



# ks.test -----------------------------------------------------------------

ks.test(mydata$normal, "pnorm", mean = mean(mydata$normal),
        sd = sd(mydata$normal))


ks.test(mydata$non_normal, "pnorm", mean = mean(mydata$non_normal),
        sd = sd(mydata$non_normal))


# Shapiro wilk ------------------------------------------------------------

shapiro.test(mydata$normal)
shapiro.test(mydata$non_normal)


# QQ ----------------------------------------------------------------------

ggplot(mydata, aes(sample = normal)) + stat_qq() + stat_qq_line()

ggplot(mydata, aes(sample = non_normal)) + stat_qq() + stat_qq_line()


# Ftest -------------------------------------------------------------------

InsectSprays

TreatA <- InsectSprays[InsectSprays$spray == "A",]$count
TreatB <- InsectSprays[InsectSprays$spray == "B",]$count
TreatE <- InsectSprays[InsectSprays$spray == "E",]$count

TreatA <- filter(InsectSprays, spray == "A")$count
TreatB <- filter(InsectSprays, spray == "B")$count
TreatE <- filter(InsectSprays, spray == "E")$count

shapiro.test(TreatA) #normal
shapiro.test(TreatB) # normal
shapiro.test(TreatE) # normal

var.test(TreatA, TreatB) # equal
var.test(TreatA, TreatE) # not equal


#---
# A,B,E: Normal distr.
# AB equal var

t.test(TreatA, TreatB, var.equal = TRUE)
t.test(TreatA, TreatE, var.equal = FALSE)
wilcox.test(TreatA, TreatB)

# paired data points
# before and after
#

