# Data from https://figshare.com/articles/dataset/A_randomized_placebo-controlled_trial_of_positive_psychology_interventions_in_Australia/1577563/1
# Psych data -------------------------------------------------------------
library(tidyverse)
ahi <- read_csv("./data/psych_data/ahi-cesd.csv")
participants <- read_csv("./data/psych_data/participant-info.csv")

# combine both tables together
ahi <- left_join(ahi, participants, by = c("id", "intervention"))
# only look at ahi and cest total
ahi_total <- select(ahi, !ahi01:cesd20)
# turn columns into factors:
ahi_total <- mutate_at(
  ahi_total,
  c("intervention", "sex", "age", "educ", "income"),
  as.factor
)

ggplot(ahi_total, aes(x = elapsed.days, y = ahiTotal, color = factor(intervention))) +
  geom_point()

# idea: round elapsed days
ggplot(ahi_total, aes(x = elapsed.days)) +
  geom_histogram()

ahi_total %>%
  mutate(
    elapsed.days = round(elapsed.days, -1)
  ) %>%
  ggplot(
    aes(x = elapsed.days, y = ahiTotal, color = factor(intervention))
  ) +
  geom_point()


# What happens at elapsed.days = 0?
filter(ahi_total, near(elapsed.days, 0, 0.9)) %>%
  ggplot(aes(x = educ, y = ahiTotal, fill = sex)) +
  geom_boxplot() +
  facet_wrap(~income)

# correlation between variables
# https://r-coder.com/correlation-plot-r/
corrplot::corrplot(cor(ahi_total %>% select(sex, age, educ, income)))


# create categories of elapsed days:
ahi_total_grouped <-
  ahi_total %>%
  mutate(ahi_total,
    elapsed.days =
      case_when(
        between(elapsed.days, 30, 90) ~ 30,
        between(elapsed.days, 90, 150) ~ 100,
        elapsed.days > 150 ~ 180,
        TRUE ~ elapsed.days
      )
  ) %>%
  mutate(
    elapsed.days = round(elapsed.days, -1)
  )

ahi_total_grouped %>%
  ggplot(
    aes(x = elapsed.days, y = ahiTotal, color = factor(intervention))
  ) +
  stat_summary(position = position_dodge(width = 5)) +
  stat_summary(
    fun = mean, geom = "line",
    position = position_dodge(width = 5)
  ) +
  facet_wrap(~sex)
