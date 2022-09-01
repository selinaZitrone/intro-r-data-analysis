#library(lterdatasampler)
library(tidyverse)

write_csv(pie_crab, file = "data/pie_crab.csv", col_names = TRUE)
read_csv(file = "data/pie_crab.csv", col_names = TRUE)
pie_crab

ggplot(data = pie_crab, aes(y = latitude, x = water_temp)) +
  geom_point()

pie_crab %>%
  ggplot(aes(y=latitude)) +
  geom_boxplot(aes(size, group = latitude, color=-latitude), outlier.size=0.8) +
  scale_x_continuous(breaks = seq(from = 7, to = 23, by = 2), limits = c(6.5,24))+
  scale_y_continuous(breaks = seq(from = 29, to = 43, by = 2), limits = c(29, 43.5)) +
  theme(legend.position= "none")

pie_crab %>%
  ggplot(aes(x=latitude, y = size))+
  geom_point()+
  geom_smooth(method = "lm")

# fit a linear model

lm_crab <- lm(size ~ latitude, data = pie_crab) # bergmanns's rule

drop1(lm_crab, test = "F")
performance::check_model(lm_crab) #looks good

lm_crab_temp <- lm(size~ latitude + water_temp, data = pie_crab)
drop1(lm_crab_temp, test = "F")
performance::check_model(lm_crab_temp) # high correlation between latitude and temp

# compare sites
ggplot(pie_crab, aes(x=site, y=size, color = latitude))+
  geom_boxplot()
