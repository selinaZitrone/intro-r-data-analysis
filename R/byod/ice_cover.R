library(lterdatasampler)
library(tidyverse)
# https://lter.github.io/lterdatasampler/articles/ntl_icecover_vignette.html

ntl_icecover
# add air temperature
write_csv(ntl_icecover, "data/icecover.csv")
ice <- read_csv("data/icecover.csv")

# Difference in ice cover  duration between sites

ggplot(
  ice,
  aes(x = lakeid, y = ice_duration, color = lakeid)
) +
  geom_boxplot(notch = TRUE) +
  geom_point(position = position_jitterdodge())

# Over the years?

ggplot(ice,
       aes(x=year, y=ice_duration, color=lakeid))+
  geom_line()+
  geom_smooth(method = "lm")

# fit a model

ice_lm <- lm(ice_duration~year + lakeid + year:lakeid, data = ice)
drop1(ice_lm, test = "F")
# seems to be no interaction
ice_lm2 <- lm(ice_duration~year + lakeid, data = ice)
drop1(ice_lm2, test = "F")
# o difference between lakes
ice_lm3 <- lm(ice_duration~year, data = ice)
drop1(ice_lm3, test = "F")
performance::check_model(ice_lm3) # looks ok-ish

# summarize both lakes for average ice cover
ice_avg <-ice %>%
  drop_na(ice_duration) %>%
  group_by(year) %>%
  summarise(ice_duration = mean(ice_duration)) %>%
  rename(avg_ice_duration = ice_duration)

ggplot(ice_avg, aes(x=year, y=avg_ice_duration))+geom_line()+
  geom_smooth(method = "lm")

# What does this have to do with temperature?
write_csv(ntl_airtemp, "data/ntl_temp.csv")
temp <- read_csv("data/ntl_temp.csv")

ggplot(temp, aes(x=year, y=ave_air_temp_adjusted))+
  stat_summary()+
  geom_smooth(method="lm")

# is there a clear trend?
lm_temp <- lm(ave_air_temp_adjusted ~ year, temp)
drop1(lm_temp, test = "F")
performance::check_model(lm_temp)

# summarize mean annual temperature
mapt <- temp %>% group_by(year) %>% summarize(temp = mean(ave_air_temp_adjusted, na.rm=TRUE))
lm_temp_Av <- lm(temp ~ year, mapt)
drop1(lm_temp_Av, test = "F")
performance::check_model(lm_temp_Av)

# join the tables
both <- left_join(ice_avg, mapt, by = "year")
ggplot(both, aes(x=avg_ice_duration,y=temp))+
  geom_point()+
  geom_smooth(method = "lm")

# Only for the wintermonths
