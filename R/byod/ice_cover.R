# Dataset 4: Ice cover duration and temperature
# Data comes from the R package lterdatasampler
# For more examples and analyses look at:
# https://lter.github.io/lterdatasampler/articles/ntl_icecover_vignette.html


# preparation -------------------------------------------------------------

# install.packages("lterdatasampler")
library(lterdatasampler)
library(tidyverse)

# Look at the two data sets
ntl_icecover
ntl_airtemp


# 1. Analyse only ice cover -----------------------------------------------

# Difference in ice cover between sites
ggplot(
  ntl_icecover,
  aes(x = lakeid, y = ice_duration, color = lakeid)
) +
  geom_boxplot(notch = TRUE) +
  geom_point(position = position_jitterdodge())


# Ice development over the years
# Looks like Ice duration declined over the years. Is this statistically significant?
ggplot(
  ntl_icecover,
  aes(x = year, y = ice_duration, color = lakeid)
) +
  geom_line() +
  geom_smooth(method = "lm")

# Linear model analysis of ice cover -------------------------------------------

# Analyse the ice duration trend over the years.

# fit linear model
ice_lm <- lm(ice_duration ~ year + lakeid + year:lakeid, data = ntl_icecover)
# Check for significant effects
drop1(ice_lm, test = "F")
# Result: No interaction between year and lakeid -> Both lakes react in the same way

# Fit the model without interaction to see if ice duration declined significantly over
# the years
ice_lm2 <- lm(ice_duration ~ year + lakeid, data = ntl_icecover)
drop1(ice_lm2, test = "F")
# Result: There is no effect of the lake id on ice duration, but a significant effect of
# the year -> Ice duration declined over the years

# Check the model performance
performance::check_model(ice_lm2) # looks ok-ish (normality of residuals does not look perfect)


# Summarize ice cover of both lakes ---------------------------------------

# summarize both lakes for average ice cover
ice_avg <- ntl_icecover |>
  summarise(avg_ice_duration = mean(ice_duration, na.rm = TRUE), .by = year) |>
  drop_na()

# Plot average ice duration over the years
ggplot(ice_avg, aes(x = year, y = avg_ice_duration)) +
  geom_line() +
  geom_smooth(method = "lm")

# Fit linear model to average ice duration
lm_avg <- lm(avg_ice_duration ~ year, data = ice_avg)
drop1(lm_avg, test = "F") # Still a significant effect of year

# Check model performance
performance::check_model(lm_avg) # looks ok-ish (Still: normality of residuals does not look perfect)


# 2. Analyse temperature alone --------------------------------------------

# Development of air temperature over time
ggplot(ntl_airtemp, aes(x = year, y = ave_air_temp_adjusted)) +
  stat_summary() +
  geom_smooth(method = "lm")

# is there a clear trend over time?
# summarize mean annual temperature
mapt <- ntl_airtemp |>
  summarize(temp = mean(ave_air_temp_adjusted, na.rm = TRUE), .by = year)
lm_temp_Av <- lm(temp ~ year, mapt)
drop1(lm_temp_Av, test = "F")
# Result: There is a significant effect of year on temperature -> Temperature increased over the years
performance::check_model(lm_temp_Av) # Looks good


# Analyse both data sets together -----------------------------------------

# join the tables (avg_ice_duration per year and mean annual temperature)
both <- left_join(ice_avg, mapt, by = "year")

# The higher the annual temperature, the shorter the ice duration
ggplot(both, aes(x = temp, y = avg_ice_duration)) +
  geom_point() +
  geom_smooth(method = "lm")
