# Purpose:

library(lterdatasampler)
library(tidyverse)
ntl_airtemp

ggplot(ntl_airtemp, aes(x = sampledate, y = ave_air_temp_adjusted)) +
  geom_line() +
  labs(title = "Air Temperature at NTL",
       x = "Date",
       y = "Air Temperature (°C)")


ntl_airtemp_mean_year <- ntl_airtemp |>
  group_by(year = lubridate::year(sampledate)) |>
  summarize(mean_air_temp = mean(ave_air_temp_adjusted, na.rm = TRUE))

ggplot(ntl_airtemp_mean_year, aes(x = year, y = mean_air_temp)) +
  geom_line() +
  labs(title = "Mean Air Temperature at NTL",
       x = "Year",
       y = "Mean Air Temperature (°C)")

# Use the years 1870-1900 as reference years
# Calculate the mean annual air temperature for each year relative to the
# mean temperature in all references years
ntl_airtemp_mean_year <- ntl_airtemp_mean_year |>
  mutate(mean_air_temp_relative = mean_air_temp -
           mean(mean_air_temp[year %in% 1870:1900]))

# Plot the relative temperature in a barplot with a horizontal line at 0
# Positive temperatures should be red, negative temperatures should be blue
ggplot(ntl_airtemp_mean_year, aes(x = year, y = mean_air_temp_relative)) +
  geom_col(aes(fill = mean_air_temp_relative > 0)) +
  geom_hline(yintercept = 0, color = "black") +
  scale_fill_manual(values = c("blue", "red")) +
  labs(title = "Relative Mean Air Temperature at NTL",
       x = "Year",
       y = "Relative Mean Air Temperature (°C)")
