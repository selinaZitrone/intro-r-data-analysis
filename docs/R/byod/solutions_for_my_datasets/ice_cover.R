# Dataset 4: Ice cover duration and temperature
# Data comes from the R package lterdatasampler
# For more examples and analyses, visit:
# https://lter.github.io/lterdatasampler/articles/ntl_ntl_icecover_vignette.html

# Load required libraries
library(tidyverse)
library(lterdatasampler)
library(performance)

# Display the datasets
ntl_icecover
ntl_airtemp

# View dataset documentation
?ntl_icecover
?ntl_airtemp

# 1. Ice duration analysis -----------------------------------------------------
# Analyze how ice cover duration has changed over the years and compare between lakes

# Plot ice cover duration trends for each lake
ggplot(ntl_icecover, aes(x = year, y = ice_duration, color = lakeid)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Ice Cover Duration Trends by Lake",
    x = "Year",
    y = "Average Ice Cover Duration (days)",
    color = "Lake"
  )

# Boxplot to compare ice cover duration between lakes
ggplot(
  ntl_icecover,
  aes(x = lakeid, y = ice_duration, color = lakeid)
) +
  geom_boxplot(notch = TRUE) +
  geom_point(position = position_jitterdodge())

# 1.2 Linear model of ice duration trend ---------------------------------------
# Fit a linear model to analyze the trend of ice duration over the years

# Fit linear model with interaction term
ice_lm <- lm(ice_duration ~ year + lakeid + year:lakeid, data = ntl_icecover)
# Test for significant effects
drop1(ice_lm, test = "F")
# Result: No interaction between year and lakeid -> Both lakes react similarly

# Fit the model without interaction term to check overall trend
ice_lm2 <- lm(ice_duration ~ year + lakeid, data = ntl_icecover)
drop1(ice_lm2, test = "F")
# Result: Significant effect of year on ice duration -> Ice duration declined over the years

# Check model performance
performance::check_model(ice_lm2) # Model diagnostics

# 2. Temperature trend analysis -------------------------------------------------
# Analyze how average air temperature has changed over the years

# Calculate yearly average temperature
yearly_temp <- ntl_airtemp |>
  summarise(avg_temp = mean(ave_air_temp_adjusted, na.rm = TRUE), .by = year)

# Plot temperature trend over time
ggplot(yearly_temp, aes(x = year, y = avg_temp)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    title = "Average Temperature Trend",
    x = "Year",
    y = "Average Temperature (°C)"
  )

# Alternative method to visualize temperature trend
ggplot(ntl_airtemp, aes(x = year, y = ave_air_temp_adjusted)) +
  stat_summary() +
  geom_smooth(method = "lm")

# 2.2 Linear regression for temperature trend ----------------------------------
# Fit a linear model to analyze the trend of average temperature over the years

# Fit the model
lm_temp_Av <- lm(avg_temp ~ year, yearly_temp)
# Test for significant effect
drop1(lm_temp_Av, test = "F")
# Result: Significant effect of year on temperature -> Temperature increased over the years

# Check model performance
performance::check_model(lm_temp_Av) # Model diagnostics

# 3. Detect significant temperature anomalies -----------------------------------
# Identify significant temperature anomalies compared to a reference period (1870-1900)

# Calculate mean and standard deviation of reference period
temp_ref <- filter(yearly_temp, year >= 1870, year <= 1900) |>
  summarise(ref_mean = mean(avg_temp), ref_sd = sd(avg_temp))

# Add reference period statistics to yearly temperature data
yearly_temp <- bind_cols(yearly_temp, temp_ref)

# Calculate temperature anomalies
yearly_temp <- yearly_temp |>
  mutate(anomaly = (avg_temp - ref_mean) / ref_sd)

# Plot temperature anomalies
ggplot(
  data = yearly_temp,
  aes(x = year, y = anomaly)
) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  theme_minimal() +
  geom_smooth(
    method = lm,
    se = FALSE,
    color = "blue",
    linetype = "dashed"
  ) +
  labs(
    y = "Standardized Anomalies",
    x = "Year",
    title = "Air temperature standardized anomalies"
  )

# Enhanced plot with color differentiation
temp_anomalies <- ggplot(
  data = yearly_temp,
  aes(x = year, y = anomaly)
) +
  geom_bar(aes(fill = anomaly > 0), stat = "identity", show.legend = FALSE) +
  theme_minimal() +
  geom_smooth(method = lm, color = "black") +
  scale_fill_manual(values = c("blue4", "red4")) +
  labs(
    y = "Standardized Anomalies",
    x = "Year",
    title = "It's getting warmer",
    subtitle = "Standardized temperature anomalies compared to reference period 1870-1900"
  ) +
  scale_y_continuous(breaks = seq(-3, 3, 1))

temp_anomalies

# Adjust the path to something that exists in your project
ggsave("img/temp_anomalies.png", temp_anomalies)

# Temperature anomalies for each month -----------------------------------------
# Analyze temperature anomalies for each month compared to the reference period

# Calculate mean and standard deviation for each month in the reference period
temp_ref_month <- ntl_airtemp |>
  filter(year >= 1870, year <= 1900) |>
  group_by(month = month(sampledate)) |>
  summarise(ref_mean = mean(ave_air_temp_adjusted), ref_sd = sd(ave_air_temp_adjusted))

# Calculate monthly mean temperature
ntl_airtemp_month <- ntl_airtemp |>
  group_by(month = month(sampledate), year) |>
  summarise(temp_month = mean(ave_air_temp_adjusted, na.rm = TRUE))

# Add reference period statistics to monthly temperature data
ntl_airtemp_month <- ntl_airtemp_month |>
  left_join(temp_ref_month, by = "month") |>
  mutate(anomaly = (temp_month - ref_mean) / ref_sd)

# Plot monthly temperature anomalies
anomalies_polar <- ggplot(ntl_airtemp_month, aes(
  x = month,
  y = anomaly,
  color = ifelse(anomaly < 0, "too cold", "too warm"),
  group = year
)) +
  geom_line() +
  scale_color_manual(values = c("too warm" = "red4", "too cold" = "blue4")) +
  coord_polar() +
  theme_minimal() +
  labs(
    title = "Monthly Temperature Anomalies",
    x = "Month",
    y = "Standardized Anomalies",
    color = "Year"
  )

anomalies_polar

# Animate the plot with gganimate
library(gganimate) # install.packages("gganimate")
anomalies_animation <- animate(
  anomalies_polar +
    transition_reveal(year),
  nframes = 100, fps = 10, renderer = gifski_renderer()
)
# Save the animation
anim_save("img/anomalies_animation.gif", anomalies_animation)

# 5. Explore correlations between ice cover duration and temperature -----------
# Analyze the correlation between ice cover duration and winter temperature

# Calculate mean temperature for each winter season (November - April)
winter_seasons <- ntl_airtemp |>
  filter(month(sampledate) %in% c(11, 12, 1, 2, 3, 4)) |>
  mutate(season = case_when(
    month(sampledate) %in% 1:4 ~ year - 1,
    .default = year
  ))

# Calculate mean temperature for each winter season
winter_seasons <- winter_seasons |>
  summarise(avg_temp = mean(ave_air_temp_adjusted, na.rm = TRUE), .by = season)

# Join ice cover data with seasonal temperature data
ice_temp_correlation <- ntl_icecover |>
  left_join(winter_seasons, by = c("year" = "season"))

# Plot correlation between ice cover duration and winter temperature for each lake
ggplot(ice_temp_correlation, aes(x = ice_duration, y = avg_temp, color = lakeid)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Ice Cover Duration vs. Winter Temperature",
    x = "Ice Cover Duration (days)",
    y = "Average Winter Temperature (°C)",
    color = "Lake"
  )
