library(tidyverse)
library(readxl)

plate_1 <- read_excel("data/Plate 1.xlsx")
plate_2 <- read_excel("data/Plate 2.xlsx")

id_data <- read_excel("data/CO2_sensors_plate_readData_frame_.xlsx")

all_data <- bind_rows(
  list(
    plate_1,
    plate_2
  ),
  .id = "Plate"
)

all_data <- all_data |>
  select(-`T° 600`) |>
  pivot_longer(
    cols = !c(Plate, Time),
    names_to = "Well"
  ) |>
  mutate(
    Plate = as.numeric(Plate)
  )

all_data <- all_data |>
  left_join(
    id_data,
    by = c("Plate", "Well")
  ) |>
  drop_na()

# Find the first measurement
first_time_point <- all_data |>
  filter(Time == 0) |>
  rename(
    time_0 = value
  ) |>
  select(-Time)

# Combine all data with time_0
all_data <- all_data |>
  left_join(
    first_time_point,
    by = c("Plate", "Well", "CO2", "Strain")
  )

# subtract time point 0 from all the values
all_data <- all_data |>
  mutate(
    corrected_value = value - time_0
  ) |>
  mutate(
    normalized_value = corrected_value / 0.23 + 0.01
  )

summarized_data <- all_data |>
  drop_na(value) |>
  summarize(
    mean_norm_value = mean(normalized_value),
    .by = c(Plate, Time, CO2, Strain)
  )

ggplot(summarized_data, aes(x = Time, y = mean_norm_value, color = CO2)) +
  geom_point() +
  geom_line() +
  facet_wrap(vars(Strain)) +
  scale_y_log10()
