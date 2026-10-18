# Title: Create dummy growth curve data
# Date: 2026-10-18
# Author: Selina Baldauf
# Purpose: Simulate raw plate reader OD values of 3 strains at 6 substrate
# concentrations and save them as one Excel sheet per concentration.

library(tidyverse)
library(writexl)

# Simulation parameters -------------------------------------------------------

concentrations <- c(0.05, 0.1, 0.2, 0.5, 1, 2) # mM, one sheet each
time <- seq(0, 48, by = 2) # hours

# gbr: substrate needed per OD unit (max OD = concentration / gbr)
# rate: growth rate per hour
strains <- tibble(
  strain = c("strainA", "strainB", "strainC"),
  gbr = c(1, 3, 10),
  rate = c(0.4, 0.3, 0.2)
)

od_start <- 0.01 # inoculum OD
blank <- 0.045 # plate reader OD of medium only
correction_factor <- 0.23 # plate reader OD = cuvette OD * correction_factor

# Simulate growth curves ------------------------------------------------------

set.seed(42)
growth <- expand_grid(
  strains,
  concentration = concentrations,
  replicate = 1:3
) |>
  # replicates differ slightly in their max OD
  mutate(od_max = od_start + concentration / gbr * exp(rnorm(n(), sd = 0.08))) |>
  expand_grid(time = time) |>
  mutate(
    # logistic growth
    od = od_max / (1 + (od_max / od_start - 1) * exp(-rate * time)),
    # convert to raw plate reader values and add measurement noise
    od_raw = blank +
      (od - od_start) * correction_factor * exp(rnorm(n(), sd = 0.03)) +
      rnorm(n(), sd = 0.0003),
    od_raw = round(od_raw, 4)
  )

# Save as one sheet per concentration -----------------------------------------

sheets <- growth |>
  mutate(column = paste(strain, replicate, sep = "_")) |>
  select(concentration, time, column, od_raw) |>
  pivot_wider(names_from = column, values_from = od_raw) |>
  mutate(sheet = paste(concentration, "mM")) |>
  select(-concentration) |>
  nest(.by = sheet)

write_xlsx(set_names(sheets$data, sheets$sheet), "data/dummy_growth_curves.xlsx")
