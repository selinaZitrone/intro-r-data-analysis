# Purpose: Read OD measurements from an Excel file with one sheet per condition,
# correct them and plot a two-panel figure: growth curves (A) and the maximum OD per condition (B).
# The raw data has the following columns:
# - time: time of measurement
# - strainA_1, strainA_2, strainA_3, strainB_1, ...: raw OD of each strain
#   (strainA, strainB, strainC) and replicate (1-3)
# Each sheet in the excel file represents a different concentration

library(tidyverse)
library(readxl)
library(patchwork) # combine panels A and B into one plot

# Read all sheets -------------------------------------------------------------

# The Excel file has one sheet per concentration (e.g. "0.05 mM").
# Define path to the excel sheet. Here R assumes to be in a project in the data/ folder
excel_path <- "data/dummy_growth_curves.xlsx" # ADJUST to your values

# Get all the sheet names (names after the concentrations)
sheet_names <- excel_sheets(excel_path)

# Read each sheet into a list, then combine all sheets into one table.
# .id = "condition" adds a column with the sheet name
od <- map(sheet_names, \(sheet) read_excel(excel_path, sheet = sheet)) |>
  set_names(sheet_names) |>
  bind_rows(.id = "condition")
od

# Convert to long format ------------------------------------------------------

# We want one row per measurement with the columns:
# condition, time, strain, replicate, od_raw
od <- od |>
  pivot_longer(
    cols = !c(condition, time),
    names_to = "well",
    values_to = "od_raw"
  ) |>
  # split column names like "strainA_1" at the "_" into strain and replicate
  separate_wider_delim(
    well,
    delim = "_",
    names = c("strain", "replicate")
  ) |>
  # get the number from the sheet name ("0.05 mM" -> 0.05)
  mutate(concentration = parse_number(condition))
od

# Correct OD values -----------------------------------------------------------

od <- od |>
  mutate(
    # subtract the first measurement (time 0) of each well
    od_corrected = od_raw - od_raw[time == 0],
    # apply the correction factor (ADJUST to whatever correction you need)
    od_corrected = od_corrected / 0.23,
    # Note: .by must identify one well (one curve), otherwise the wrong t0 is subtracted
    .by = c(concentration, strain, replicate)
  )

# Panel A: growth curves ------------------------------------------------------

# Mean of the triplicates for each time point
od_mean <- od |>
  summarise(
    od_mean = mean(od_corrected),
    .by = c(concentration, strain, time)
  )

# One facet per strain, one line per concentration.
# Color shows the strain, transparency shows the concentration (light = low).
# group = concentration is needed so that each concentration gets its own line
panel_a <- ggplot(
  od_mean,
  aes(
    x = time,
    y = od_mean,
    color = strain,
    alpha = concentration,
    group = concentration
  )
) +
  geom_line(linewidth = 0.8) +
  facet_wrap(vars(strain), nrow = 1) +
  scale_y_log10() +
  scale_alpha_continuous(transform = "log10", range = c(0.2, 1)) +
  labs(
    x = "Time (hours)",
    y = "OD (600 nm)",
    alpha = "Concentration (mM)"
  ) +
  theme_minimal()
panel_a

# Note: If you want growth in the exponential phase to look linear, you can add
# scale_y_log10() to the plot. But note that then you might have to add a small
# value like 0.01 to each curve because log(0) (for the first time point) will be
# -Infinity

# Panel B: maximum OD ---------------------------------------------------------

# Maximum OD of each replicate, then mean and standard deviation of the
# triplicates
max_od <- od |>
  summarise(
    od_max = max(od_corrected),
    .by = c(concentration, strain, replicate)
  ) |>
  summarise(
    mean = mean(od_max),
    sd = sd(od_max),
    .by = c(concentration, strain)
  )
max_od

# Both axes on a log scale. geom_smooth fits the linear model on the log
# scale, so the lines are straight in the plot
panel_b <- ggplot(max_od, aes(x = concentration, y = mean, color = strain)) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, linewidth = 0.8) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0) +
  geom_point(size = 2) +
  # Optional: Scale axes logarithmically if this makes sense for your data
  #scale_x_log10() +
  #scale_y_log10() +
  labs(
    x = "Concentration (mM)",
    y = "Max. OD (600 nm)"
  ) +
  theme_minimal()
panel_b

# Combine both panels ---------------------------------------------------------

# The patchwork package combines multiple plots into one. The easiest version is
panel_a / panel_b # top/bottom
panel_a + panel_b # left right

# Check out https://patchwork.data-imaginist.com/ for more you can do, e.g.
panel_a /
  panel_b +
  plot_annotation(tag_levels = "A")
