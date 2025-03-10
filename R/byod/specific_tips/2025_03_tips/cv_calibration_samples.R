# Setup --------------------
# Load required packages
library(tidyverse)
library(broom) # For tidy model outputs
library(patchwork) # For arranging plots

# Simulate a simplified dataset ----------------------------------------------
# not so important to understand, just check out the resulting table to see
# if it corresponds to your data set somehow

# This dataset contains 12 drugs (drug_1 to drug_12), 5 calibration levels,
# and data from 7 experiments (5 inter-day and 5 intra-day replicates)
set.seed(100) # For reproducibility

# Define experiments_: 5 intra-day and 5 inter-day replicates
experiment_ids <- c(rep("intra_day", 5), rep("inter_day", 5))

# Define drug names as drug_1 to drug_12
drug_names <- paste0("drug_", 1:12)

# Define calibration levels
calib_levels <- c(10, 20, 50, 100, 200)

# Generate the simplified dataset using expand.grid
data_sim <- expand_grid(
  drug = drug_names,
  calib_level = calib_levels,
  experiment = experiment_ids
) |>
  mutate(
    # The true concentration is set to the calibration level
    true_conc = calib_level,
    # Add an experimental bias: inter-day has higher variability than intra-day
    exp_bias = if_else(experiment == "intra-day", rnorm(n(), 1, 0.02), rnorm(n(), 1, 0.05)),
    # Measured concentration is modeled by true concentration with a multiplicative error term
    measured_conc = true_conc * exp_bias * (1 + rnorm(n(), 0, 0.03)),
  )

# Checkout the simulated dataset
data_sim

# Calculate CV ----------------------------------------------------------------
# Group data by drug, calibration level, and experiment type
# CV = sd/mean * 100%
cv_data <- data_sim |>
  summarise(
    mean_conc = mean(measured_conc),
    sd_conc = sd(measured_conc),
    cv_percent = (sd_conc / mean_conc) * 100,
    .by = c(drug, calib_level, experiment)
  )
# look at the cv_data
cv_data

# Fit Calibration Curves for 1 sample ----------------------------------------

# Example of fitting one regression to a subset
# see below on how to do it for all experiments at once
data_subset <- data_sim |>
  filter(drug == "drug_1", experiment == "intra_day")
# fit a linear model
lm_fit <- lm(measured_conc ~ true_conc, data = data_subset)
# get the model statistics
summary(lm_fit)
# or use the glance function from the broom pkg to get nice summarized output
glance(lm_fit)

# Plot just one model and calibration curve
data_subset |>
  ggplot(aes(x = true_conc, y = measured_conc)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Calibration Curve for drug_1 intra_day",
    x = "True Concentration", y = "Measured Concentration"
  ) +
  theme_minimal()

# Fit Calibration Curves for all samples ---------------------------------------
calibration_models <- data_sim |>
  group_by(drug, experiment) |>
  # nest nests your data by the grouping variables
  # here we fit a model to each drug-experiment combination that is defined in group_by
  nest() |>
  mutate(
    # Fit a linear model for each drug-experiment combination
    # response is the measured concentration, predictor is the true concentration
    model = map(data, ~ lm(measured_conc ~ true_conc, data = .x)),
    # Extract model statistics
    model_stats = map(model, glance)
  ) |>
  # model_stats is a list column, so we need to unnest it to access the model statistics
  unnest(model_stats)

# take a look at the model results
# Interpretation: High R-squared indicate that model fits calibration well.
# significant p-values indicate that true_conc is a strong predictor
calibration_models


# Visualizations ------------------------------------------------------------
# Use geom_smooth to add the linear regression lines
data_sim |>
  ggplot(aes(x = true_conc, y = measured_conc, color = experiment)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Calibration Curves for all drugs",
    x = "True Concentration", y = "Measured Concentration"
  ) +
  theme_minimal() +
  facet_wrap(~drug)
