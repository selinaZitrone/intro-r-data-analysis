# Purpose: Compare the binding of different sera to glycans with a
# reference serum

library(tidyverse)
library(patchwork) # to combine the plots into one figure
library(ggsignif) # to add the test results to the plot

# Create dummy data _---------------------------------------------------------

# This step creates dummy data I use for the analysis. This would be replaced
# by reading in your own data. if your data is in multiple csv files, check out
# read_and_combine_tables.R to read them all into one table

set.seed(1)

binding <- expand_grid(
  serum = c("reference", "serum_A", "serum_B"),
  glycan = paste0("glycan_", 1:6)
) |>
  mutate(strength = if_else(serum == "reference", 1, runif(n(), 0.1, 1.5)))

mfi <- expand_grid(
  binding,
  dilution = c(50, 100, 200),
  replicate = 1:3
) |>
  mutate(
    MFI = round(100 + strength * 2e5 / dilution * rlnorm(n(), sdlog = 0.15))
  ) |>
  select(-strength)

# The dummy data looks like this (i hope this kind of fits with your dat astructure):
# - serum: name of the serum (reference, serum_A and serum_B)
# - glycan: name of the glycan (glycan_1-6)
# - dilution: dilution of the serum (50,100,200)
# - replicate: replicate number (1-3)
# - MFI
mfi

# Normalization ---------------------------------------------------------------

mfi <- mfi |>
  mutate(log2_mfi = log2(MFI))

# Mean of the replicates
mfi_mean <- mfi |>
  summarise(
    mean_mfi = mean(MFI),
    mean_log2_mfi = mean(log2_mfi),
    .by = c(serum, glycan, dilution)
  )

# log2 fold change compared to the reference serum (same glycan and
# dilution):

# Step 1: create a subset for only the reference
reference <- mfi_mean |>
  filter(serum == "reference") |>
  # Select only necessary columns and rename mean_log2_mfi to ref_log2_mfi
  select(glycan, dilution, ref_log2_mfi = mean_log2_mfi)

# Step 2: Join the reference with all data points
mfi_mean <- mfi_mean |>
  left_join(reference, by = c("glycan", "dilution"))
# This gives use one table with a column for the mean_log2_mfi for all serums and
# one column for the reference
mfi_mean

# Step 3: Calculate log2FC
mfi_mean <- mfi_mean |>
  mutate(log2_fc = mean_log2_mfi - ref_log2_mfi)

mfi_mean


# Line plot: dilution curves --------------------------------------------------

# One panel per glycan, one line per serum.
# Both axes on a log scale
plot_lines <- ggplot(
  mfi_mean,
  aes(x = dilution, y = mean_mfi, color = serum)
) +
  geom_line() +
  geom_point() +
  facet_wrap(vars(glycan)) +
  scale_x_log10() +
  scale_y_log10() +
  labs(x = "Dilution (1:x)", y = "MFI", color = "Serum") +
  theme_minimal()
plot_lines

# Heatmap: log2 fold change ---------------------------------------------------

mfi_mean |>
  # remove the reference from the plot
  filter(serum != "reference") |>
  ggplot(aes(x = serum, y = glycan, fill = log2_fc)) +
  geom_tile() +
  # white = like the reference, blue = weaker, red = stronger binding
  scale_fill_gradient2(low = "steelblue", mid = "white", high = "firebrick") +
  labs(x = "Serum", y = "Glycan", fill = "log2 FC") +
  facet_wrap(~dilution)

# Bar plot -------------------------------------------------------------------
ggplot(mfi, aes(x = glycan, y = MFI, fill = serum)) +
  stat_summary(fun = mean, geom = "col", position = "dodge") +
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    position = position_dodge(width = 0.9),
    width = 0.2
  ) +
  facet_wrap(~dilution, ncol = 1)


# Tests against the reference -------------------------------------------------

# Welch's t-test for each glycan and serum against the reference, using the
# log2 MFI of the replicates (The dummy data only has 3 replicates so the test
# results don't mean too much here)

# Step 1: Filter out the replicates of the reference
reference_replicates <- mfi |>
  filter(serum == "reference") |>
  select(glycan, dilution, replicate, ref_log2_mfi = log2_mfi)

# Step 2: Join them with the table with all the other serums
test_data <- mfi |>
  filter(serum != "reference") |>
  left_join(reference_replicates, by = c("glycan", "dilution", "replicate"))
test_data

# Step 3: Test each glycan, serum and dilution against the reference
tests <- test_data |>
  summarise(
    # this runs a t-test for each group and extracts the p-value
    p_value = t.test(log2_mfi, ref_log2_mfi)$p.value,
    .by = c(glycan, serum, dilution)
  ) |>
  # Because there are many tests, the p-values are adjusted just like they would be
  # in the pairwise.t.test() function
  mutate(p_adjusted = p.adjust(p_value, method = "holm"))
tests

# Combine the plots -----------------------------------------------------------

# Plot 1: Barplot with geom signif
# Plot 2: lineplot

# Both plots for one glycan and dilution
glycan_plot <- "glycan_4"
dilution_plot <- 100

# Adjusted p-values of this glycan and dilution from the tests above
p_labels <- tests |>
  filter(glycan == glycan_plot, dilution == dilution_plot) |>
  arrange(serum) |>
  mutate(label = paste("p =", signif(p_adjusted, 2)))
p_labels

# Plot 1: mean +- standard error with the p-values of the tests
plot_signif <- mfi |>
  filter(glycan == glycan_plot, dilution == dilution_plot) |>
  ggplot(aes(x = serum, y = MFI)) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.15) +
  stat_summary(fun = mean, geom = "bar", fill = "grey70") +
  geom_signif(
    # same order as in p_labels (serum_A, serum_B)
    comparisons = list(c("reference", "serum_A"), c("reference", "serum_B")),
    annotations = p_labels$label,
    step_increase = 0.1
  )

# Plot 2: dilution curve
plot_curve <- mfi_mean |>
  filter(glycan == glycan_plot) |>
  ggplot(aes(x = dilution, y = mean_mfi, color = serum)) +
  geom_line() +
  geom_point()

plot_signif + plot_curve + plot_annotation(tag_levels = "A")
