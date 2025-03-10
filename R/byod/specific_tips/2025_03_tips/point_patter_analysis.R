# References and help
# https://spatstat.org/
# https://book.spatstat.org/

# Load necessary libraries
library(tidyverse)
library(spatstat)

# The example data sets -------------------------------------------------------
# Spatstat includes 3 example data sets of point patterns:
# They are already in a format that spatstat can work with
# The format is caled ppp (point pattern object)
# If you have your data in a different format, you can convert it to ppp
# using the ppp() function

bei # tropical trees showing clustering
redwood # redwood trees showing regularity
japanesepines # Japanese pines showing randomness


# Create a basic plot of the point pattern
# Create a multi-panel plot for all three datasets
plot(bei, main = "bei")
plot(redwood, main = "redwood")
plot(japanesepines, main = "Jap pine")

# Calculate Ripley's K-function ----------------------------------------------

# Calculate and plot K-function for bei
K_bei <- Kest(bei)
# Interpretation: If observed K (solid line) is above the theoretical CSR line,
# points are more clustered than random at that distance.
# So in this case, they are clustered
plot(K_bei, main = "bei data: Ripley's K")

# Calculate and plot K-function for redwood
K_redwood <- Kest(redwood)
# Interpretation: If observed K is below the theoretical line,
# points are more regularly spaced than random (inhibition).
plot(K_redwood, main = "redwood: Ripley's K")

# Calculate and plot K-function for japanesepines
K_jp <- Kest(japanesepines)
# Interpretation: If observed K closely follows the theoretical line,
# the pattern approximates complete spatial randomness.
plot(K_jp, main = "japanesepines: Ripley's K")

# Ripleys pair-cross correlation function -------------------------------------
# Calculate and plot PCF for bei
pcf_bei <- pcf(bei)
# Interpretation: Values > 1 indicate clustering at that distance;
# the higher the peak, the stronger the clustering.
plot(pcf_bei, main = "bei: Pair Correlation")

# Calculate and plot PCF for redwood
pcf_redwood <- pcf(redwood)
# Interpretation: Values < 1 indicate inhibition/regularity at that distance;
# dips below 1 suggest trees avoid being at those distances from each other.
plot(pcf_redwood, main = "redwood: Pair Correlation")

# Calculate and plot PCF for japanesepines
pcf_jp <- pcf(japanesepines)
# Interpretation: Values fluctuating around 1 suggest randomness;
# no strong attraction or repulsion at those distances.
plot(pcf_jp, main = "japanesepines: Pair Correlation")

# Alternative: Make the plots with ggplot -------------------------------------

# Function to convert ppp object to tibble
# Convert all three datasets to tibbles
bei_tbl <- tibble(x = bei$x, y = bei$y)
redwood_tbl <- tibble(x = redwood$x, y = redwood$y)
japanesepines_tbl <- tibble(x = japanesepines$x, y = japanesepines$y)

# Plot one dataset individually
ggplot(bei_tbl, aes(x = x, y = y)) +
  geom_point() +
  labs(title = "bei")

# Combine all three datasets into one tibble
all_tbl <- bind_rows(
  bei = bei_tbl, redwood = redwood_tbl,
  pines = japanesepines_tbl, .id = "dataset"
)

# Plot all three datasets in one plot
ggplot(all_tbl, aes(x = x, y = y)) +
  geom_point() +
  labs(title = "All datasets") +
  # scales = "free" to see the differences better (allows for different axes)
  facet_wrap(~dataset, scales = "free")

# Plot the results from Ripley's K-function with ggplot ------------------------

# We have the three K-functions
K_bei
K_redwood
K_jp

# Convert the K-function objects to tibbles
# look at the objects with the str() function
str(K_bei)
plot(K_bei)

k_bei_tbl <- tibble(
  r = K_bei$r, # Distance
  border = K_bei$bord, # Observed K (isotropic correction)
  theo = K_bei$theo # Theoretical K (CSR)
)

str(K_redwood)
plot(K_redwood)

k_redwood_tbl <- tibble(
  r = K_redwood$r,
  iso = K_redwood$iso,
  trans = K_redwood$trans,
  border = K_redwood$bord,
  theo = K_redwood$theo
)

str(K_jp)
plot(K_jp)

k_jp_tbl <- tibble(
  r = K_jp$r,
  iso = K_j$iso,
  trans = K_j$trans,
  border = K_jp$bord,
  theo = K_jp$theo
)

# Plot just one of them
# of course you can further customize the look
k_redwood_tbl |>
  pivot_longer(-r) |>
  ggplot(aes(x = r, y = value, color = name)) +
  geom_line()

# Combine all 3 in one result table
all_k <- bind_rows(
  bei = k_bei_tbl,
  redwood = k_redwood_tbl,
  japan = k_jp_tbl, .id = "dataset"
)

# Create a ggplot with facets for Ripley's K
# You can make it more pretty if you want
all_k |>
  pivot_longer(-c(r, dataset)) |>
  ggplot(aes(x = r, y = value, color = name)) +
  geom_line() +
  facet_wrap(~dataset, scales = "free") +
  scale_color_manual(
    values = c(
      iso = "black",
      trans = "red",
      border = "green",
      theo = "blue"
    )
  ) +
  labs(
    title = "Ripley's K-function Analysis",
    subtitle = "Observed vs. Theoretical under Complete Spatial Randomness",
    x = "Distance (r)",
    y = "K(r)",
    color = "Legend title"
  )

# Plot pair correlations with ggplot 2 ----------------------------------------

# First we again need to convert the pcf results to a tibble
str(pcf_bei)

pcf_bei_tbl <- tibble(
  r = pcf_bei$r, # Distance
  trans = pcf_bei$trans, # Transformed PCF
  iso = pcf_bei$iso, # Observed PCF (isotropic correction)
  theo = rep(1, length(pcf_bei$r)) # Theoretical PCF (CSR)
)

pcf_redwood_tbl <- tibble(
  r = pcf_redwood$r,
  trans = pcf_redwood$trans,
  iso = pcf_redwood$iso,
  theo = rep(1, length(pcf_redwood$r))
)

pcf_jp_tbl <- tibble(
  r = pcf_jp$r,
  trans = pcf_jp$trans,
  iso = pcf_jp$iso,
  theo = rep(1, length(pcf_jp$r))
)

# example plot for one dataset
pcf_bei_tbl |>
  pivot_longer(-r) |>
  ggplot(aes(x = r, y = value, color = name)) +
  geom_line()

# Combine all PCF results into one table
all_pcf <- bind_rows(
  bei = pcf_bei_tbl, redwood = pcf_redwood_tbl,
  japan = pcf_jp_tbl, .id = "dataset"
)

# Create a ggplot with facets for PCF
all_pcf |>
  pivot_longer(-c(r, dataset)) |>
  ggplot(aes(x = r, color = name, y = value)) +
  geom_line() +
  facet_wrap(~dataset, scales = "free") +
  theme_minimal() +
  labs(
    title = "Pair Correlation Function Analysis",
    subtitle = "Values > 1 indicate clustering, values < 1 indicate regularity",
    x = "Distance (r)",
    y = "g(r)",
    color = "Legend"
  )
