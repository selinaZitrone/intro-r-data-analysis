# Links
# https://eeeeeric.com/rSalvador/
# https://github.com/eeeeeric/rSalvador
# Papers:
# R package: https://academic.oup.com/g3journal/article/7/12/3849/6027424?login=false
# A new practical guide to the Luria–Delbrück protocol: https://www.sciencedirect.com/science/article/pii/S0027510715300348

# Install the package
devtools::install_github("eeeeeric/rSalvador", subdir = "rsalvador")

library(rsalvador)
library(tidyverse)
library(scales) # If you want to format axis labels (e.g., scientific notation)

# Look at some data included in rsalvador
demerec.data
luria.16.data

# -----------------------------------------------------------------------------#
# Example workflow with just one data set ------------------------------------
# ----------------------------------------------------------------------------#

luria.16.data

# Visualize the distribution of mutant counts
# Example demerec data
tibble(mutants = demerec.data) |>
  ggplot(aes(x = mutants)) +
  geom_histogram() +
  labs(
    title = "Mutant counts per culture (Demerec dataset)",
    x = "Mutants per culture",
    y = "Number of cultures"
  ) +
  theme_minimal()

# Fit Luria–Delbrück MSS-ML (full plating assumed)
# For partial plating, see newton.LD.plating()
m_single <- newton.LD(data = demerec.data)
ci_m_single <- confint.LD(data = demerec.data)

# Convert m to mutation rate μ = m / Nt.
# ADJUST: For demo, assume Nt (final viable cells per culture). Replace with your measured Nt.
assumed_Nt_single <- 1e9
mu_single <- m_single / assumed_Nt_single
mu_ci_single <- ci_m_single / assumed_Nt_single

# Visualize μ with CI
mu_single_df <- tibble(
  mu = mu_single,
  mu_lower = mu_ci_single[1],
  mu_upper = mu_ci_single[2]
)

# Plot mutation rate with error bars
ggplot(mu_single_df, aes(x = "a", y = mu)) +
  geom_point(size = 4, color = "#1b9e77") +
  geom_errorbar(
    aes(ymin = mu_lower, ymax = mu_upper),
    width = 0.1,
    color = "#1b9e77",
    linewidth = 1
  ) +
  # this is from the scales package to format y-axis in scientific notation
  scale_y_continuous(labels = scientific) +
  labs(
    title = "Estimated mutation rate μ",
    subtitle = paste0(
      "Assuming Nt = ",
      format(assumed_Nt_single, scientific = TRUE),
      " cells per culture"
    ),
    y = "μ (mutations per cell division)"
  ) +
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank(),
    panel.grid.minor = element_blank()
  )

# -----------------------------------------------------------------------------#
# Example workflow with just two conditions -----------------------------------
# ----------------------------------------------------------------------------#

# I take the two example datasets included in rsalvador
# and pretend they are two conditions in an experiment.
demerec.data
luria.16.data

# Assemble both datasets in a tibble
ld_two <- bind_rows(
  tibble(condition = "Luria.16", mutants = luria.16.data),
  tibble(condition = "Demerec", mutants = demerec.data)
)
ld_two

# Plot the distributions of mutant counts
ld_two |>
  ggplot(aes(x = mutants)) +
  geom_histogram(position = "identity") +
  facet_wrap(vars(condition)) +
  labs(
    title = "Mutant counts per culture (example datasets)",
    x = "Mutants per culture",
    y = "Number of cultures"
  ) +
  theme_bw()

# Fit per condition and compute m, CI, and μ
assumed_Nt <- 1e9 # ADJUST: Replace with your measured Nt.

summ_two <- ld_two |>
  summarize(
    m = newton.LD(data = mutants),
    m_lower = confint.LD(data = mutants)[1],
    m_upper = confint.LD(data = mutants)[2],
    .by = condition
  )

summ_two

# Add mutation rates
summ_two <- summ_two |>
  mutate(
    mu = m / assumed_Nt,
    mu_lower = m_lower / assumed_Nt,
    mu_upper = m_upper / assumed_Nt
  )

# Plot mutation rates with CIs
ggplot(summ_two, aes(x = condition, y = mu)) +
  geom_point(size = 4, color = "#1b9e77") +
  geom_errorbar(
    aes(ymin = mu_lower, ymax = mu_upper),
    width = 0.1,
    color = "#1b9e77",
    linewidth = 1
  ) +
  # this is from the scales package to format y-axis in scientific notation
  scale_y_continuous(labels = scientific) +
  labs(
    title = "Estimated mutation rates by condition",
    subtitle = paste0(
      "Assuming Nt = ",
      format(assumed_Nt, scientific = TRUE),
      " cells per culture"
    ),
    x = "Condition",
    y = "μ (mutations per cell division)"
  ) +
  theme_minimal()
