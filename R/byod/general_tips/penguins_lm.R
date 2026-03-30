library(tidyverse)


# Example 1: Simple linear regression -----------------------------------------
# Research question: Does flipper length predict body mass?

# Fit the model
# lm(response ~ predictor, data = ...)
m1 <- lm(body_mass ~ flipper_len, data = penguins)

# Model summary: Shows coefficients, R2,...
# Intercept: predicted body mass when flipper_len = 0
# flipper_len coefficient: change in body mass (g) per 1 mm increase in flipper length
# (i.e. slope of the linear regression)
summary(m1)

# Test significance of each predictor using a likelihood-ratio-based approach.
# drop1() compares the full model to a model with each term dropped.
# For simple regression this matches the summary p-value, but it is useful
# for more complex models.
drop1(m1, test = "F")

# Check model assumptions:
# I like this function from the performance pkg becauese it gives a nice overview of
# what to look out for
# - Linearity & homoscedasticity (residuals vs. fitted)
# - Normality of residuals (Q-Q plot)
# - Influential observations (Cook's distance)
# install.packages("performance")
performance::check_model(m1)

# Visualize the fitted relationship with ggplot
ggplot(penguins, aes(x = flipper_len, y = body_mass)) +
  geom_point(alpha = 0.5) +
  # geom_smooth with method = "lm" draws the regression line
  geom_smooth(method = "lm") +
  labs(
    x = "Flipper length (mm)",
    y = "Body mass (g)",
  ) +
  theme_minimal()

# Example 2: Multiple regression with a categorical predictor -----------------
# Research question: Does flipper length predict body mass, and does this
# relationship differ by species?

# Adding species as a predictor estimates a separate intercept per species
# (i.e., it shifts the regression line up or down for each species).
m2 <- lm(body_mass ~ flipper_len + species, data = penguins)

# The summary now shows:
# - flipper_len: slope (shared across species)
# - speciesChinstrap, speciesGentoo: intercept offsets relative to Adelie
#   (the reference level, shown by the intercept)
summary(m2)

# drop1() tests the contribution of each predictor while keeping the others.
# Here it tells us whether species improves the model beyond flipper length alone.
drop1(m2, test = "F")

# Check model assumptions for the extended model
# The red colinearity is fine.
performance::check_model(m2)

# Visualize: one regression line per species
# NOTE: when color = species is in aes(), geom_smooth fits a *separate slope*
# per species. This does NOT match m2, which constrains all species to the same
# slope! The plot is therefore slightly misleading for m2. To model different
# slopes per species, add an interaction term, see Example 3 below.
ggplot(penguins, aes(x = flipper_len, y = body_mass, color = species)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  scale_color_brewer(palette = "Dark2") +
  labs(
    x = "Flipper length (mm)",
    y = "Body mass (g)",
    color = "Species",
  ) +
  theme_minimal()

# Example 3: Interaction between a continuous and a categorical predictor ------
# Research question: Does the effect of flipper length on body mass differ
# between species (i.e. do species have different slopes)?

# The * operator adds both main effects AND their interaction.
# flipper_len:species is the interaction term — it allows the slope of
# flipper_len to differ per species.
m3 <- lm(body_mass ~ flipper_len * species, data = penguins)

# The summary now shows:
# - (Intercept): Intercept for the reference species (Adelie)
# - flipper_len: slope for the reference species (Adelie)
# - speciesChinstrap, speciesGentoo: intercept offsets relative to Adelie
# - flipper_len:speciesChinstrap, flipper_len:speciesGentoo: how much the slope
#   of flipper_len differs from Adelie's slope for each species
summary(m3)

# drop1() now tests whether the interaction term as a whole is significant,
# i.e. whether allowing different slopes improves the model.
# Here it is significant, suggesting that the relationship between flipper length
# and body mass differs by species.
drop1(m3, test = "F")

# Check model assumptions
performance::check_model(m3)

# The geom_smooth plot now correctly represents the model:
# each species gets its own slope AND intercept.
ggplot(penguins, aes(x = flipper_len, y = body_mass, color = species)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  scale_color_brewer(palette = "Dark2") +
  labs(
    x = "Flipper length (mm)",
    y = "Body mass (g)",
    color = "Species",
  ) +
  theme_minimal()
