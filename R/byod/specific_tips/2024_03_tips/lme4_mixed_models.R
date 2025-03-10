library(tidyverse)
library(palmerpenguins)
library(lme4)

# year as a random effect
penguins_model <- lmer(
  body_mass_g ~ species * sex + (1 | year),
  data = penguins)

# Are effects significant?
car::Anova(mod_light_cover)
# test random effects
lmerTest::ranova(mod_light_cover)
# Check model assumptions
performance::check_model(mod_light_cover)
