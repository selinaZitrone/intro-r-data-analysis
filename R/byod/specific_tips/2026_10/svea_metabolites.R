# Purpose: Calculate pharmacokinetic parameters (Cmax, Tmax, AUC) from
# concentration-time curves and compare them
# 1. between 2 groups for application 1 (automatically chosen test)
# 2. between 3 applications in group B (linear mixed model)

library(tidyverse)
library(lmerTest) # mixed models (lme4) with p-values
library(emmeans) # pairwise comparisons from a mixed model

# Create dummy data (skip this part with your own data) -----------------------

# You can just run this, no need to understand the details of the dummy data
# creation. The dummy data follows your study design
set.seed(1)

curves <- tibble(
  participant = sprintf("P%02d", 1:20),
  group = rep(c("A", "B"), each = 10),
  level = rnorm(20, sd = 0.2) # each participant has their own level
) |>
  expand_grid(application = c("app1", "app2", "app3")) |>
  filter(group == "B" | application == "app1") |>
  mutate(
    ka = 1.2 * if_else(application == "app2", 2, 1) * exp(rnorm(n(), sd = 0.2)),
    ke = 0.25 * exp(rnorm(n(), sd = 0.15)),
    dose = 100 *
      if_else(group == "B", 1.3, 1) * # group B: 30% higher exposure
      if_else(application == "app3", 0.7, 1) *
      exp(level + rnorm(n(), sd = 0.1))
  )

conc <- curves |>
  expand_grid(time = c(0, 0.5, 1, 2, 4, 6, 8, 12)) |>
  mutate(
    concentration = dose * ka / (ka - ke) * (exp(-ke * time) - exp(-ka * time)),
    concentration = round(concentration * exp(rnorm(n(), sd = 0.05)), 2)
  ) |>
  select(participant, group, application, time, concentration)

# This is what the dummy data looks like:
# - participant: participant ID
# - group: A (only application 1) or B (applications 1, 2 and 3)
# - application: app1, app2, app3
# - time: time after administration
# - concentration: concentration of the analyte
conc

# Look at the curves
ggplot(conc, aes(x = time, y = concentration, group = participant)) +
  geom_line(alpha = 0.5) +
  facet_grid(group ~ application)

# Calculate PK parameters -----------------------------------------------------

# One row per curve (participant and application)
# I use PKNCA package to calculate auc here (install it once with
# install.packages("PKNCA"), https://github.com/humanpred/pknca/)
pk <- conc |>
  arrange(time) |>
  summarise(
    cmax = max(concentration), # maximum concentration
    tmax = time[which.max(concentration)], # time of maximum concentration
    auc = PKNCA::pk.calc.auc.last(concentration, time), # area under the curve
    .by = c(participant, group, application)
  )
pk

# Convert to long format
pk_long <- pk |>
  pivot_longer(c(cmax, tmax, auc), names_to = "parameter", values_to = "value")
pk_long

# Part 1: Group A vs. B: automatic choice of test ------------------------------

# Automatic choice of the test for each parameter:
# - not normal (Shapiro-Wilk) -> Wilcoxon test
# - normal and equal variances (Levene) -> Student's t-test
# - normal and unequal variances -> Welch's t-test

# Filter only for the first application
app1 <- pk_long |>
  filter(application == "app1") |>
  # Here you can consider log-transforming auc and cmax. I think they could be
  # log-normally distributed and maybe log-transforming them would be appropriate
  # Note that this changes the test results (probably more are normal because of
  # the log transformation). If you don't want to log transform, just remove this
  # line
  mutate(value = if_else(parameter == "tmax", value, log(value)))

# Step 1: Shapiro-Wilk test for each parameter and group. I count a parameter as
# normal if the test is not significant in both groups
normality <- app1 |>
  summarise(
    # run the shapiro test and extract the p-value
    shapiro_p = shapiro.test(value)$p.value,
    .by = c(parameter, group)
  ) |>
  summarise(normal = all(shapiro_p > 0.05), .by = parameter)
normality

# Step 2: Levene test for equal variances for each parameter
variances <- app1 |>
  summarise(
    # for the levene test it's a bit more complicated to extract the p-value
    levene_p = car::leveneTest(value, factor(group))$`Pr(>F)`[1],
    .by = parameter
  ) |>
  mutate(equal_var = levene_p > 0.05)
variances

# Step 3: Run all 3 tests (Student's, Welch's t and Wilcoxon)
all_tests <- app1 |>
  summarise(
    p_student = t.test(value ~ group, var.equal = TRUE)$p.value,
    p_welch = t.test(value ~ group)$p.value,
    p_wilcox = wilcox.test(value ~ group, exact = FALSE)$p.value,
    .by = parameter
  )
all_tests

# Step 4: Add the results of the normality and variance tests from step 1 and 2
# and choose the test (and its p-value) that fits
test_results <- all_tests |>
  left_join(normality, by = "parameter") |>
  left_join(variances, by = "parameter") |>
  mutate(
    test = case_when(
      !normal ~ "Wilcoxon",
      equal_var ~ "Student",
      .default = "Welch"
    ),
    p_value = case_when(
      test == "Wilcoxon" ~ p_wilcox,
      test == "Student" ~ p_student,
      .default = p_welch
    )
  )
test_results

# Step 5: Correct the p-values for multiple testing (one test per parameter).
# Not sure if you need this. If Cmax, Tmax and AUC are separate tests maybe you don't need
# to adjust the p-values for multiple testing here
test_results <- test_results |>
  mutate(p_adjusted = p.adjust(p_value, method = "holm"))
test_results |>
  select(parameter, normal, equal_var, test, p_value, p_adjusted)

# Part 2: Applications 1, 2 and 3 in group B (LME) --------------------

# The same participants had all 3 applications, so the measurements are not
# independent. The random effect (1 | participant) gives each participant their
# own level (random intercept)
# What to pay attention to:
# - Cmax and AUC could be log-normally distributed, so you might have to use log(value)
# - Not sure if you can fit a mixed model for Tmax because Tmax can only take few values
# - With few participants, keep the model simple (only a random intercept)
# - Check the model assumptions (residuals) with performance::check_model()

group_b <- pk_long |>
  filter(group == "B")

# Mixed model for Cmax
model_cmax <- lmer(
  log(value) ~ application + (1 | participant),
  data = filter(group_b, parameter == "cmax")
)

# Is there a difference between the applications?
anova(model_cmax)

# Which applications differ? Pairwise comparisons (p-values are adjusted).
# type = "response" gives the results as ratios
emmeans(model_cmax, pairwise ~ application, type = "response")

# Check the model assumptions
# The plots tell you what to look for in the residual plots. But in general
# assumptions of a linear model are:
# - residuals are normally distributed
# - variance is constant
performance::check_model(model_cmax)

# Same for AUC
model_auc <- lmer(
  log(value) ~ application + (1 | participant),
  data = filter(group_b, parameter == "auc")
)
anova(model_auc)
emmeans(model_auc, pairwise ~ application, type = "response")

# Plot the results ------------------------------------------------------------

# Estimated Cmax per application with 95% CI from the model, back-transformed
# from the log scale to the original scale
cmax_means <- emmeans(model_cmax, ~application, type = "response")
cmax_means

# Option 1: ggplot with one grey line per participant and the model estimates
cmax_means_table <- as_tibble(cmax_means)

ggplot() +
  geom_line(
    data = filter(group_b, parameter == "cmax"),
    aes(x = application, y = value, group = participant),
    color = "grey70"
  ) +
  geom_pointrange(
    data = cmax_means_table,
    aes(x = application, y = response, ymin = lower.CL, ymax = upper.CL),
    color = "#28a87d"
  ) +
  labs(x = "Application", y = "Cmax") +
  theme_minimal()

# Option 2: quick plot of the estimates with the emmeans package
plot(cmax_means)

# Option 3: estimates and measured values with the ggeffects package
plot(
  ggeffects::predict_response(model_cmax, terms = "application"),
  show_data = TRUE
)

# More info on mixed models: https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html
