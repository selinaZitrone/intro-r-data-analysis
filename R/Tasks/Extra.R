# Linear models -----------------------------------------------------------
library(tidyverse)

library(performance)

### 1.2 Linear regression: bill depth depends on bill length --------------
# make a plot
g <- ggplot(penguins, aes(x = bill_len, y = bill_dep)) +
  geom_point()
g

# fit the model
lm1 <- lm(bill_dep ~ bill_len, data = penguins)
drop1(lm1, test = "F") # bill length significant

# check assumptions
performance::check_model(lm1) # do not look perfect but okay -> could they be better?

# Add model to plot
# Option 1: geom_smooth
g +
  geom_smooth(method = "lm", se = FALSE)

# Option 2: extract coefficients
intercept <- lm1$coefficients[1]
slope <- lm1$coefficients[2]
g +
  geom_abline(slope = slope, intercept = intercept)

# Option 3: using predict (this is a bit overkill for this simple example)
# Create data to predict from
pred_dat <- tibble(
  bill_len = seq(
    from = min(penguins$bill_len, na.rm = TRUE),
    to = max(penguins$bill_len, na.rm = TRUE)
  )
)
# Predict the response and add it to pred_data
pred_dat$bill_dep <- predict(lm1, newdata = pred_dat)
# Add a line with the new predicted data
g +
  geom_line(data = pred_dat, color = "cyan4")

# predict a single value
predict(lm1, newdata = tibble(bill_len = 200))

### 1.3 Analysis of covariance: bill depth depends on bill length and species --------------------------

# make a plot
g2 <- ggplot(
  penguins,
  aes(x = bill_len, y = bill_dep, color = species)
) +
  geom_point()
g2

# fit model
# Without interaction
lm2a <- lm(bill_dep ~ bill_len + species, data = penguins)
# With interaction
lm2b <- lm(bill_dep ~ bill_len * species, data = penguins)

lm2b <- lm(
  bill_dep ~
    bill_len +
      species +
      bill_len:species,
  data = penguins
)


# test assumptions
check_model(lm2a) # looks good, better than without species
check_model(lm2b) # also looks good

# test significant effects
drop1(lm2a, test = "F") # bill length and species both significant
drop1(lm2b, test = "F") # interaction not significant

# add regression lines to the plot
# Option 1: regression lines with interaction
g2 +
  geom_smooth(method = "lm", se = FALSE)

# Option 2: extract model coefficients
intercept_adelie <- lm2a$coefficients[1]
slope <- lm2a$coefficients[2]
intercept_chinstrap <- intercept_adelie + lm2a$coefficients[3]
intercept_gentoo <- intercept_adelie + lm2a$coefficients[4]

# lets add some custom colors
cols <- c("darkorange", "cyan4", "purple")

g2 +
  geom_abline(slope = slope, intercept = intercept_adelie, color = cols[1]) +
  geom_abline(slope = slope, intercept = intercept_chinstrap, color = cols[2]) +
  geom_abline(slope = slope, intercept = intercept_gentoo, color = cols[3]) +
  scale_color_manual(values = cols)

# Option 3: predict
pred_dat <- expand_grid(
  bill_len = min(penguins$bill_len, na.rm = TRUE):max(
    penguins$bill_len,
    na.rm = TRUE
  ),
  species = c("Adelie", "Chinstrap", "Gentoo")
)
pred_dat$bill_dep <- predict(lm2a, newdata = pred_dat)

g2 +
  geom_line(data = pred_dat) +
  ggsci::scale_color_futurama()

### 1.4 Anova: weight of penguins depend on sex and species ------------

# remove missing sex
penguins_sex <- filter(penguins, !is.na(sex))

# make a plot
ggplot(penguins_sex, aes(x = species, y = body_mass, fill = sex)) +
  geom_boxplot(notch = TRUE)

# Fit the model
# With interaction
lm3 <- lm(body_mass ~ sex + species + sex:species, data = penguins_sex)

# Test significant effects
drop1(lm3, test = "F") # significant interaction

# check assumptions
check_model(lm3) # looks good

# Post-hoc TukeyHSD
TukeyHSD(aov(lm3))
plot(TukeyHSD(aov(lm3)))

#### Extra

penguins_sex %>%
  ggplot(aes(
    x = species,
    y = body_mass,
    color = sex
  )) +
  geom_boxplot() +
  geom_point(
    size = 2,
    alpha = 0.5,
    position = position_jitterdodge(
      seed = 123
    )
  ) +
  coord_flip() +
  ggsci::scale_color_d3() +
  labs(y = "Body mass [g]", x = "Species") +
  theme_bw() +
  theme(legend.position = c(0.85, 0.15))

ggplot(penguins_sex, aes(x = species, y = body_mass, color = sex)) +
  stat_summary(
    position = position_dodge(width = 0.5)
  ) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  labs(y = "Body mass [g]")

ggplot(penguins_sex, aes(x = species, y = body_mass, fill = sex)) +
  stat_summary(
    fun.y = mean,
    geom = "bar",
    width = 0.7,
    position = position_dodge(width = 0.8)
  ) +
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    position = position_dodge(width = 0.8),
    width = 0.3
  ) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
  labs(y = "Body mass [g]")


lm2a <- lm(bill_dep ~ bill_len + species, data = penguins)
# With interaction
lm2b <- lm(bill_dep ~ bill_len * species, data = penguins)
