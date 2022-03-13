library(tidyverse)
library(palmerpenguins)
library(performance)
# Statistical tests -------------------------------------------------------

# subset for 3 species flipper length

adelie <- filter(penguins, species == "Adelie") %>% pull(flipper_length_mm)
chinstrap <- filter(penguins, species == "Chinstrap") %>% pull(flipper_length_mm)
gentoo <- filter(penguins, species == "Gentoo") %>% pull(flipper_length_mm)

# Step 1: test for normality

# Visual test using QQ-Plots

# use patchwork package to combine the three plots into one
library(patchwork)
a <- ggpubr::ggqqplot(adelie) + labs(title = "adelie")
c <- ggpubr::ggqqplot(chinstrap) + labs(title = "chinstrap")
g <- ggpubr::ggqqplot(gentoo) + labs(title = "gentoo")

a + c + g

# Other option
penguins %>%
  ggplot(aes(sample = flipper_length_mm, color = species))+
  stat_qq()+
  stat_qq_line()

# Test normality with Shapiro-Wilk test

shapiro.test(adelie) # normal
shapiro.test(chinstrap) # normal
shapiro.test(gentoo) # not normal

# Comparison Chinstrap vs. Adelie
# both normal so test for equal variance
var.test(chinstrap, adelie) # variances do not differ
# t-test to test for equal means
t.test(chinstrap, adelie) # means differ

# Comparison Chinstrap vs. Gentoo
# Gentoo not normal: use Wilcoxon-rank-sum test to compare means
wilcox.test(chinstrap, gentoo) # means differ

# Comparison Gentoo- Adelie
# Gentoo not normal: use Wilcoxon-rank-sum test to compare means
wilcox.test(gentoo, adelie) # means differ


# Compare flipper lengths of penguins visually using a boxplot
penguins %>%
  ggplot(aes(x = species, y = flipper_length_mm)) +
  geom_boxplot(notch = TRUE)

# Linear models -----------------------------------------------------------
library(tidyverse)
library(palmerpenguins)
library(performance)

### 1.2 Linear regression: bill depth depends on bill length --------------
# make a plot
g <- ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point()
g

# fit the model
lm1 <- lm(bill_depth_mm ~ bill_length_mm, data = penguins)
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
  bill_length_mm = seq(
    from = min(penguins$bill_length_mm, na.rm = TRUE),
    to = max(penguins$bill_length_mm, na.rm = TRUE)
  )
)
# Predict the response and add it to pred_data
pred_dat$bill_depth_mm <- predict(lm1, newdata = pred_dat)
# Add a line with the new predicted data
g +
  geom_line(data = pred_dat, color = "cyan4")

# predict a single value
predict(lm1, newdata = tibble(bill_length_mm = 200))

### 1.3 Analysis of covariance: bill depth depends on bill length and species --------------------------

# make a plot
g2 <- ggplot(penguins, aes(x = bill_length_mm,
                           y = bill_depth_mm,
                           color = species)) +
  geom_point()
g2

# fit model
# Without interaction
lm2a <- lm(bill_depth_mm ~ bill_length_mm + species, data = penguins)
# With interaction
lm2b <- lm(bill_depth_mm ~ bill_length_mm * species, data = penguins)
lm2b <- lm(bill_depth_mm ~ bill_length_mm +
             species + bill_length_mm:species, data = penguins)


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
  bill_length_mm = min(penguins$bill_length_mm, na.rm = TRUE):max(penguins$bill_length_mm, na.rm = TRUE),
  species = c("Adelie", "Chinstrap", "Gentoo")
)
pred_dat$bill_depth_mm <- predict(lm2a, newdata = pred_dat)

g2 +
  geom_line(data = pred_dat) +
  ggsci::scale_color_futurama()

### 1.4 Anova: weight of penguins depend on sex and species ------------

# remove missing sex
penguins_sex <- filter(penguins, !is.na(sex))

# make a plot
ggplot(penguins_sex, aes(x = species, y = body_mass_g, fill = sex)) +
  geom_boxplot(notch = TRUE)

# Fit the model
# With interaction
lm3 <- lm(body_mass_g ~ sex + species + sex:species, data = penguins_sex)

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
    y = body_mass_g,
    color = sex
  )) +
  geom_boxplot() +
  geom_point(
    size = 2, alpha = 0.5,
    position = position_jitterdodge(
      seed = 123
    )
  ) +
  coord_flip() +
  ggsci::scale_color_d3() +
  labs(y = "Body mass [g]", x = "Species") +
  theme_bw() +
  theme(legend.position = c(0.85, 0.15))

summary <- penguins_sex %>%
  group_by(species, sex) %>%
  summarize(
    sd = sd(body_mass_g),
    mean = mean(body_mass_g)
  )

ggplot(summary, aes(x = species, y = mean, color = sex)) +
  geom_point(size = 3, position = position_dodge(0.4)) +
  geom_errorbar(
    aes(ymin = mean - sd, ymax = mean + sd),
    position = position_dodge(0.4),
    width = 0.2
  ) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  labs(y = "Body mass [g]")


ggplot(summary, aes(x = species, y = mean, fill = sex)) +
  geom_col(position = position_dodge(1)) +
  geom_errorbar(
    aes(ymin = mean - sd, ymax = mean + sd),
    position = position_dodge(1),
    width = 0.2
  ) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
  labs(y = "Body mass [g]")
