library(tidyverse)
library(palmerpenguins)
library(performance)
# Statistical tests -------------------------------------------------------

# subset for 3 species flipper length
adelie <- filter(penguins, species == "Adelie")$flipper_length_mm
chinstrap <- filter(penguins, species == "Chinstrap")$flipper_length_mm
gentoo <- filter(penguins, species == "Gentoo")$flipper_length_mm

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
t.test(chinstrap, adelie, var.equal = TRUE) # means differ

# Comparison Chinstrap vs. Gentoo
# Gentoo not normal: use Wilcoxon-rank-sum test to compare means
wilcox.test(chinstrap, gentoo) # means differ

# Comparison Gentoo- Adelie
# Gentoo not normal: use Wilcoxon-rank-sum test to compare means
wilcox.test(gentoo, adelie) # means differ


# Compare flipper lengths of penguins visually using a boxplot
# Add the p-values of the comparisons with the test into the plot
library(ggsignif)
penguins %>%
  ggplot(aes(x = species, y = flipper_length_mm)) +
  geom_boxplot(notch = TRUE) +
  geom_signif(
    comparisons = list(
      c("Chinstrap", "Adelie")
    ),
    test = "t.test",
    test.args = list(var.equal = TRUE),
    map_signif_level = TRUE
  ) +
  geom_signif(
    comparisons = list(
      c("Chinstrap", "Gentoo"),
      c("Gentoo", "Adelie")
    ),
    test = "wilcox.test",
    y_position = c(235,240),
    map_signif_level = TRUE
  )

# Plot with mean and errorbars:
penguins %>%
  ggplot(aes(x = species, y = flipper_length_mm)) +
  stat_summary() +
  geom_signif(
    comparisons = list(
      c("Chinstrap", "Adelie")
    ),
    test = "t.test",
    test.args = list(var.equal = TRUE),
    map_signif_level = TRUE,
    y_position = 200,
    tip_length = 0.01
  ) +
  geom_signif(
    comparisons = list(
      c("Chinstrap", "Gentoo"),
      c("Gentoo", "Adelie")
    ),
    test = "wilcox.test",
    y_position = c(216,217),
    map_signif_level = TRUE,
    tip_length = 0.01
  )

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

ggplot(penguins_sex, aes(x = species, y = body_mass_g, color = sex)) +
  stat_summary(
    position = position_dodge(width = 0.5)
  ) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  labs(y = "Body mass [g]")

ggplot(penguins_sex, aes(x = species, y = body_mass_g, fill = sex)) +
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


lm2a <- lm(bill_depth_mm ~ bill_length_mm + species, data = penguins)
# With interaction
lm2b <- lm(bill_depth_mm ~ bill_length_mm * species, data = penguins)


library(jtools)

summ(lm2b, confint = TRUE)

effect_plot(lm2b, pred = species)
effect_plot(lm2b, pred = bill_length_mm)

plot_summs(lm2a, lm2b)

library(sjPlot)
# plot the results from the summary table
plot_model(lm2a)
plot_model(lm2a, show.values = TRUE)
