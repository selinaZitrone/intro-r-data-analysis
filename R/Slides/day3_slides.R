# Code for slides from day 3
# Author: Selina Baldauf (selina.baldauf@fu-berlin.de)
# Hint use the document outline (button top right next to source) to navigate the script

library(tidyverse)
# 1. statistical tests ----------------------------------------------------

# create some dummy data

set.seed(123)
mydata <- tibble(
  normal = rnorm(
    n = 200,
    mean = 50,
    sd = 5
  ),
  non_normal = runif(
    n = 200,
    min = 45,
    max = 55
  )
)

# histogram
mydata %>%
  pivot_longer(cols = 1:2, names_to = "type", values_to = "value") %>%
  ggplot() +
  geom_histogram(alpha = 0.5, aes(x = value, fill = type, y = ..density..)) + # plot probability instead of count on y axis
  stat_function(
    fun = dnorm,
    args = list(
      mean = 50,
      sd = 5
    ),
    color = "darkorange", size = 1
  ) +
  stat_function(
    fun = dnorm,
    args = list(
      mean = mean(mydata$non_normal),
      sd = sd(mydata$non_normal)
    ),
    color = "cyan4", size = 1
  ) +
  scale_fill_manual(values = c("cyan4", "darkorange")) +
  theme(legend.position = c(0.85, 0.85))



# Normality tests ---------------------------------------------------------

# KS-Test -----------------------------------------------------------------

ks.test(mydata$normal,
        "pnorm",
        mean = mean(mydata$normal),
        sd = sd(mydata$normal)
) #normal

ks.test(mydata$non_normal,
        "pnorm",
        mean = mean(mydata$non_normal),
        sd = sd(mydata$non_normal)
) # normal


# Shapiro Wilk test -------------------------------------------------------

shapiro.test(mydata$normal) # normal
shapiro.test(mydata$non_normal) # not normal


# QQ-Plots ----------------------------------------------------------------
ggplot(mydata, aes(sample = normal)) +
  stat_qq() +
  stat_qq_line()

ggplot(mydata, aes(sample = non_normal)) +
  stat_qq() +
  stat_qq_line()


# Variance tests ----------------------------------------------------------
InsectSprays <- InsectSprays %>%
  filter(spray %in% c("A", "B", "E")) %>%
  mutate(spray = recode(spray, "E" = "C"))

TreatA <- filter(InsectSprays,
                 spray == "A")$count
TreatB <- filter(InsectSprays,
                 spray == "B")$count
TreatC <- filter(InsectSprays,
                 spray == "C")$count

ggplot(InsectSprays, aes(x = spray, y = count)) +
  geom_boxplot(notch = TRUE)

shapiro.test(TreatA) # normal
shapiro.test(TreatB) # normal
shapiro.test(TreatC) # normal

var.test(TreatA, TreatB) # variances equal
var.test(TreatA, TreatC) # variances not equal


# t-test ------------------------------------------------------------------

t.test(TreatA, TreatB, var.equal = TRUE)
t.test(TreatA, TreatC, var.equal = FALSE) # Welch test for unequal variances

# Plot the results

InsectSprays |>
  filter(spray %in% c("A", "B", "C")) |>
  ggplot(aes(x=spray, y=count))+
  geom_boxplot()+
  geom_point()


# Wilcoxon test -----------------------------------------------------------

wilcox.test(TreatA, TreatB)


# paired test -------------------------------------------------------------

t.test(TreatA, TreatB, var.equal = TRUE, paired = TRUE)
t.test(TreatA, TreatB, var.equal = FALSE, paired = TRUE)
wilcox.test(TreatA, TreatB, paired = TRUE)



# Plot the results --------------------------------------------------------

# Plot the results --------------------------------------------------------

ggplot(InsectSprays, aes(x = spray, y = count)) +
  geom_boxplot(notch = TRUE) +
  ggsignif::geom_signif(
    comparisons = list(
      c("A", "B"),
      c("B", "C"),
      c("A", "C")
    ),
    test = "t.test",
    map_signif_level = TRUE,
    y_position = c(23,24,25)
  )

# Plot a bar plot with errorbars

ggplot(InsectSprays, aes(x = spray, y = count)) +
  stat_summary()

ggplot(InsectSprays, aes(x = spray, y = count)) +
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar"
  ) +
  stat_summary(
    fun.y = mean,
    geom = "point",
    color = "#28a87d",
    size = 4
  )

# Bar with errorbars
ggplot(InsectSprays, aes(x = spray, y = count)) +
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    width = .3
  ) +
  stat_summary(
    fun.y = mean,
    geom = "bar",
    size = 4
  )+
  ggsignif::geom_signif(
    comparisons = list(
      c("A", "B"),
      c("B", "C"),
      c("A", "C")
    ),
    test = "t.test",
    map_signif_level = TRUE,
    y_position = c(17,18,19)
  )

# 2. Linear models --------------------------------------------------------
# patchwork package to combine plots
# install.packages("patchwork")
library(patchwork)
library(performance)

prod <- readr::read_tsv(here::here("data/slides/03_productivity.txt")) %>%
  mutate(across(site, as_factor))

# Hypothesis 1: Productivity increases with species richness:
regr <- ggplot(prod, aes(x = richness, y = productivity)) +
  geom_point() +
  geom_smooth(method = "lm", alpha = 0) +
  labs(x = "species richness", title = "Linear regression")
regr_m <- ggplot(prod, aes(color = site, x = richness, y = productivity)) +
  geom_point() +
  geom_smooth(method = "lm", alpha = 0) +
  labs(x = "species richness", title = "Analysis of covariance")

box <- ggplot(prod, aes(x = site, y = productivity)) +
  geom_boxplot(aes(color = site)) +
  labs(title = "Analysis of variance")

(regr  / box / regr_m)


# linear regression -------------------------------------------------------
# does species richness influence productivity?

ggplot(prod, aes(x = richness, y = productivity)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme(
    axis.title.y = element_text(color = "#4C7488", face = "bold"),
    axis.title.x = element_text(color = "#D78974", face = "bold")
  )

prod_lm <- lm(productivity ~ richness,
              data = prod)

# test for significance
drop1(prod_lm, test = "F")
# summary table
summary(prod_lm)

# test model assumptions
# with performance package
performance::check_model(prod_lm)
# with base plot
par(mfrow = c(2, 2)) # change graphics settings to fit 4 plots
plot(prod_lm) # The actual diagnostic plots
par(mfrow = c(1, 1)) # change graphics settings back to default

# plot the model

# OPTION 1: extract model coefficients
prod_lm$coefficients
intercept <- prod_lm$coefficients[1]
slope <- prod_lm$coefficients[2]

ggplot(prod, aes(x = richness, y = productivity)) +
  geom_point() +
  geom_abline(
    slope = slope,
    intercept = intercept
  )

# OPTION 2: geom_smooth

ggplot(prod, aes(x = richness, y = productivity)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# Ancova ------------------------------------------------------------------


# does species richness and site influence productivity
ggplot(prod, aes(x = richness, y = productivity, color = site)) +
  geom_point()
# without interaction
prod_lm2a <- lm(productivity ~ richness + site, data = prod)
# with interaction
prod_lm2b <- lm(productivity ~ richness + site + richness:site, data = prod)
# or the same in short version:
# prod_lm2b <- lm(productivity ~ richness * site, data = prod)

# test for significant effects
drop1(prod_lm2a, test = "F") # no interaction
drop1(prod_lm2b, test = "F") # interaction


# extract coefficients
summary(prod_lm2a)

# test assumptions
performance::check_model(prod_lm2a)

# plot model

# OPTION 1: geom_smooth() for model with interaction
ggplot(prod, aes(x = richness,
                 y = productivity,
                 color = site)) +
  geom_point() +
  geom_smooth(method = "lm")

# OPTION 2: extract coefficients

prod_lm2a$coefficients
slope <- prod_lm2a$coefficients[2]
intercept1 <- prod_lm2a$coefficients[1]
intercept2 <- prod_lm2a$coefficients[1] + prod_lm2a$coefficients[3]

ggplot(prod, aes(x = richness, y = productivity, color = site)) +
  geom_point() +
  scale_color_manual(values = c("#4C7488", "#D78974")) +
  geom_abline(slope = slope, intercept = intercept1, color = "#4C7488") +
  geom_abline(slope = slope, intercept = intercept2, color = "#D78974")

# OPTION 3: predict

# step 1: create some data to predict from
pred_data <- tidyr::expand_grid(
  richness = min(prod$richness):max(prod$richness),
  site = c("site1", "site2")
)
# step2: predict productivity values from pred_data
predictions <- predict(prod_lm2a, newdata = pred_data)

# step 3: add predictions to the tibble
pred_data$productivity <- predictions

# step 4: Add predictions with geom_line
ggplot(prod, aes(x = richness,
                 y = productivity,
                 color = site)) +
  geom_point() +
  geom_line(data = pred_data)



# Anova -------------------------------------------------------------------

# chickwts data set: does the diet effect the weight of chicken?
chickwts

ggplot(chickwts, aes(x = factor(feed), y = weight)) +
  geom_boxplot()

# fit the model
lm_chicken <- lm(weight ~ feed, data = chickwts)
# check for significant effects
drop1(lm_chicken, test = "F")

# coefficients
summary(lm_chicken)

# check assumptions
performance::check_model(lm_chicken)

# plot the model

# boxplot
chickwts %>%
  mutate(feed = as.factor(feed)) %>%
  mutate(feed = fct_reorder(feed, -weight)) %>%
  ggplot(aes(
    x = feed,
    y = weight,
    color = feed
  )) +
  geom_boxplot() +
  geom_point(
    size = 3, alpha = 0.25,
    position = position_jitter(width = 0.2, seed = 0)
  ) +
  coord_flip() +
  ggsci::scale_color_uchicago() +
  labs(y = "weight [g]", x = "Diet") +
  theme(legend.position = "none")

