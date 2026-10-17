# Title: Statistical tests (Day 3 slides)
# Date: 2026-10-17
# Author: Selina Baldauf
# Purpose: All code from the statistical tests slides, in slide order.

library(tidyverse)
library(ggsignif)

# Compare two species -----------------------------------------------------

flowers <- as_tibble(iris) |>
  filter(Species %in% c("versicolor", "virginica")) |>
  select(sepal_width = Sepal.Width, species = Species) |>
  droplevels()
flowers

flowers |>
  summarize(
    n = n(),
    mean = mean(sepal_width),
    sd = sd(sepal_width),
    .by = species
  )

ggplot(flowers, aes(x = species, y = sepal_width)) +
  geom_boxplot() +
  geom_point(
    position = position_jitter(width = 0.08, height = 0, seed = 123),
    alpha = 0.6
  ) +
  labs(x = "Species", y = "Sepal width (cm)")

### Group vectors
versicolor <- filter(flowers, species == "versicolor")$sepal_width
virginica <- filter(flowers, species == "virginica")$sepal_width
versicolor

### Normality
shapiro.test(versicolor)
shapiro.test(virginica)

ggpubr::ggqqplot(versicolor)
ggpubr::ggqqplot(virginica)

# Simulated non-normal examples
set.seed(123)
skewed <- rlnorm(50, meanlog = 0, sdlog = 0.6)
set.seed(4)
heavy_tails <- rt(50, df = 2)

ggpubr::ggqqplot(skewed, title = "Skewed: points form an arch")
ggpubr::ggqqplot(heavy_tails, title = "Heavy tails: both ends bend away")

### Welch's t-test
t.test(versicolor, virginica)

### Wilcoxon rank-sum test
wilcox.test(versicolor, virginica)

### Paired t-test
sleep_paired <- sleep |>
  pivot_wider(
    names_from = group,
    names_prefix = "drug_",
    values_from = extra
  )
sleep_paired

t.test(
  sleep_paired$drug_1,
  sleep_paired$drug_2,
  paired = TRUE
)

# Visualize test results --------------------------------------------------

ggplot(
  flowers,
  aes(x = species, y = sepal_width)
) +
  geom_boxplot()

ggplot(
  flowers,
  aes(x = species, y = sepal_width)
) +
  geom_boxplot() +
  geom_signif(
    comparisons = list(
      c("versicolor", "virginica")
    ),
    test = "t.test",
    map_signif_level = TRUE
  )

### Means and error bars
ggplot(
  flowers,
  aes(x = species, y = sepal_width)
) +
  stat_summary()

ggplot(
  flowers,
  aes(x = species, y = sepal_width)
) +
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    width = 0.15
  ) +
  stat_summary(
    fun = mean,
    geom = "point",
    color = "#28a87d",
    size = 4
  )

### Barplot
ggplot(
  flowers,
  aes(x = species, y = sepal_width)
) +
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    width = 0.15
  ) +
  stat_summary(
    fun = mean,
    geom = "bar",
    fill = "#28a87d",
    width = 0.65
  )

ggplot(
  flowers,
  aes(x = species, y = sepal_width)
) +
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    width = 0.15
  ) +
  stat_summary(
    fun = mean,
    geom = "bar",
    fill = "#28a87d",
    width = 0.65
  ) +
  geom_signif(
    comparisons = list(
      c("versicolor", "virginica")
    ),
    test = "t.test",
    map_signif_level = TRUE
  )

# Compare three species ---------------------------------------------------

flowers_all <- as_tibble(iris) |>
  select(sepal_width = Sepal.Width, species = Species)

flowers_all |>
  summarize(
    n = n(),
    mean = mean(sepal_width),
    .by = species
  )

ggplot(flowers_all, aes(x = species, y = sepal_width)) +
  geom_boxplot() +
  geom_point(
    position = position_jitter(width = 0.08, height = 0, seed = 123),
    alpha = 0.6
  ) +
  labs(x = "Species", y = "Sepal width (cm)")

### Pairwise Welch's t-tests with Holm-adjusted p-values
pairwise.t.test(
  flowers_all$sepal_width,
  flowers_all$species,
  pool.sd = FALSE
)

### Add p-values by hand
ggplot(
  flowers_all,
  aes(x = species, y = sepal_width)
) +
  geom_boxplot() +
  geom_signif(
    comparisons = list(
      c("setosa", "versicolor"),
      c("setosa", "virginica"),
      c("versicolor", "virginica")
    ),
    annotations = c(
      "p = 7.5e-15",
      "p = 9.1e-09",
      "p = 0.0018"
    ),
    step_increase = 0.1,
    tip_length = 0.02
  )

### Extract p-values automatically
pairwise_results <- pairwise.t.test(
  flowers_all$sepal_width,
  flowers_all$species,
  pool.sd = FALSE
)
p <- pairwise_results$p.value

comparisons <- list(
  c("setosa", "versicolor"),
  c("setosa", "virginica"),
  c("versicolor", "virginica")
)
# p["row", "column"]
adjusted_p <- c(
  p["versicolor", "setosa"],
  p["virginica", "setosa"],
  p["virginica", "versicolor"]
)
p_labels <- paste(
  "p =",
  format.pval(adjusted_p, digits = 2)
)

ggplot(
  flowers_all,
  aes(x = species, y = sepal_width)
) +
  geom_boxplot() +
  geom_signif(
    comparisons = comparisons,
    annotations = p_labels,
    step_increase = 0.1,
    tip_length = 0.02
  )

### Barplot with adjusted p-values
ggplot(
  flowers_all,
  aes(x = species, y = sepal_width)
) +
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    width = 0.15
  ) +
  stat_summary(
    fun = mean,
    geom = "bar",
    fill = "#28a87d",
    width = 0.65
  ) +
  geom_signif(
    comparisons = comparisons,
    annotations = p_labels,
    step_increase = 0.1,
    tip_length = 0.02
  )
