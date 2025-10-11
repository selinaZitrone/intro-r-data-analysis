library(tidyverse)


# 1. Tidyr ----------------------------------------------------------------

# 1. relig_income

relig_income

relig <- pivot_longer(
  relig_income,
  cols = !religion, # 2:11, `<$10k`:`Don't know/refused`
  names_to = "income",
  values_to = "count"
)

relig

# 2. billboard

billboard

pivot_longer(billboard, cols = wk1:wk76, names_to = "week", values_to = "rank")

# 3. fish_encounters

fish_encounters |>
  pivot_wider(
    names_from = station,
    values_from = seen,
    values_fill = 0
  )


#### Extras
charts <- pivot_longer(
  billboard,
  cols = wk1:wk76,
  names_to = "week",
  values_to = "rank",
  names_prefix = "wk",
  values_drop_na = TRUE
)
charts

# Penguins boxplot
penguins |>
  select(species, bill_len, bill_dep) |>
  pivot_longer(!species, names_to = "variable") |>
  ggplot(aes(x = species, y = value, fill = variable)) +
  geom_boxplot()

# 2. Statistical tests -------------------------------------------------------

# subset for 3 species flipper length
adelie <- filter(penguins, species == "Adelie")$flipper_len
chinstrap <- filter(penguins, species == "Chinstrap")$flipper_len
gentoo <- filter(penguins, species == "Gentoo")$flipper_len

# Step 1: test for normality

# Visual test using QQ-Plots

# use patchwork package to combine the three plots into one
library(patchwork)

a <- ggpubr::ggqqplot(adelie) + labs(title = "adelie")
c <- ggpubr::ggqqplot(chinstrap) + labs(title = "chinstrap")
g <- ggpubr::ggqqplot(gentoo) + labs(title = "gentoo")

a + c + g

# Other option
ggplot(penguins, aes(sample = flipper_len, color = species)) +
  stat_qq() +
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
penguins |>
  ggplot(aes(x = species, y = flipper_len)) +
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
    y_position = c(235, 240),
    map_signif_level = TRUE
  )

# Plot with mean and errorbars:
ggplot(penguins, aes(x = species, y = flipper_len)) +
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
    y_position = c(216, 217),
    map_signif_level = TRUE,
    tip_length = 0.01
  )
