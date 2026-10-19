library(tidyverse)
library(ggsignif)
library(ggpubr)


# 1. Tidyr ----------------------------------------------------------------

# 1. relig_income

relig_income

relig_tidy <- pivot_longer(
  relig_income,
  cols = !religion, # 2:11, `<$10k`:`Don't know/refused`
  names_to = "income",
  values_to = "count"
)

relig_tidy

# 2. billboard

billboard

pivot_longer(billboard, cols = wk1:wk76, names_to = "week", values_to = "rank")

# 3. fish_encounters

pivot_wider(
  fish_encounters,
  names_from = station,
  values_from = seen
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

# Replace missing values with 0 (fish not seen at that station)
pivot_wider(
  fish_encounters,
  names_from = station,
  values_from = seen,
  values_fill = 0
)

# Penguins boxplot
penguins |>
  select(species, bill_len, bill_dep) |>
  pivot_longer(!species, names_to = "variable") |>
  ggplot(aes(x = species, y = value, fill = variable)) +
  geom_boxplot()

# 2. Statistical tests -------------------------------------------------------

## Part 1: Compare two species

# 1. Smaller version of the data
penguins_small <- penguins |>
  filter(species %in% c("Adelie", "Chinstrap"))

# 2. Plot the two species
ggplot(penguins_small, aes(x = species, y = flipper_len)) +
  geom_boxplot()

# 3. Check for normality
adelie <- filter(penguins_small, species == "Adelie")$flipper_len
chinstrap <- filter(penguins_small, species == "Chinstrap")$flipper_len

shapiro.test(adelie) # normal
shapiro.test(chinstrap) # normal

ggqqplot(adelie)
ggqqplot(chinstrap)

# Or plot both in the same plot
ggqqplot(penguins_small, x = "flipper_len", facet.by = "species")

# 4. Compare the means
t.test(adelie, chinstrap)
# Mean flipper length was 5.9 mm shorter in Adelie than in Chinstrap penguins
# (Welch's t-test, t(119.7) = -5.78, p < 0.001)

# 5. Add the test result to the plot
ggplot(penguins_small, aes(x = species, y = flipper_len)) +
  geom_boxplot() +
  geom_signif(
    comparisons = list(c("Adelie", "Chinstrap")),
    test = "t.test",
    map_signif_level = TRUE
  )

## Part 2: Compare all three species

# 1. Pairwise comparisons
pairwise.t.test(
  penguins$flipper_len,
  penguins$species,
  pool.sd = FALSE
)
# All three species differ (Holm-adjusted p < 0.001 for all pairs)

# 2. Plot with significance levels

# p-values typed by hand
ggplot(penguins, aes(x = species, y = flipper_len)) +
  geom_boxplot() +
  geom_signif(
    comparisons = list(
      c("Adelie", "Chinstrap"),
      c("Adelie", "Gentoo"),
      c("Chinstrap", "Gentoo")
    ),
    annotations = c("p < 0.001", "p < 0.001", "p < 0.001"),
    step_increase = 0.1,
    tip_length = 0.02
  )

# p-values extracted automatically
pairwise_results <- pairwise.t.test(
  penguins$flipper_len,
  penguins$species,
  pool.sd = FALSE
)
p <- pairwise_results$p.value
p

comparisons <- list(
  c("Adelie", "Chinstrap"),
  c("Adelie", "Gentoo"),
  c("Chinstrap", "Gentoo")
)
# p["row", "column"]
adjusted_p <- c(
  p["Chinstrap", "Adelie"],
  p["Gentoo", "Adelie"],
  p["Gentoo", "Chinstrap"]
)
p_labels <- paste("p =", format.pval(adjusted_p, digits = 2))
p_labels

penguins_boxplot <- ggplot(penguins, aes(x = species, y = flipper_len)) +
  geom_boxplot() +
  geom_signif(
    comparisons = comparisons,
    annotations = p_labels,
    step_increase = 0.1,
    tip_length = 0.02
  )
penguins_boxplot

## For the fast ones

# Mean +- standard error
ggplot(penguins, aes(x = species, y = flipper_len)) +
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    width = 0.15
  ) +
  stat_summary(
    fun = mean,
    geom = "point",
    size = 3
  ) +
  geom_signif(
    comparisons = comparisons,
    annotations = p_labels,
    step_increase = 0.1,
    tip_length = 0.02
  )

# Barplot with error bars
ggplot(penguins, aes(x = species, y = flipper_len)) +
  stat_summary(
    fun.data = mean_se,
    geom = "errorbar",
    width = 0.15
  ) +
  stat_summary(
    fun = mean,
    geom = "bar",
    fill = "grey70",
    width = 0.65
  ) +
  geom_signif(
    comparisons = comparisons,
    annotations = p_labels,
    step_increase = 0.3,
    tip_length = 0.02
  )

# Make the plot look nice
penguins_boxplot +
  labs(x = NULL, y = "Flipper length (mm)") +
  theme_classic() +
  theme(
    text = element_text(size = 16),
    panel.grid.major.y = element_line(color = "grey90")
  )

# Wilcoxon rank-sum test
wilcox.test(adelie, chinstrap)
# Same conclusion: flipper length was smaller in Adelie than in Chinstrap
# penguins (median 190 vs. 196 mm; Wilcoxon rank-sum test, W = 2733.5, p < 0.001)

# Normality of Gentoo penguins
gentoo <- filter(penguins, species == "Gentoo")$flipper_len
shapiro.test(gentoo) # small p-value
ggqqplot(gentoo) # only small deviations from the line
# With 123 penguins, the test detects even small deviations: the t-test is fine

## Bonus: Many groups

# Table with one row per comparison
p_table <- pairwise_results$p.value |>
  as_tibble(rownames = "group1") |>
  pivot_longer(!group1, names_to = "group2", values_to = "p_adj") |>
  drop_na(p_adj) |>
  mutate(
    label = if_else(
      p_adj < 0.001,
      "p < 0.001",
      paste("p =", signif(p_adj, 2))
    )
  )
p_table

ggplot(penguins, aes(x = species, y = flipper_len)) +
  geom_boxplot() +
  geom_signif(
    comparisons = map2(p_table$group2, p_table$group1, c),
    annotations = p_table$label,
    step_increase = 0.1,
    tip_length = 0.02
  )
