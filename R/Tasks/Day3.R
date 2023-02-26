library(tidyverse)
library(palmerpenguins)
library(performance)

# 1. Tidyr ----------------------------------------------------------------

# 1. relig_income

relig_income

pivot_longer(relig_income,
             cols = !religion, # 2:11, `<$10k`:`>150k`
             names_to = "income",
             values_to = "count"
)


# 2. billboard

billboard

pivot_longer(billboard,
             cols = wk1:wk76,
             names_to = "week",
             values_to = "rank"
)

#### Extras

charts <- pivot_longer(billboard,
                       cols = wk1:wk76,
                       names_to = "week",
                       names_prefix = "wk",
                       values_to = "rank",
                       values_drop_na = TRUE
)

separate(
  charts,
  date.entered, # column to separate
  sep = "-",
  into = c("year", "month", "day")
)

# 3. fish_encounters

fish_encounters

summary(fish_encounters)

pivot_wider(fish_encounters,
            names_from = station,
            values_from = seen
)
#### Extra

pivot_wider(fish_encounters,
            names_from = station,
            values_from = seen,
            values_fill = 0
)

# Transpose a tibble with a combination of pivot_longer and pivot_wider

relig_income %>%
  pivot_longer(-religion) %>%
  pivot_wider(names_from = religion, values_from = value)

# Penguins boxplot

penguins %>%
  select(species, bill_length_mm, bill_depth_mm) %>%
  pivot_longer(!species) %>%
  ggplot(aes(x = species, y = value, fill = name)) +
  geom_boxplot()

# 2. Statistical tests -------------------------------------------------------

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

# Test the same with the Kolmogorov Smirnov Test
ks.test(adelie, "pnorm",
  mean = mean(adelie, na.rm = TRUE), sd = sd(adelie, na.rm = TRUE)
)
ks.test(chinstrap, "pnorm",
  mean = mean(chinstrap, na.rm = TRUE), sd = sd(chinstrap, na.rm = TRUE)
)
ks.test(gentoo, "pnorm",
  mean = mean(gentoo, na.rm = TRUE), sd = sd(gentoo, na.rm = TRUE)
)

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

