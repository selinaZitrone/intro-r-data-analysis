# Dataset 3: Atlantic marsh fiddler crab
# Data set from Johnson 2019 provided in the lterdatasample package
# See here for more info: https://lter.github.io/lterdatasampler/articles/pie_crab_vignette.html

library(lterdatasampler)
library(tidyverse)
library(corrplot)
library(factoextra)

# Remove all NAs
pie_crab_no_na <- pie_crab %>% drop_na()


# Exploratory plots -------------------------------------------------------

# Bergman's rule: Organisms are larger in higher latitudes
pie_crab %>%
  ggplot(aes(y = latitude)) +
  geom_boxplot(aes(size, group = latitude, color = -latitude), outlier.size = 0.8) +
  scale_x_continuous(breaks = seq(from = 7, to = 23, by = 2), limits = c(6.5, 24)) +
  scale_y_continuous(breaks = seq(from = 29, to = 43, by = 2), limits = c(29, 43.5)) +
  theme(legend.position = "none")

# Relationship between water temperature and latitude

ggplot(data = pie_crab, aes(y = latitude, x = water_temp)) +
  geom_point()

# Correlation plots -------------------------------------------------------

# Correlation plot
pie_crab_no_na %>%
  select(where(is.numeric)) %>%
  as.matrix() %>%
  cor() %>%
  corrplot.mixed(lower = "shade", upper = "pie", order = "hclust")


# PCA analysis ------------------------------------------------------------

# Run the PCA only on the numeric columns
pie.pca <- pie_crab_no_na %>%
  select(where(is.numeric)) %>%
  prcomp(scale = TRUE)

# Visualize eigenvalues (how much variance is explained by the pcs)
fviz_eig(pie.pca)

# Plot the variables on the first 2 PCs
# How are the variables correlated with each other?
# Longer arrows and more red colors mean higher contribution to the PC
fviz_pca_var(pie.pca,
  col.var = "contrib", # Color by contributions to the PC
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  repel = TRUE # Avoid text overlapping
)

# Add points and color them by the salt march
fviz_pca_biplot(pie.pca,
  label = "var",
  habillage = pie_crab_no_na$name
)

# Does this plot make sense? How are the different sites related to
# The environmental variables?
# Yes it makes sense, e.g. the Bare Cove Park site has high latitude
# and low air temperature, which is also what we see in the
# pca plot
pie_crab %>%
  select(-date, -site, -size) %>%
  distinct() %>%
  pivot_longer(!name, names_to = "type", names_repair = "minimal") %>%
  ggplot(aes(y = name, x = value)) +
  geom_point() +
  facet_wrap(~type, nrow = 1, scales = "free_x")
