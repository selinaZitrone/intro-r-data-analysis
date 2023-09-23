library(tidyverse)
library(corrplot)
library(factoextra)

pie_crab <- read_csv(file = "data/pie_crab.csv", col_names = TRUE)

# Remove all NAs
pie_crab_no_na <- pie_crab %>% drop_na()

# Correlation plot
pie_crab_no_na %>%
  select(where(is.numeric)) %>%
  as.matrix() %>%
  cor() %>%
  corrplot.mixed(lower = "shade", upper = "pie", order = "hclust")

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
