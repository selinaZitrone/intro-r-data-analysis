# Purpose: Show example of NMDS ordination + PERMANOVA
# Here are more links and info on NMDS:
# https://uw.pressbooks.pub/appliedmultivariatestatistics/chapter/nmds/

# 0) Libraries -----------------------------------------------------------------
library(tidyverse)
library(vegan) # for ordination, distances and permanova

# 1) Load data -----------------------------------------------------------------
# we use the built-in 'dune' dataset from vegan package
# dune: sites x species abundance matrix (20 sites, 30 plant species)
# dune.env: environmental metadata for those 20 sites (e.g., Management, Moisture)
data(dune)
data(dune.env)
?dune

# 2) Run NMDS ------------------------------------------------------------------
# Run the NMDS using default settings.
# Check ?metaMDS for details on arguments.
nmds_fit <- metaMDS(
  dune,
  distance = "bray", # dissimilarity index (default is "bray")
  k = 2, # number of dimensions (default is 2)
)

# create a stress plot to evaluate the NMDS fit
# What to look for:
# High R2 values
# points clustering tightly around the red line
# no obvious outliers
# If there is a problem with this plot: consider increasing k (number of dimensions)
# in metaMDS(), default is k=2. But here it looks perfect
stressplot(nmds_fit)
# Check the stress value: Should be small (roughly <0.2 depending on context)
# Here it looks good
nmds_fit$stress

# 3) Extract site scores for plotting ------------------------------------------
# scores(..., display = "sites") gives the NMDS coordinates of each site (row in dune).
nmds_sites <- scores(nmds_fit, display = "sites") |>
  as_tibble()

# Look at the site scores
nmds_sites

# We join the environmental data
# Here, we can just bind the environment columns to the nmds results
# They are both in the same order (20 rows, 20 rows)

dune.env

plot_df <- bind_cols(nmds_sites, dune.env)
plot_df

# 4) Ordination plot (ggplot2) -------------------------------------------------
# Points are colored by Moisture
# You could also color by other variables in dune.env, e.g., Management
ggplot(plot_df, aes(x = NMDS1, y = NMDS2, color = Moisture)) +
  geom_point(size = 3, alpha = 0.9) +
  coord_equal() +
  theme_classic(base_size = 12) +
  labs(
    title = "NMDS ordination of dune community composition",
    x = "NMDS1",
    y = "NMDS2",
    color = "Moisture level"
  )

# 5) PERMANOVA (adonis2) -------------------------------------------------------
# Tests whether centroids of groups (Management) differ in multivariate space.
# Uses the same dissimilarity to match the ordination (default of ).
bray_dist <- vegdist(dune, method = "bray")
# Check the distance matrix
bray_dist

# Test if different Management types have different community centroids
# If p < 0.05: there is a significant difference in community composition
# among the different Management types.
# R2: proportion of variance explained by Management
# test a single predictor
adonis2(
  bray_dist ~ Moisture,
  data = dune.env,
  permutations = 999
)

# test two predictors (predictors cannot be correlated)
# in this example: both predictors are significant
adonis2(
  bray_dist ~ Management + Moisture,
  data = dune.env,
  permutations = 999,
  by = "margin"
) # Each term adjusted for others

# 6) Homogeneity of dispersions check (betadisper) -----------------------------
# permanova assumes similar within-group dispersion. So we need to test
# whether this assumption is met.
# betadisper tests whether group dispersions differ significantly.
bd_mgmt <- betadisper(bray_dist, dune.env$Management)
bd_moist <- betadisper(bray_dist, dune.env$Moisture)
# Look for non-significant results (p > 0.05)
# If significant: groups have different dispersions
anova(bd_mgmt)
anova(bd_moist)
