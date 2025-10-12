# Demonstration of k-means clustering in R using the penguins dataset

# Load required libraries
library(tidyverse)
library(GGally) # For enhanced visualizations
library(factoextra) # For clustering visualization

# Remove NAs from the penguins dataset
penguins_clean <- penguins |> drop_na() # Remove rows with missing values

# -----------------------------------------------------------------------------#
# Step 1: Prepare data for clustering ------------------------------------
# -----------------------------------------------------------------------------#

# Select numerical features for clustering
# We'll use bill length, bill depth, flipper length, and body mass
penguins_numeric <- penguins_clean |>
  select(bill_len, bill_dep, flipper_len, body_mass)

# Scale the data (important for k-means since it's distance-based)
penguins_scaled <- scale(penguins_numeric)

# ----------------------------------------------------------------------------#
# Step 2: Determine optimal number of clusters--------------------------------
# ----------------------------------------------------------------------------#
# Compute total within-cluster sum of squares for different k values
set.seed(123) # For reproducibility

# Idea: Run k-means with different no of clusters and check the
# within-cluster sum of squares (wss) to find the "elbow point" where
# adding more clusters doesn't significantly improve the fit

# Option A: Do it by hand
# Run k-means for k = 1 to 10 and store the total within-cluster sum of squares
# nstart = 25 means the algorithm will be run 25 times with different
# random initializations to find a better solution
wss <- map_dbl(1:10, \(k) {
  kmeans(penguins_scaled, centers = k, nstart = 25)$tot.withinss
})
# Plot wss against k to visualize the elbow
tibble(
  n_clust = 1:10,
  wss = wss
) |>
  ggplot(aes(x = n_clust, y = wss)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 1:10) +
  labs(
    title = "Elbow Method for Determining Optimal k",
    x = "Number of clusters (k)",
    y = "Total within-cluster sum of squares (WSS)"
  )

# Option B: Use factoextra package to visualize the elbow method
fviz_nbclust(penguins_scaled, FUNcluster = kmeans, method = "wss") +
  labs(title = "Optimal number of clusters using Elbow method")

# ----------------------------------------------------------------------------#
# STep 3: Perform k-means clustering with optimal k -------------------------
# ----------------------------------------------------------------------------#

# Based on the elbow plot and our knowledge of penguins, let's use k = 3 (3 species of penguins)
set.seed(123)
kmeans_result <- kmeans(penguins_scaled, centers = 3, nstart = 25)

# View clustering results
kmeans_results

# Add cluster assignments to our original data
penguins_clustered <- penguins_clean |>
  mutate(cluster = as.factor(kmeans_result$cluster))

# -----------------------------------------------------------------------------#
# STEP 4: Visualize and interpret the clusters ------------------------------
# -----------------------------------------------------------------------------#

# Visualize cluster centroids
# Shown in 2 dimensions using PCA for visualization
fviz_cluster(
  kmeans_result,
  data = penguins_scaled,
  geom = "point",
  ellipse.type = "convex",
  palette = "jco",
  ggtheme = theme_minimal()
)

# Or with ggplot
# You can either plot dimensions separately or use GGally::ggpairs to see
# pairwise relationships
ggpairs(
  penguins_clustered,
  columns = c("bill_len", "bill_dep", "flipper_len", "body_mass"),
  aes(color = cluster),
  upper = list(continuous = "points"),
  diag = list(continuous = "densityDiag"),
  lower = list(continuous = "cor"),
  title = "Clustering Results"
) +
  theme_minimal() +
  labs(caption = "Clusters from k-means (k=3)")


# ----------------------------------------------------------------------------#
# STEP 5: Evaluate clustering performance ------------------------------
# ----------------------------------------------------------------------------#

# How well do the clusters match the actual species?
table(
  Species = penguins_clean$species,
  Cluster = kmeans_result$cluster
)

# Calculate silhouette score (measures how similar objects are to their own cluster
# compared to other clusters - ranges from -1 to 1
# > 0.5: Well-clustered
# 0.25-0.5: Reasonable fit
# < 0.25: Poorly clustered (may belong to another cluster)

# Compute silhouette widths for the k-means clustering (uses the cluster package)
sil <- cluster::silhouette(kmeans_result$cluster, dist(penguins_scaled))

# Plot silhouette using factoextra
fviz_silhouette(sil, palette = "jco", ggtheme = theme_minimal()) +
  labs(title = "Silhouette Plot for K-means (k=3)")
