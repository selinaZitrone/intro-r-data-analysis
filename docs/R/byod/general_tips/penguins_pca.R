# Load necessary libraries  ---------------------------------------------------
library(tidyverse) # For data manipulation and visualization
library(palmerpenguins) # Contains Palmer penguins dataset for demonstration
library(corrplot) # For creating correlation plots
library(factoextra) # For visualizing PCA and extracting PCA results

# Principal Component Analysis (PCA)  -----------------------------------------
# PCA is a technique used to reduce the dimensionality of a dataset,
# while retaining most of the variance. Here, we demonstrate how to perform PCA on the penguins data.

# Remove rows containing any missing values to avoid errors during PCA.
peng_no_na <- penguins %>% drop_na()

# Select the numeric variables relevant for PCA.
# Here, we use bill_length, bill_depth, flipper_length, and body_mass.
peng_pca <- peng_no_na %>%
  select(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g)

# Perform PCA using the prcomp() function.
# We set 'scale = TRUE' to standardize the variables,
# so that they contribute equally to the analysis.
peng.pca <- prcomp(peng_pca, scale = TRUE)

# Visualizing the PCA Results  -----------------------------------------------

# 1. Eigenvalues: Visualize how much variance is explained by each principal component.
#    This helps in deciding how many components to consider.
fviz_eig(peng.pca)

# 2. PCA Variable Plot:
#    Visualize the correlations between the original variables and the principal components.
#    Variables with longer arrows have a stronger contribution to the PC.
#    The color gradient (from dark blue to red) indicates the magnitude of the contribution.
fviz_pca_var(peng.pca,
  col.var = "contrib", # Color the arrows by their contribution
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  repel = TRUE # Avoid overlapping text labels
)

# 3. PCA Individual Plot:
#    Visualize the scores of each observation (penguin) in the PCA space.
#    Here, we color the points by their species (using 'habillage'),
#    and add ellipses representing the 95% confidence level.
fviz_pca_ind(peng.pca,
  label = "none", # Do not label individual points for clarity
  habillage = peng_no_na$species, # Color points by penguin species
  addEllipses = TRUE, # Add confidence ellipses over groups
  ellipse.level = 0.95, # 95% confidence intervals
  palette = "Dark2"
)

# 4. Combined PCA Plot:
#    This plot overlays the PCA biplot with both individuals (colored by species) and variable vectors.
#    It also includes ellipses for the groups and labels the variables.
fviz_pca(peng.pca,
  label = "var", # Label the variable vectors
  habillage = peng_no_na$species, # Color individuals by species
  addEllipses = TRUE, # Add confidence ellipses around groups
  ellipse.level = 0.95, # 95% interval for ellipses
  palette = "Dark2"
)

# Extracting and Understanding PCA Statistics  --------------------------------
# Get the eigenvalues which indicate the amount of variance explained by each component.
eig.val <- get_eigenvalue(peng.pca)

# Get results for the variables:
#  - Coordinates: How variables relate to the principal components.
#  - Contributions: How much each variable contributes to the components.
#  - Cos2 values: The quality of representation of the variables on the components.
res.var <- get_pca_var(peng.pca)
res.var$coord # The coordinates of each variable in the PCA space
res.var$contrib # The contributions of each variable to the components
res.var$cos2 # The squared cosine values showing representation quality

# Get results for the individuals (observations)
res.ind <- get_pca_ind(peng.pca)
# Display the length of contribution values for individuals to confirm results
length(res.ind$contrib)
