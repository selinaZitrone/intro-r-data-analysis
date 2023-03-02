library(tidyverse)
library(palmerpenguins)
library(corrplot)
library(factoextra)

# Tutorial here

# Correlation plot --------------------------------------------------------
penguins %>%
  select(where(is.numeric)) %>%
  drop_na() %>%
  as.matrix() %>%
  cor() %>%
  corrplot.mixed(lower = "shade", upper = "pie", order = "hclust")


# PCA ---------------------------------------------------------------------
peng_no_na <- penguins %>% drop_na()
peng_pca <- peng_no_na %>%
  select(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g)

peng.pca <- prcomp(peng_pca, scale=TRUE)

# Visualize eigenvalues (how much variance is explained by the pcs)
fviz_eig(peng.pca)

# Plot the variables on the first 2 PCs
# How are the variables correlated with each other?
# Longer arrows and more red colors mean higher contribution to the PC
fviz_pca_var(peng.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
)


# Adding groups
fviz_pca_ind(peng.pca,
             label = "none",
             habillage = peng_no_na$species,
             addEllipses = TRUE,
             ellipse.level = 0.95, palette = "Dark2"
)
# Adding ellipses
fviz_pca(peng.pca,
                label = "var",
                habillage = peng_no_na$species,
                addEllipses = TRUE,
                ellipse.level = 0.95,
                palette = "Dark2"
)

# get eigenvalues
eig.val <- get_eigenvalue(peng.pca)

# results for the variables
res.var <- get_pca_var(peng.pca)
res.var$coord # Coordinates
res.var$contrib # Contributions to the PCs
res.var$cos2 # Quality of representation
res.var$coord

res.ind <- get_pca_ind(peng.pca)
res.ind$contrib %>% length()
