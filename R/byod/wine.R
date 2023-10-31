# Dataset 1: What makes a good wine
# Dataset from Cortez et al. (2009) "Modeling wine preferences by data mining from physicochemical properties"
# See slides for full reference

library(tidyverse)
library(corrplot)
library(factoextra)


# Prepare the data --------------------------------------------------------

wine <- read_csv("data/wine.csv")
# clean the names
wine <- janitor::clean_names(wine)

names(wine)

# How many quality levels does the wine have?
# Quality from 3-8
wine |>
  pull(quality) |>
  unique()

# Drop NA quality
wine <- wine |> filter(!is.na(quality))

# Turn quality into a factor
wine <- mutate(wine, quality = as.factor(quality))


# Exploratory data analysis -----------------------------------------------

# Relationship between all variables and quality
wine |>
  pivot_longer(!quality) |>
  ggplot(aes(x = quality, y = value)) +
  geom_boxplot(notch = TRUE) +
  facet_wrap(~name, scales = "free_y")

# Looks like alcohol and acidity really make a difference
wine |>
  select(alcohol, citric_acid, volatile_acidity, quality) |>
  pivot_longer(!quality) |>
  ggplot(aes(x = quality, y = value)) +
  geom_boxplot(notch = TRUE) +
  facet_wrap(~name, scales = "free_y")


# Correlation plots -------------------------------------------------------

# How are the different characteristics correlated with each other?
# https://taiyun.github.io/corrplot/

# Remove the quality column, drop all nas and make create a correlation matrix
M <- wine |>
  select(-quality) |>
  drop_na() |>
  cor()

# Create correlation plots
corrplot(M)
corrplot.mixed(M)
corrplot.mixed(M, lower = "shade", upper = "pie", order = "hclust")

# PCA analysis ------------------------------------------------------------------
# Tutorial: http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/118-principal-component-analysis-in-r-prcomp-vs-princomp/
# https://rpkgs.datanovia.com/factoextra/index.html

res.pca <- wine |>
  drop_na() |>
  select(-quality) |>
  prcomp(scale = TRUE)

# Visualize eigenvalues (how much variance is explained by the pcs)
fviz_eig(res.pca)

# How are the variables correlated with each other?
# Longer arrows and more red colors mean higher contribution to the PC
fviz_pca_var(res.pca,
  col.var = "contrib", # Color by contributions to the PC
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  repel = TRUE # Avoid text overlapping
)

# Plot individual data points
fviz_pca_ind(res.pca,
  col.ind = "cos2", geom = "point",
  gradient.cols = c("white", "#2E9FDF", "#FC4E07")
)

# Adding groups according to the quality judgement
groups <- factor(wine |> drop_na() |> pull(quality))

fviz_pca_ind(res.pca,
  label = "none",
  habillage = wine |> drop_na() |> pull(quality) |> factor(),
  addEllipses = TRUE,
  ellipse.level = 0.95, palette = "Dark2"
)

# Add arrows for the properties
fviz_pca_biplot(res.pca,
  label = "var",
  habillage = wine |> drop_na() |> pull(quality) |> factor(),
  addEllipses = TRUE,
  ellipse.level = 0.95,
  palette = "Dark2"
)

# get eigenvalues
eig.val <- get_eigenvalue(res.pca)

# results for the variables
res.var <- get_pca_var(res.pca)
res.var$coord # Coordinates
res.var$contrib # Contributions to the PCs
res.var$cos2 # Quality of representation
