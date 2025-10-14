# Goal: Understanding seurat objects, build lineage trees of cells

# Useful links

# Seurat website: check out the tutorials in "Get Started" and "Vignettes" to
# see what is interesting or you
# https://satijalab.org/seurat/

# Load libraries
library(tidyverse)
library(Seurat)


# ----------------------------------------------------------------------------#
# 1. Seurat objects ----------------------------------------------------------
# ----------------------------------------------------------------------------#

# Example data
data("pbmc_small")
# Get info on the example dataset
?pbmc_small

# Quick overview of Seurat object
# number of features (genes), cells, assays, and reductions present
pbmc_small

# What assays are included?
Assays(pbmc_small)
# What is the default assay?
# Many of the following functions will use this assay unless you specify otherwise
DefaultAssay(pbmc_small)

# Extract Cell-level metadata and transform to a tibble for easier viewing
cell_meta <- pbmc_small@meta.data |>
  tibble::rownames_to_column(var = "cell") |>
  as_tibble()
cell_meta

# Get Assay data (from the default essay)
# - counts: raw counts (usually integers)
# - data: normalized expression (log-normalized by default)
# - scale.data: scaled values used for PCA/heatmaps

counts_mat <- GetAssayData(pbmc_small, layer = "counts")
norm_mat <- GetAssayData(pbmc_small, layer = "data")
scale_mat <- GetAssayData(pbmc_small, layer = "scale.data")

# How do the matrices look like?
# Peek at dimensions and a small slice of each matrix
dim(counts_mat)
counts_mat[1:5, 1:5]
dim(norm_mat)
norm_mat[1:5, 1:5]
dim(scale_mat)
scale_mat[1:5, 1:5]

# Variable features (genes with high variability used for PCA, etc.)
VariableFeatures(pbmc_small) |> head(20)

# Identities (cluster labels) if present
# returns a factor with the cluster/identity per cell
head(Idents(pbmc_small))

# Dimensionality reductions available (e.g., PCA, UMAP, t-SNE)
Reductions(pbmc_small)

# Access reduction embeddings directly (e.g., PCA, t-SNE if present)
# Here for the PCA reduction
pca_coords <- Embeddings(pbmc_small, "pca")
head(pca_coords)
# Here for tsne
tsne_coords <- Embeddings(pbmc_small, "tsne")
head(tsne_coords)

# Quick plots to visualize structure
# - DimPlot: scatter plot of cells in a low-dimensional space
# - FeaturePlot: gene expression overlay on the embedding

# PCA plot colored by cluster (ident)
DimPlot(pbmc_small, reduction = "pca") +
  ggtitle("PCA: cells colored by cluster")

# t-SNE plot colored by cluster (if available)
DimPlot(pbmc_small, reduction = "tsne", label = TRUE) +
  ggtitle("t-SNE: cells colored by cluster")

# FeaturePlot: visualize expression of a gene (choose a gene that exists)
# What genes are in the dataset?
rownames(pbmc_small)

# select one of those genes to plot
gene_to_plot <- "LYZ"
# Default layer is data (normalized expression)
FeaturePlot(pbmc_small, features = gene_to_plot, reduction = "pca") +
  ggtitle(paste("Expression of", gene_to_plot, "on PCA"))
# Plot counts
FeaturePlot(
  pbmc_small,
  features = gene_to_plot,
  reduction = "pca",
  slot = "count"
) +
  ggtitle(paste("Expression of", gene_to_plot, "on PCA"))
