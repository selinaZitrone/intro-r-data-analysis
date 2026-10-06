# Goal: Run a basic scRNA-seq analysis pipeline in Seurat
# Steps:
# Read data
# QC
# Normalize
# Variable features
# PCA
# Elbow plot
# UMAP
# Clustering
# Marker genes

# Useful links:
# Seurat "Getting started" tutorial (very detailed - this script is based on it):
#   https://satijalab.org/seurat/articles/pbmc3k_tutorial
# Seurat cheat sheet:
#   https://satijalab.org/seurat/articles/essential_commands

# Install Seurat if needed (only run once):
# install.packages("Seurat")

library(tidyverse)
library(Seurat)

# Load data ---------------------------------------------------------------

# If your data is in 10X format (a folder with matrix.mtx, barcodes.tsv,
# features/genes.tsv), use Read10X:
# ADJUST the path to point to your folder containing the 3 files
# counts <- Read10X(data.dir = "path/to/your/mtx_folder/")
# seurat_obj <- CreateSeuratObject(counts = counts, project = "my_project")

# here I use an example data set included with Seurat, which is already in the
# correct format.
data("pbmc_small")

# Extract the raw counts and create a fresh Seurat object from scratch
# (so we can run the full pipeline ourselves)
counts <- GetAssayData(pbmc_small, layer = "counts")
seurat_obj <- CreateSeuratObject(counts = counts, project = "pbmc_demo")

seurat_obj # Quick overview: how many genes, how many cells?

# QC ---------------------------------------------------

# The percentage of reads that map to the mitochondrial genome
seurat_obj[["percent.mt"]] <- PercentageFeatureSet(seurat_obj, pattern = "^MT-")

# Look at QC metrics stored in the metadata
head(seurat_obj@meta.data)


# QC violin plots — check the distributions before filtering
VlnPlot(
  seurat_obj,
  features = c("nFeature_RNA", "nCount_RNA", "percent.mt"),
  ncol = 3
)

# Same plot but custom
# Custom ggplot version: extract metadata and pivot to long for faceted violins
cell_meta <- seurat_obj@meta.data |> as_tibble()

cell_meta |>
  pivot_longer(c(nFeature_RNA, nCount_RNA, percent.mt), names_to = "metric") |>
  ggplot(aes(x = metric, y = value)) +
  geom_violin(fill = "lightblue") +
  geom_jitter(width = 0.2, size = 0.5, alpha = 0.5) +
  facet_wrap(~metric, scales = "free") +
  theme_minimal()

# Scatter plots to spot outliers
# Cells with very high counts but few genes (or vice versa) are suspicious
FeatureScatter(seurat_obj, feature1 = "nCount_RNA", feature2 = "nFeature_RNA")
FeatureScatter(seurat_obj, feature1 = "nCount_RNA", feature2 = "percent.mt")

# Custom ggplot version
cell_meta |>
  ggplot(aes(x = nCount_RNA, y = nFeature_RNA)) +
  geom_point(size = 0.5) +
  theme_minimal()

cell_meta |>
  ggplot(aes(x = nCount_RNA, y = percent.mt)) +
  geom_point(size = 0.5) +
  theme_minimal()

# Filter cells based on QC metrics
# Adjust these thresholds to your data
seurat_obj <- subset(
  seurat_obj,
  subset = nFeature_RNA > 5 & nFeature_RNA < 200 & percent.mt < 20
)

# How many cells remain after filtering?
seurat_obj

# ----------------------------------------------------------------------------#
# 3. Normalize ---------------------------------------------------------------
# ----------------------------------------------------------------------------#

# Log-normalize: divide each cell's counts by total counts, multiply by 10000,
# then log-transform. This makes cells comparable despite different library sizes.
seurat_obj <- NormalizeData(seurat_obj)

# ----------------------------------------------------------------------------#
# 4. Find variable features --------------------------------------------------
# ----------------------------------------------------------------------------#

# Identify genes with high cell-to-cell variation — these are most informative
# for downstream analysis (PCA, clustering)
seurat_obj <- FindVariableFeatures(seurat_obj, nfeatures = 200)

# Plot: the top variable genes are labeled
VariableFeaturePlot(seurat_obj)

# See which genes were selected
top10_var <- head(VariableFeatures(seurat_obj), 10)
top10_var

# ----------------------------------------------------------------------------#
# 5. Scale data --------------------------------------------------------------
# ----------------------------------------------------------------------------#

# Scale (z-score) each gene so that highly-expressed genes don't dominate PCA
# This also regresses out unwanted variation if you specify vars.to.regress
seurat_obj <- ScaleData(seurat_obj)

# ----------------------------------------------------------------------------#
# 6. PCA ---------------------------------------------------------------------
# ----------------------------------------------------------------------------#

# Run PCA on the variable features
seurat_obj <- RunPCA(seurat_obj)

# PCA plot — color by identity (all cells are one group here, but with your
# data this will show clusters)
DimPlot(seurat_obj, reduction = "pca") +
  ggtitle("PCA")

# Custom ggplot version: extract PCA embeddings + metadata
pca_data <- Embeddings(seurat_obj, "pca") |>
  as_tibble() |>
  bind_cols(seurat_obj@meta.data |> as_tibble())

ggplot(pca_data, aes(x = PC_1, y = PC_2)) +
  geom_point(size = 1) +
  theme_minimal() +
  ggtitle("PCA (custom ggplot)")

# See which genes drive each PC
print(seurat_obj[["pca"]], dims = 1:3, nfeatures = 5)

# Visualize genes driving the first two PCs
VizDimLoadings(seurat_obj, dims = 1:2, reduction = "pca")

# Heatmap of top genes per PC (useful for understanding what each PC captures)
DimHeatmap(seurat_obj, dims = 1:3, cells = 50, balanced = TRUE)

# ----------------------------------------------------------------------------#
# 7. Elbow plot — choose number of PCs --------------------------------------
# ----------------------------------------------------------------------------#

# The elbow plot shows the % variance explained by each PC
# Look for the "elbow" where the curve flattens — use PCs up to that point
ElbowPlot(seurat_obj)

# Custom ggplot version: extract the standard deviation per PC
stdev_data <- tibble(
  PC = 1:length(seurat_obj[["pca"]]@stdev),
  stdev = seurat_obj[["pca"]]@stdev
)

ggplot(stdev_data, aes(x = PC, y = stdev)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = stdev_data$PC) +
  labs(y = "Standard deviation") +
  theme_minimal() +
  ggtitle("Elbow plot (custom ggplot)")

# For this tiny dataset we'll use 5 PCs
# ADJUST: for real data, typically 10–30 PCs. Pick where the elbow is.
n_pcs <- 5

# ----------------------------------------------------------------------------#
# 8. UMAP -------------------------------------------------------------------
# ----------------------------------------------------------------------------#

# First build a nearest-neighbor graph (needed for both UMAP and clustering)
seurat_obj <- FindNeighbors(seurat_obj, dims = 1:n_pcs)

# Run UMAP for visualization
seurat_obj <- RunUMAP(seurat_obj, dims = 1:n_pcs)

# UMAP plot (no clusters yet, so all cells are the same color)
DimPlot(seurat_obj, reduction = "umap") +
  ggtitle("UMAP (before clustering)")

# Custom ggplot version
umap_data <- Embeddings(seurat_obj, "umap") |>
  as_tibble() |>
  bind_cols(seurat_obj@meta.data |> as_tibble())

ggplot(umap_data, aes(x = umap_1, y = umap_2)) +
  geom_point(size = 0.5) +
  theme_minimal() +
  ggtitle("UMAP (custom ggplot)")

# ----------------------------------------------------------------------------#
# 9. Clustering --------------------------------------------------------------
# ----------------------------------------------------------------------------#

# Find clusters using the Louvain algorithm
# resolution controls granularity: higher = more clusters
# ADJUST resolution to your data (try 0.3–1.2 and see what makes sense)
seurat_obj <- FindClusters(seurat_obj, resolution = 0.5)

# How many clusters were found?
table(Idents(seurat_obj))

# UMAP plot colored by cluster
DimPlot(seurat_obj, reduction = "umap", label = TRUE) +
  ggtitle("UMAP: clusters")

# Custom ggplot version: re-extract embeddings now that clusters exist
umap_data <- Embeddings(seurat_obj, "umap") |>
  as_tibble() |>
  bind_cols(seurat_obj@meta.data |> as_tibble())

ggplot(umap_data, aes(x = umap_1, y = umap_2, color = seurat_clusters)) +
  geom_point(size = 0.5) +
  theme_minimal() +
  ggtitle("UMAP: clusters (custom ggplot)")

# PCA plot colored by cluster (for comparison)
DimPlot(seurat_obj, reduction = "pca", label = TRUE) +
  ggtitle("PCA: clusters")

# Custom ggplot version
pca_data <- Embeddings(seurat_obj, "pca") |>
  as_tibble() |>
  bind_cols(seurat_obj@meta.data |> as_tibble())

ggplot(pca_data, aes(x = PC_1, y = PC_2, color = seurat_clusters)) +
  geom_point(size = 1) +
  theme_minimal() +
  ggtitle("PCA: clusters (custom ggplot)")

# ----------------------------------------------------------------------------#
# 10. Marker genes -----------------------------------------------------------
# ----------------------------------------------------------------------------#

# Find marker genes for each cluster (genes that are differentially expressed
# in one cluster vs. all other cells)
markers <- FindAllMarkers(seurat_obj, only.pos = TRUE, min.pct = 0.1)

# Top 3 markers per cluster
top_markers <- markers |>
  as_tibble() |>
  slice_max(avg_log2FC, n = 3, by = cluster)

top_markers

# Visualize marker expression on UMAP
# ADJUST: pick genes that are interesting for your data
genes_to_plot <- top_markers |>
  slice_max(avg_log2FC, n = 1, by = cluster) |>
  pull(gene) |>
  unique()

FeaturePlot(seurat_obj, features = genes_to_plot, reduction = "umap")

# Custom ggplot version: overlay gene expression on UMAP
# Get normalized expression for the genes of interest and combine with UMAP coords
expr_data <- GetAssayData(seurat_obj, layer = "data")[genes_to_plot, ] |>
  as.matrix() |>
  t() |>
  as_tibble() |>
  bind_cols(
    Embeddings(seurat_obj, "umap") |> as_tibble()
  )

# Plot one gene at a time (example: first gene)
ggplot(
  expr_data,
  aes(x = umap_1, y = umap_2, color = .data[[genes_to_plot[1]]])
) +
  geom_point(size = 0.5) +
  scale_color_viridis_c() +
  theme_minimal() +
  ggtitle(paste("Expression of", genes_to_plot[1], "(custom ggplot)"))

# Or plot all genes at once using pivot_longer + facet_wrap
expr_data |>
  pivot_longer(
    all_of(genes_to_plot),
    names_to = "gene",
    values_to = "expression"
  ) |>
  ggplot(aes(x = umap_1, y = umap_2, color = expression)) +
  geom_point(size = 0.5) +
  scale_color_viridis_c() +
  facet_wrap(~gene) +
  theme_minimal() +
  ggtitle("Marker expression on UMAP (custom ggplot)")

# Violin plot of marker expression across clusters
VlnPlot(seurat_obj, features = genes_to_plot)

# Custom ggplot version
expr_violin <- GetAssayData(seurat_obj, layer = "data")[genes_to_plot, ] |>
  as.matrix() |>
  t() |>
  as_tibble() |>
  bind_cols(seurat_obj@meta.data |> as_tibble())

expr_violin |>
  pivot_longer(
    all_of(genes_to_plot),
    names_to = "gene",
    values_to = "expression"
  ) |>
  ggplot(aes(x = seurat_clusters, y = expression, fill = seurat_clusters)) +
  geom_violin(scale = "width") +
  facet_wrap(~gene, scales = "free_y") +
  theme_minimal() +
  ggtitle("Marker expression by cluster (custom ggplot)")

# Dot plot — shows expression level and % of cells expressing each marker
DotPlot(seurat_obj, features = genes_to_plot) +
  coord_flip() +
  ggtitle("Marker expression by cluster")

# Heatmap of top markers per cluster
# (subset to top 5 per cluster to keep it readable)
top5_markers <- markers |>
  as_tibble() |>
  slice_max(avg_log2FC, n = 5, by = cluster) |>
  pull(gene)

DoHeatmap(seurat_obj, features = top5_markers) +
  ggtitle("Top marker genes per cluster")
