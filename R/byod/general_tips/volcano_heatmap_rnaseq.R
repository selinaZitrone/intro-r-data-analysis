# Install and load required packages ------------------------------------------
# Install packages from bioconductor  (this may take some time)
# if (!requireNamespace("BiocManager", quietly = TRUE)) {
#   install.packages("BiocManager")
# }
# BiocManager::install(c("airway", "DESeq2"), update = FALSE, ask = FALSE)

library(airway)
library(DESeq2)
library(tidyverse)

# Prepare the data ------------------------------------------------------------

# Load the airway dataset
# This dataset contains raw counts per genes from airway smooth muscle cells
# treated with dexamethasone.
data(airway)
?airway # check what the dataset is about

# Usually, your data comes as tables (e.g. Excel). To show how to start from
# such tables, I convert the airway data into two simple tables:
# count_table: one row per gene, one column per sample
# sample_table: one row per sample with the experimental conditions
count_table <- as_tibble(assay(airway), rownames = "gene")
sample_table <- as_tibble(colData(airway), rownames = "sample") |>
  select(sample, cell, dex)
count_table
sample_table

# With your own data, you would read these tables from your own data (e.g. excel)
# instead, e.g.:
# count_table <- readxl::read_excel("data/my_counts.xlsx")
# sample_table <- readxl::read_excel("data/my_samples.xlsx")

# Note: DESeq2 needs raw counts (whole numbers), not normalized values,
# because it does the normalization itself.
# If you already have normalized values and DE results
# (log2 fold change, p-values), use the script from the RNA-seq example
# dataset instead (dnaseq_data.R), which starts from these tables.

# DESeq2 needs the counts as a matrix with the gene names as row names
count_matrix <- count_table |>
  column_to_rownames("gene") |>
  as.matrix()

# The columns of the count matrix must be in the same order as the rows of the
# sample table. Here I check this (result should be TRUE)
all(colnames(count_matrix) == sample_table$sample)

# Set up the DESeq2 object
# design: how do the counts for each gene depend on the variables in the
# sample table? In this example cell type and treatment (dexamethasone or not)
dds <- DESeqDataSetFromMatrix(
  countData = count_matrix,
  colData = sample_table,
  design = ~ cell + dex
)

# Run the differential expression analysis
# to calculate log2 fold changes, p-values, and adjusted p-values
dds <- DESeq(dds)

# Extract normalized counts from the DESeq2 object for later use
normalized_counts <- counts(dds, normalized = TRUE)

# Extract the results of the differential expression analysis
res <- results(dds, contrast = c("dex", "trt", "untrt"))

# Convert both to tibble for easier handling
# (genes are the rownames of the table so we convert them
# explicitly to a column)
normalized_counts <- as_tibble(normalized_counts, rownames = "gene")
res <- as_tibble(res, rownames = "gene")

# Look at the data
normalized_counts
res

# Thresholds for significant genes --------------------------------------------

# In this script I decided the following (this might be different for your
# analyses, so adjust if needed):
# A gene counts as significantly up- or downregulated if
# 1. its adjusted p-value is below padj_cutoff of 0.05 AND
# 2. its absolute log2 fold change is above 1 (i.e. 2-fold change)
# I save these 2 values in variables as I will use them both for the
# heatmap and the volcano plot

padj_cutoff <- 0.05
lfc_cutoff <- 1

# Heatmap --------------------------------------------------------------------

# Lets do a heatmap of the top 20 differentially expressed genes
top20_genes <- res |>
  # Keep only significant genes (see thresholds above)
  filter(padj < padj_cutoff & abs(log2FoldChange) > lfc_cutoff) |>
  # Arrange by adjusted p-value (most significant genes first)
  arrange(padj) |>
  # Just look at the top 20 genes
  head(20) |>
  # pull the gene names out of the table
  pull(gene)

# Look at the top 20 genes
top20_genes

# Create a new table that only contains these top 20 genes
normalized_counts_top20 <- normalized_counts |>
  filter(gene %in% top20_genes)

# Heatmap with pheatmap ----------------------------------------------

# install.packages("pheatmap")
library(pheatmap)

# pheatmap takes a matrix as input so we need to convert the tibble to a matrix
top_20_genes_matrix <- normalized_counts_top20 |>
  # select only the columns with the samples
  select(starts_with("SRR")) |>
  # convert to matrix
  as.matrix()

# add rownames to the matrix with the gene names
rownames(top_20_genes_matrix) <- normalized_counts_top20$gene

# Sample annotation: a colored bar above the heatmap that shows the treatment
# pheatmap needs a data.frame with the sample names as rownames
sample_annotation <- sample_table |>
  select(sample, dex) |>
  column_to_rownames("sample")

# Basic heatmpa
heat2 <- top_20_genes_matrix %>%
  pheatmap(
    scale = "row", # scale the data by row
    annotation_col = sample_annotation, # add the treatment bar
    show_colnames = TRUE,
    show_rownames = TRUE
  )

# or with more options (example from Shaoyi from previous course)
top_20_genes_matrix |>
  pheatmap(
    scale = "row",
    border = "#8B0A50",
    cluster_cols = TRUE,
    cluster_rows = TRUE,
    show_colnames = TRUE,
    show_rownames = TRUE,
    legend = TRUE,
    legend_breaks = c(-2, 0, 2),
    fontsize_row = 8,
    fontsize_col = 10,
    treeheight_row = 35,
    treeheight_col = 40,
    clustering_distance_rows = "minkowski",
    clustering_method = "complete",
    angle_col = 270,
    main = "Genes of interest",
    # cellwidth = 20,
    # cellheight = 40,
    cutree_rows = 6,
    cutree_cols = 7,
    display_numbers = TRUE,
    fontsize_number = 6,
    number_color = "red",
    number_format = "%.2f"
    # filename = "documents/img/genes of interest.png" # to save the plot if you want
  )

# Volcano plot ----------------------------------------------------------------
# https://training.galaxyproject.org/training-material/topics/transcriptomics/tutorials/rna-seq-viz-with-volcanoplot/tutorial.html#Fu2015

# A volcano plot shows all genes with
# x-axis: log2 fold change (how much does the expression change?)
# y-axis: -log10 of the adjusted p-value (how significant is the change?
#         the higher, the more significant)

# Categories for coloring (using the thresholds defined above):
# Up: padj < padj_cutoff & log2FoldChange > lfc_cutoff
# Down: padj < padj_cutoff & log2FoldChange < -lfc_cutoff
# ns (not significant): all other genes

# Prepare the data for both volcano plots
volcano_data <- res |>
  # create a column for the -log10 adjusted p-value
  mutate(padj_log10 = -log10(padj)) |>
  # define categories of up- and downregulated genes
  mutate(
    category = case_when(
      padj < padj_cutoff & log2FoldChange > lfc_cutoff ~ "Up",
      padj < padj_cutoff & log2FoldChange < -lfc_cutoff ~ "Down",
      .default = "ns"
    )
  )
volcano_data

# Basic volcano plot ----------------------------------------------------------
# Genes without adjusted p-value (NA) are removed with a warning. These are
# genes that DESeq2 filtered out (e.g. because of very low counts)
volcano_data |>
  ggplot(aes(
    x = log2FoldChange,
    y = padj_log10,
    color = category
  )) +
  geom_point()

# Nicer volcano plot with colors and threshold lines --------------------------

# Save the plot in a variable so we can add labels to it in the next step
volcano_plot <- volcano_data |>
  ggplot(aes(
    x = log2FoldChange,
    y = padj_log10,
    color = category
  )) +
  geom_point(alpha = 0.4) +
  # dashed lines show the thresholds (same as used for the categories)
  # horizontal line
  geom_hline(yintercept = -log10(padj_cutoff), linetype = "dashed") +
  # 2 vertical lines
  geom_vline(xintercept = c(-lfc_cutoff, lfc_cutoff), linetype = "dashed") +
  # name the colors so each category gets the right color
  scale_color_manual(values = c(Up = "red", Down = "blue", ns = "grey")) +
  labs(x = "log2 fold change", y = "-log10(adjusted p-value)") +
  theme_classic() +
  theme(
    # move legend to the top right of the plot
    legend.position = "inside",
    legend.position.inside = c(0.9, 0.9),
    legend.title = element_blank()
  )

volcano_plot

# Add labels for the top genes -------------------------------------------------
# Label the top 10 genes using the ggrepel package (labels don't overlap)
# https://ggrepel.slowkow.com/articles/examples.html
library(ggrepel)

# Select the genes to label: the 10 most significant up- or downregulated genes
label_data <- volcano_data |>
  # keep only significant genes
  filter(category != "ns") |>
  # most significant genes first
  arrange(padj) |>
  head(10)
label_data

# Add the labels to the saved plot
# data = label_data: only the genes in label_data get a label
volcano_plot +
  geom_label_repel(
    data = label_data,
    aes(label = gene),
    max.overlaps = Inf,
    size = 3,
    show.legend = FALSE,
    color = "black"
  )

# PCA analysis ----------------------------------------------------------------
# Package to do the PCA plots
# https://rpkgs.datanovia.com/factoextra/index.html
library(factoextra)
# Variance stabilizing transformation (reduce heteroscedasticity)
vsd <- vst(dds, blind = FALSE)

# Filter out genes with zero variance across samples
# otherwise you get an error in the PCA because it cannot rescale if variance
# is zero
non_zero_variance_genes <- apply(assay(vsd), 1, var) > 0
vsd_filtered <- vsd[non_zero_variance_genes, ]

# PCA operates on the covariance matrix of the data, which requires samples to
# be in rows and features (genes) in columns. Transposing ensures that each
# sample is treated as an observation and each gene as a variable
# assay() retrieves the data as a table
# t() transposes the table to have the genes as columns and the samples as
# observations
vsd_filtered_for_pca <- t(assay(vsd_filtered))

# Perform PCA using prcomp on the prepared data
pca_res_filtered <- prcomp(vsd_filtered_for_pca, scale. = TRUE)

# Visualize the PCA using factoextra using different options from the
# factoextra package
fviz_pca_ind(
  pca_res_filtered,
  geom.ind = "point", # Show points only (not "text")
  col.ind = colData(dds)$dex, # Color by treatment
  palette = "jco", # Color palette
  addEllipses = TRUE, # Add concentration ellipses?
  legend.title = "Treatment",
  title = "PCA of Airway Smooth Muscle Cells"
)

# Scree plot: Shows the percentage of variance explained by each principal component
fviz_eig(
  pca_res_filtered,
  addlabels = TRUE,
  ylim = c(0, 50),
  title = "Scree Plot of PCA"
)

# Save the PCA plot as a PNG file
# ggsave("airway_pca_factoextra_filtered.png", pca_plot_filtered, width = 10, height = 8, dpi = 300)

# Gene ontology plots --------------------------------------------------------
# install and load the packages if needed
# BiocManager::install("clusterProfiler")
# BiocManager::install("org.Hs.eg.db")
library(clusterProfiler)
# This package provides mappings between different gene identifiers for human genes.
library(org.Hs.eg.db)

# Define universe = all tested genes with non-NA padj (mapped to Entrez)
universe_map <- bitr(
  res$gene,
  fromType = "ENSEMBL",
  toType = "ENTREZID",
  OrgDb = org.Hs.eg.db
)

universe_entrez <- unique(universe_map$ENTREZID)

# Define significant gene set
sig_genes <- res |>
  filter(padj < 0.05) |>
  pull(gene)

# Convert gene symbols to Entrez IDs
significant_map <- bitr(
  sig_genes,
  fromType = "ENSEMBL",
  toType = "ENTREZID",
  OrgDb = org.Hs.eg.db
)

sig_entrez <- unique(significant_map$ENTREZID)

# Perform GO enrichment analysis
#identify biological processes that are overrepresented in the list of differentially expressed genes.
# takes a little while to run
go_enrichment <- enrichGO(
  gene = sig_entrez,
  universe = universe_entrez,
  OrgDb = org.Hs.eg.db,
  keyType = "ENTREZID",
  ont = "BP", # Biological Process
  pAdjustMethod = "BH",
  pvalueCutoff = 0.05,
  qvalueCutoff = 0.2,
  readable = TRUE
)

# Visualize the GO enrichment results with a bar plot
# it's a ggplot so you can add more layers to it if you want
barplot_go <- barplot(
  go_enrichment,
  showCategory = 10,
  title = "GO Enrichment Analysis: Biological Process"
)

# Visualize the GO enrichment results with a dot plot
dotplot_go <- dotplot(
  go_enrichment,
  showCategory = 10,
  title = "GO Enrichment Analysis: Biological Process"
)
