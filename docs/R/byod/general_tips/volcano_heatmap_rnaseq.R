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
#  This dataset contains RNA-seq data from airway smooth muscle cells treated with dexamethasone.
data(airway)
airway
?airway # check what the dataset is about

# Set up the DESeq2 object
dds <- DESeqDataSet(airway, design = ~ cell + dex)

# Run the differential expression analysis
dds <- DESeq(dds)

# Extract normalized counts from the DESeq2 object
dds <- estimateSizeFactors(dds)
normalized_counts <- counts(dds, normalized = TRUE)

# Get the results
res <- results(dds, contrast = c("dex", "trt", "untrt"))

# Convert to a tibble (genes are the rownames of the table so we convert them
# explicitly to a column)
res_tbl <- as.data.frame(res) |> as_tibble(rownames = "gene")

# Heatmap --------------------------------------------------------------------

# Lets do a heatmap of the top 20 differentially expressed genes
top20_genes <- res_tbl |>
  filter(padj < 0.05) |>
  arrange(desc(abs(log2FoldChange))) |>
  head(20) |>
  pull(gene)

# Extract normalized counts for the top 20 genes
normalized_counts_top20 <- normalized_counts[top20_genes, ]

# Convert to a tibble for easier handling
normalized_counts_top20 <- as.data.frame(normalized_counts_top20) |> as_tibble(rownames = "gene")

# Example with ggplot ---------------------------------------------------------
normalized_counts_top20 |>
  # First we need to reshape the data
  pivot_longer(!gene, names_to = "sample") |>
  pivot_wider(names_from = gene, values_from = value) |>
  # Now we need to manually scale the data to plot it
  mutate(across(where(is.double), ~ scale(.) |> as.vector())) |>
  # then we shape it back to a long format
  pivot_longer(where(is.double), names_to = "gene", values_to = "expr_scaled") |>
  ggplot(aes(x = sample, y = gene, fill = expr_scaled)) +
  geom_tile(color = "lightgrey") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white") +
  theme(
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 90),
    axis.title = element_blank()
  )

# Example with pheatmap (easier) ----------------------------------------------
# install.packages("pheatmap")
library(pheatmap)
# pheatmap takes a matrix as input so we need to convert the tibble to a matrix
top_20_genes_matrix <- normalized_counts_top20 %>%
  # select only the columns with the samples
  select(starts_with("SRR")) %>%
  as.matrix()

# add rownames to the matrix with the gene names
rownames(top_20_genes_matrix) <- normalized_counts_top20$gene

heat2 <- top_20_genes_matrix %>%
  pheatmap(
    scale = "row", # scale the data by row
    show_colnames = TRUE,
    show_rownames = TRUE
  )

# or with more options (example from Shaoyi from previous course)
top_20_genes_matrix %>%
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
# Categories for coloring:
# Not sign.: pvalue < 0.01
# Up: p-value < 0.01 & logFC >0
# Down: p-value < 0.01 & logFC < 0

# Basic volcano plot ----------------------------------------------------------
res_tbl |>
  # create a column for the log10 p-value
  mutate(p_log10 = -log10(pvalue)) |>
  # define categories of up- and downregulated genes based on p-value and log2FoldChange
  mutate(
    category = case_when(
      pvalue < 0.01 & log2FoldChange > 0 ~ "Up",
      pvalue < 0.01 & log2FoldChange < 0 ~ "Down",
      TRUE ~ "ns"
    )
  ) |>
  ggplot(aes(
    x = log2FoldChange, y = p_log10, color = category
  )) +
  geom_point()

# More fancy volcano plot with labels and nice colors ------------------------
# Label the top 10 genes using the ggrepel package
# https://ggrepel.slowkow.com/articles/examples.html
library(ggrepel)

# Add labels for some top differentially expressed genes
label_genes <- res_tbl |>
  filter(padj < 0.05) |>
  arrange(desc(abs(log2FoldChange))) |>
  head(10) |>
  pull(gene)

# extract the name of the gene
res_tbl |>
  # create a column for the log10 p-value
  mutate(p_log10 = -log10(pvalue)) |>
  # define categories of up- and downregulated genes based on p-value and log2FoldChange
  mutate(
    category = case_when(
      pvalue < 0.01 & log2FoldChange > 0 ~ "Up",
      pvalue < 0.01 & log2FoldChange < 0 ~ "Down",
      TRUE ~ "ns"
    )
  ) |>
  ggplot(aes(
    x = log2FoldChange, y = p_log10, color = category,
    label = ifelse(gene %in% label_genes, gene, "")
  )) +
  geom_point() +
  geom_label_repel(
    max.overlaps = Inf,
    max.iter = Inf,
    size = 3,
    show.legend = FALSE,
    color = "black"
    # min.segment.length = 0, seed = 42, box.padding = 0.5
  ) +
  geom_hline(yintercept = -log10(0.01), linetype = "dashed") +
  geom_vline(xintercept = c(-1, 1), linetype = "dashed") +
  scale_color_manual(values = c("red", "grey", "blue")) +
  labs(x = "logFC", y = "-log10(p)") +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
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
fviz_pca_ind(pca_res_filtered,
  geom.ind = "point", # Show points only (not "text")
  col.ind = colData(dds)$dex, # Color by treatment
  palette = "jco", # Color palette
  addEllipses = TRUE, # Add concentration ellipses?
  legend.title = "Treatment",
  title = "PCA of Airway Smooth Muscle Cells"
)

# Scree plot: Shows the percentage of variance explained by each principal component
fviz_eig(pca_res_filtered,
         addlabels = TRUE,
         ylim = c(0, 50),
         title = "Scree Plot of PCA"
)

# Biplot: Shows both individuals and variables on the PCA plot
fviz_pca_biplot(pca_res_filtered,
  geom.ind = "point", # Show points only for individuals
  col.ind = colData(dds)$dex, # Color by treatment
  palette = "jco",
  addEllipses = TRUE, # Concentration ellipses
  legend.title = "Treatment",
  title = "PCA Biplot of Airway Data"
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

# Extract the top differentially expressed genes
# Here, we use the top 100 genes for demonstration
sig_genes <- res_tbl %>%
  filter(padj < 0.05) %>%
  arrange(padj) %>%
  head(100) %>%
  pull(gene)

# Convert gene symbols to Entrez IDs
entrez_ids <- bitr(sig_genes, fromType = "ENSEMBL", toType = "ENTREZID", OrgDb = org.Hs.eg.db)

# Perform GO enrichment analysis
#identify biological processes that are overrepresented in the list of differentially expressed genes.
go_enrichment <- enrichGO(gene = entrez_ids$ENTREZID,
                          OrgDb = org.Hs.eg.db,
                          ont = "BP", # Biological Process
                          pAdjustMethod = "BH",
                          pvalueCutoff = 0.05,
                          qvalueCutoff = 0.2,
                          readable = TRUE)

# Visualize the GO enrichment results with a bar plot
# it's a ggplot so you can add more layers to it if you want
barplot_go <- barplot(go_enrichment, showCategory = 10, title = "GO Enrichment Analysis: Biological Process")

# Visualize the GO enrichment results with a dot plot
dotplot_go <- dotplot(go_enrichment, showCategory = 10, title = "GO Enrichment Analysis: Biological Process")
