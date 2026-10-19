library(tidyverse)

# Read and combine the data ---------------------------------------------------

# ADJUST the paths to where you saved the files
heatmap_genes <- read_tsv("data/heatmap_genes")
DE_results <- read_tsv("data/limma-voom_luminalpregnant-luminallactate")
DE_results <- janitor::clean_names(DE_results)
normalized_counts <- read_tsv("data/limma-voom_normalised_counts")
normalized_counts <- janitor::clean_names(normalized_counts)

# combine the DE results and the normalized counts into one table
genes <- left_join(
  DE_results,
  normalized_counts,
  by = c("entrezid", "symbol", "genename")
) |>
  # remove columns we don't need
  select(-entrezid, -genename)
genes

# Sample information ----------------------------------------------------------

# The sample names (e.g. mcl1_dg) don't tell us which group a sample belongs
# to, so we create a table with this information.
# There are 6 groups with 2 replicates each: basal and luminal cells from
# virgin, pregnant and lactating mice
sample_table <- tibble(
  sample = c(
    "mcl1_dg",
    "mcl1_dh",
    "mcl1_di",
    "mcl1_dj",
    "mcl1_dk",
    "mcl1_dl",
    "mcl1_la",
    "mcl1_lb",
    "mcl1_lc",
    "mcl1_ld",
    "mcl1_le",
    "mcl1_lf"
  ),
  cell_type = rep(c("basal", "luminal"), each = 6),
  condition = rep(c("virgin", "pregnant", "lactating"), each = 2, times = 2)
)
sample_table

# Note: the DE results compare luminal pregnant (lc, ld) vs. luminal
# lactating (le, lf) samples:
# log_fc > 0: higher expression in pregnant
# log_fc < 0: higher expression in lactating

# Thresholds for significant genes --------------------------------------------

# Following the Galaxy tutorial, a gene counts as significantly up- or
# downregulated if
# 1. its adjusted p-value is below 0.01 AND
# 2. its absolute log2 fold change is above 0.58 (i.e. 1.5-fold change)
# I save these 2 values in variables as I use them for the heatmaps and the
# volcano plots
padj_cutoff <- 0.01
lfc_cutoff <- 0.58

# Top 20 genes ----------------------------------------------------------------

top_20_genes <- genes |>
  # Keep only significant genes (see thresholds above)
  filter(adj_p_val < padj_cutoff & abs(log_fc) > lfc_cutoff) |>
  # Arrange by adjusted p-value (most significant genes first)
  arrange(adj_p_val) |>
  # Just keep the top 20 genes
  head(20)
top_20_genes

# Heatmap of the top 20 genes -------------------------------------------------

# install.packages("pheatmap")
library(pheatmap)

# The normalized counts from limma-voom are already on a log2 scale
# (log2 counts per million), so we can use them directly for the heatmap.

# pheatmap takes a matrix as input with the gene names as rownames
top_20_matrix <- top_20_genes |>
  # select the gene names and the columns with the samples
  select(symbol, starts_with("mcl1")) |>
  # use the gene names as rownames (this keeps names and values together)
  column_to_rownames("symbol") |>
  # convert to matrix
  as.matrix()
top_20_matrix

# Sample annotation: colored bars above the heatmap that show cell type and
# condition. pheatmap needs a data.frame with the sample names as rownames
sample_annotation <- sample_table |>
  column_to_rownames("sample")

# Heatmap with all samples
top_20_matrix |>
  pheatmap(
    scale = "row", # scale the data by row
    annotation_col = sample_annotation # add the cell type and condition bars
  )

# Heatmap with only the samples that were compared in the DE analysis
# (luminal pregnant vs. luminal lactating)
compared_samples <- sample_table |>
  filter(cell_type == "luminal" & condition != "virgin") |>
  pull(sample)
compared_samples

heat_top_20 <- top_20_matrix[, compared_samples] |>
  pheatmap(
    scale = "row",
    # all 4 samples are luminal, so only show the condition
    annotation_col = select(sample_annotation, condition),
    cutree_cols = 2, # split samples into 2 groups (pregnant vs lactating)
    main = "Top 20 genes: luminal pregnant vs. lactating"
  )
# ggsave("img/heat_top20_2.png", heat_top_20)

# Heatmap of the genes of interest --------------------------------------------

# Genes from Figure 6b of the paper (heatmap_genes)
genes_of_interest_matrix <- genes |>
  filter(symbol %in% heatmap_genes$GeneID) |>
  select(symbol, starts_with("mcl1")) |>
  column_to_rownames("symbol") |>
  as.matrix()

genes_of_interest_matrix |>
  pheatmap(
    scale = "row",
    annotation_col = sample_annotation,
    main = "Genes of interest"
  )

# PCA to see clustering of the samples ----------------------------------------

library(factoextra)

# PCA on the top 20 genes
# prcomp() needs the samples in rows and the genes in columns, so we transpose
# the matrix with t()
pca_res <- prcomp(t(top_20_matrix), scale. = TRUE)

# The columns of the matrix must be in the same order as the rows of the
# sample table so that the colors are correct. This should be TRUE
all(colnames(top_20_matrix) == sample_table$sample)

fviz_pca_ind(
  pca_res,
  geom.ind = "point", # show points only (not the sample names)
  col.ind = paste(sample_table$cell_type, sample_table$condition), # color by group
  palette = "jco", # color palette
  pointsize = 3,
  legend.title = "Group"
)

# Volcano plot ----------------------------------------------------------------
# https://training.galaxyproject.org/training-material/topics/transcriptomics/tutorials/rna-seq-viz-with-volcanoplot/tutorial.html

# A volcano plot shows all genes with
# x-axis: log2 fold change (how much does the expression change?)
# y-axis: -log10 of the adjusted p-value (how significant is the change?
#         the higher, the more significant)

# Categories for coloring (using the thresholds defined above):
# Up: adj_p_val < padj_cutoff & log_fc > lfc_cutoff (higher in pregnant)
# Down: adj_p_val < padj_cutoff & log_fc < -lfc_cutoff (higher in lactating)
# ns (not significant): all other genes

# Prepare the data for all volcano plots
volcano_data <- genes |>
  # create a column for the -log10 adjusted p-value
  mutate(padj_log10 = -log10(adj_p_val)) |>
  # define categories of up- and downregulated genes
  mutate(
    category = case_when(
      adj_p_val < padj_cutoff & log_fc > lfc_cutoff ~ "Up",
      adj_p_val < padj_cutoff & log_fc < -lfc_cutoff ~ "Down",
      .default = "ns"
    )
  )
volcano_data

# How many genes are significantly up- or downregulated?
dplyr::count(volcano_data, category)

# Basic volcano plot ----------------------------------------------------------
volcano_data |>
  ggplot(aes(
    x = log_fc,
    y = padj_log10,
    color = category
  )) +
  geom_point()

# Nicer volcano plot with colors and threshold lines --------------------------

# Save the plot in a variable so we can add labels to it in the next steps
volcano_plot <- volcano_data |>
  ggplot(aes(
    x = log_fc,
    y = padj_log10,
    color = category
  )) +
  geom_point(alpha = 0.3) +
  # dashed lines show the thresholds (same as used for the categories)
  # horizontal line
  geom_hline(yintercept = -log10(padj_cutoff), linetype = "dashed") +
  # 2 vertical lines
  geom_vline(xintercept = c(-lfc_cutoff, lfc_cutoff), linetype = "dashed") +
  # name the colors so each category gets the right color
  scale_color_manual(values = c(Up = "red", Down = "blue", ns = "grey")) +
  labs(x = "log2 fold change", y = "-log10(adjusted p-value)") +
  theme_bw() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  )
volcano_plot

# Label the top 10 genes ------------------------------------------------------
# Label genes using the ggrepel package (labels don't overlap)
# https://ggrepel.slowkow.com/articles/examples.html
library(ggrepel)

# Select the genes to label: the 10 most significant up- or downregulated genes
label_data <- volcano_data |>
  # keep only significant genes
  filter(category != "ns") |>
  # most significant genes first
  arrange(adj_p_val) |>
  head(10)
label_data

# Add the labels to the saved plot
# data = label_data: only the genes in label_data get a label
volcano_plot +
  geom_label_repel(
    data = label_data,
    aes(label = symbol),
    max.overlaps = Inf,
    size = 3,
    show.legend = FALSE,
    color = "black"
  )

# Label the genes of interest -------------------------------------------------

# Genes from Figure 6b of the paper (heatmap_genes)
label_data_interest <- volcano_data |>
  filter(symbol %in% heatmap_genes$GeneID)

volc <- volcano_plot +
  geom_label_repel(
    data = label_data_interest,
    aes(label = symbol),
    max.overlaps = Inf,
    size = 3,
    show.legend = FALSE,
    color = "black"
  )
volc
# ggsave("img/volcano.png", volc)
