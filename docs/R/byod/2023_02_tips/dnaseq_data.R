library(tidyverse)
# https://galaxyproject.github.io/training-material/topics/transcriptomics/tutorials/rna-seq-viz-with-heatmap2/tutorial.html
heatmap_genes <- read_tsv("R/byod/2023_02_tips/dnaseq/heatmap_genes")
# Comparing gene expression in luminal cells in pregnant vs. lactating mice
# includes genes that are not significantly differentially expressed
# significantly expressed means p<0.01 and abs(log2FC) > 0.58
DE_results <- read_tsv("R/byod/2023_02_tips/dnaseq/limma-voom_luminalpregnant-luminallactate")

# Normalized counts for different samples (in columns)
normalized_counts <- read_tsv("R/byod/2023_02_tips/dnaseq/limma-voom_normalised_counts")

# only significant genes
significant_genes <- DE_results %>% filter(P.Value < 0.01 & abs(logFC) > 0.58)

# Select the top 20 genes to plot, by p-value
top20_genes <- significant_genes %>%
  arrange(P.Value) %>%
  dplyr::slice(1:20)

# Get normalized counts for the top 20 genes (join on ENTREZID)
top_20_genes <- top20_genes %>% left_join(normalized_counts,
  by = c("ENTREZID", "SYMBOL", "GENENAME")
)

# Create a heatmap of the top genes
top_20_genes %>%
  pivot_longer(starts_with("MCL1."),
    names_to = "sample",
    values_to = "value"
  ) %>%
  ggplot(aes(x = sample, y = SYMBOL, fill = value)) +
  geom_tile(color = "lightgrey") +
  scale_fill_gradient2()

