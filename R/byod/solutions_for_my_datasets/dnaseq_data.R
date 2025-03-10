library(tidyverse)


# This was just for me to prepare -----------------------------------------

# https://galaxyproject.github.io/training-material/topics/transcriptomics/tutorials/rna-seq-viz-with-heatmap2/tutorial.html
# https://training.galaxyproject.org/training-material/topics/transcriptomics/tutorials/rna-seq-viz-with-volcanoplot/tutorial.html#Fu2015
# heatmap_genes <- read_tsv("R/byod/2023_02_tips/dnaseq/heatmap_genes")
# write_csv(heatmap_genes, "R/byod/2023_02_tips/dnaseq/heatmap_genes.csv")
# # Comparing gene expression in luminal cells in pregnant vs. lactating mice
# # includes genes that are not significantly differentially expressed
# # significantly expressed means p<0.01 and abs(log2FC) > 0.58
# DE_results <- read_tsv("R/byod/2023_02_tips/dnaseq/limma-voom_luminalpregnant-luminallactate")
# write_csv(DE_results, "R/byod/2023_02_tips/dnaseq/DE_results.csv")
# Normalized counts for different samples (in columns)
# normalized_counts <- read_tsv("R/byod/2023_02_tips/dnaseq/limma-voom_normalised_counts")
# write_csv(normalized_counts, "R/byod/2023_02_tips/dnaseq/normalized_counts.csv")


# Start analysis ----------------------------------------------------------

heatmap_genes <- read_csv("R/byod/2023_02_tips/dnaseq/heatmap_genes.csv")
DE_results <- read_csv("R/byod/2023_02_tips/dnaseq/DE_results.csv")
DE_results <- janitor::clean_names(DE_results)
normalized_counts <- read_csv("R/byod/2023_02_tips/dnaseq/normalized_counts.csv")
normalized_counts <- janitor::clean_names(normalized_counts)

# combine tables
genes <- left_join(DE_results, normalized_counts, by = c("entrezid", "symbol", "genename")) %>%
  select(-entrezid, genename)

# only significant genes
significant_genes <- genes %>% filter(p_value < 0.01 & abs(log_fc) > 0.58)

# Select the top 20 genes to plot, by p-value
top_20_genes <- significant_genes %>%
  arrange(p_value) %>%
  dplyr::slice(1:20)


# Heatmap of top genes ----------------------------------------------------

# Create a heatmap of the top genes
heat_top_20 <- top_20_genes %>%
  mutate(symbol = fct_reorder(symbol, -log_fc)) %>%
  select(c(symbol, starts_with("mcl1"))) %>%
  pivot_longer(starts_with("mcl"), names_to = "sample") %>%
  pivot_wider(names_from = symbol, values_from = value) %>%
  mutate(across(where(is.double), ~ scale(.) %>% as.vector())) %>% # normalize the data
  pivot_longer(where(is.double), names_to = "gene", values_to = "expr_scaled") %>%
  ggplot(aes(x = sample, y = gene, fill = expr_scaled)) +
  geom_tile(color = "lightgrey") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white") +
  theme(
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 90),
    axis.title = element_blank()
  )
# ggsave("img/heat_top20.png")

# Heatmap of interesting genes --------------------------------------------
# Filter normalized counts to contain only the genes interesting for the plot
genes %>%
  filter(symbol %in% heatmap_genes$GeneID) %>%
  pivot_longer(starts_with("mcl"), names_to = "sample") %>%
  select(symbol, sample, value) %>%
  pivot_wider(names_from = symbol, values_from = value) %>%
  mutate(across(where(is.double), ~ scale(.) %>% as.vector())) %>% # normalize the data
  pivot_longer(where(is.double), names_to = "gene", values_to = "expr_scaled") %>%
  ggplot(aes(x = gene, y = sample, fill = expr_scaled)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white") +
  theme(
    axis.title = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.1, hjust = 1)
  )

# Use pheatmap instead ------------------------------------------------
library(pheatmap)

# this takes quite long because there are so many genes
# normalized_counts %>%
#   select(starts_with("mcl1")) %>%
#   as.matrix() %>%
#   pheatmap(scale = "row")

top_20_genes_matrix <- top_20_genes %>%
  arrange(desc(log_fc)) %>%
  select(starts_with("mcl1")) %>%
  as.matrix()

rownames(top_20_genes_matrix) <- top_20_genes$symbol

heat2 <- top_20_genes_matrix %>%
  pheatmap(
    scale = "row",
    show_colnames = TRUE,
    show_rownames = TRUE
  )

#ggsave("img/heat_top20_2.png", heat2)

# PCA to see clustering ---------------------------------------------------
pcres <- top_20_genes %>%
  select(starts_with("mcl1")) %>%
  prcomp()

factoextra::fviz_pca_ind(pcres,
  col.ind = "cos2", geom = "point",
  gradient.cols = c("white", "#2E9FDF", "#FC4E07")
)
# Volcano plot ----------------------------------------------------------------
# https://training.galaxyproject.org/training-material/topics/transcriptomics/tutorials/rna-seq-viz-with-volcanoplot/tutorial.html#Fu2015
# Categories for coloring:
# Not sign.: pvalue < 0.01
# Up: p-value < 0.01 & logFC >0
# Down: p-value < 0.01 & logFC < 0

genes %>%
  mutate(p_log10 = -log10(p_value)) %>%
  mutate(
    category = case_when(
      p_value < 0.01 & log_fc > 0 ~ "Up",
      p_value < 0.01 & log_fc < 0 ~ "Down",
      TRUE ~ "ns"
    )
  ) %>%
  ggplot(aes(
    x = log_fc, y = p_log10, color = category
  )) +
  geom_point()

# Label the top 10 genes
# https://ggrepel.slowkow.com/articles/examples.html
library(ggrepel)
labels <- top_20_genes$symbol[1:10]

DE_results %>%
  mutate(p_log10 = -log10(p_value)) %>%
  mutate(
    category = case_when(
      p_value < 0.01 & log_fc > 0 ~ "Up",
      p_value < 0.01 & log_fc < 0 ~ "Down",
      TRUE ~ "ns"
    )
  ) %>%
  ggplot(aes(
    x = log_fc, y = p_log10, color = category,
    label = ifelse(symbol %in% labels, symbol, "")
  )) +
  geom_point() +
  geom_text_repel(min.segment.length = 0, seed = 42, box.padding = 0.5) +
  geom_hline(yintercept = -log10(0.01), linetype = "dashed") +
  geom_vline(xintercept = c(-1, 1), linetype = "dashed") +
  scale_color_manual(values = c("red", "grey", "blue"))


# label genes of interest -------------------------------------------------
labels <- heatmap_genes$GeneID

volc <- DE_results %>%
  mutate(p_log10 = -log10(p_value)) %>%
  mutate(
    category = case_when(
      p_value < 0.01 & log_fc > 0 ~ "Up",
      p_value < 0.01 & log_fc < 0 ~ "Down",
      TRUE ~ "ns"
    )
  ) %>%
  ggplot(aes(
    x = log_fc, y = p_log10, color = category,
    label = ifelse(symbol %in% labels, symbol, "")
  )) +
  geom_point(alpha = 0.3) +
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
#ggsave("img/volcano.png", volc)
