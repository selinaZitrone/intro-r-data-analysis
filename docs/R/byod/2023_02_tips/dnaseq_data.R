library(tidyverse)
# https://galaxyproject.github.io/training-material/topics/transcriptomics/tutorials/rna-seq-viz-with-heatmap2/tutorial.html
# https://training.galaxyproject.org/training-material/topics/transcriptomics/tutorials/rna-seq-viz-with-volcanoplot/tutorial.html#Fu2015
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
  mutate(SYMBOL = fct_reorder(SYMBOL, -logFC)) %>%
  select(c(SYMBOL, starts_with("MCL1"))) %>%
  pivot_longer(starts_with("MCL"), names_to = "sample") %>%
  pivot_wider(names_from = SYMBOL, values_from = value) %>%
  mutate(across(where(is.double), ~ scale(.) %>% as.vector)) %>%
#  mutate(across(starts_with("MCL1"), ~ .x / mean_value)) %>%
  pivot_longer(where(is.double), names_to = "gene", values_to = "expr_scaled") %>%
  ggplot(aes(x = sample, y = gene, fill = expr_scaled)) +
  geom_tile(color = "lightgrey") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white") +
  theme(legend.title = element_blank())

# Heatmap of interesting genes --------------------------------------------
# Filter normalized counts to contain only the genes interesting for the plot
normalized_counts %>%
  select(-ENTREZID, -GENENAME) %>%
  filter(SYMBOL %in% heatmap_genes$GeneID) %>%
    pivot_longer(starts_with("MCL"), names_to = "sample") %>%
    pivot_wider(names_from = SYMBOL, values_from = value) %>%
    mutate(across(where(is.double), ~ scale(.) %>% as.vector)) %>%
  pivot_longer(where(is.double), names_to = "gene", values_to = "expr_scaled") %>%
  ggplot(aes(x = gene, y = sample, fill = expr_scaled)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white") +
  theme(axis.title = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.1, hjust = 1))


# Use pheatmap instead ------------------------------------------------
library(pheatmap)
# this takes quite long because there are so many genes
normalized_counts %>%
  select(starts_with("MCL1")) %>% as.matrix() %>%
  pheatmap(scale = "row")

top_20_genes_matrix <- top_20_genes %>%
  arrange(des(logFC)) %>%
  select(starts_with("MCL1")) %>%
  as.matrix()
rownames(top_20_genes_matrix) <- top_20_genes$SYMBOL

top_20_genes_matrix %>%
pheatmap(scale = "row",
           show_colnames = TRUE,
           show_rownames = TRUE)


# PCA to see clustering ---------------------------------------------------
library(ggfortify)
top_20_genes %>%
  select(starts_with("MCL1")) %>%
  prcomp() %>%
  autoplot()

# Volcano plot ----------------------------------------------------------------
# https://training.galaxyproject.org/training-material/topics/transcriptomics/tutorials/rna-seq-viz-with-volcanoplot/tutorial.html#Fu2015
# Categories for coloring:
# Not sign.: pvalue < 0.01
# Up: p-value < 0.01 & logFC >0
# Down: p-value < 0.01 & logFC < 0


DE_results %>%
  mutate(p_log10 = -log10(P.Value)) %>%
  mutate(
    category = case_when(
      P.Value < 0.01 & logFC > 0 ~ "Up",
      P.Value < 0.01 & logFC < 0 ~ "Down",
      TRUE ~ "ns"
    )
  ) %>%
  ggplot(aes(
    x = logFC, y = p_log10, color = category
  )) +
  geom_point()

# Label the top 10 genes
# https://ggrepel.slowkow.com/articles/examples.html
library(ggrepel)
labels <- top_20_genes$SYMBOL[1:10]

DE_results %>%
  mutate(p_log10 = -log10(P.Value)) %>%
  mutate(
    category = case_when(
      P.Value < 0.01 & logFC > 0 ~ "Up",
      P.Value < 0.01 & logFC < 0 ~ "Down",
      TRUE ~ "ns"
    )
  ) %>%
  ggplot(aes(
    x = logFC, y = p_log10, color = category,
    label = ifelse(SYMBOL %in% labels, SYMBOL, "")
  )) +
  geom_point() +
  geom_text_repel(min.segment.length = 0, seed = 42, box.padding = 0.5) +
  geom_hline(yintercept = -log10(0.01), linetype = "dashed") +
  geom_vline(xintercept = c(-1, 1), linetype = "dashed") +
  scale_color_manual(values = c("red", "grey", "blue"))


# label genes of interest -------------------------------------------------
labels <- heatmap_genes$GeneID

DE_results %>%
  mutate(p_log10 = -log10(P.Value)) %>%
  mutate(
    category = case_when(
      P.Value < 0.01 & logFC > 0 ~ "Up",
      P.Value < 0.01 & logFC < 0 ~ "Down",
      TRUE ~ "ns"
    )
  ) %>%
  ggplot(aes(
    x = logFC, y = p_log10, color = category,
    label = ifelse(SYMBOL %in% labels, SYMBOL, "")
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
  theme(legend.position = "bottom")



# Use pheatmap package ----------------------------------------------------
# https://compgenomr.github.io/book/gene-expression-analysis-using-high-throughput-sequencing-technologies.html


# colorectal cancer
counts_file <- system.file("extdata/rna-seq/SRP029880.raw_counts.tsv",
  package = "compGenomRData"
)
coldata_file <- system.file("extdata/rna-seq/SRP029880.colData.tsv",
  package = "compGenomRData"
)

colddata <- read.table(coldata_file, header = T, sep = "\t") %>% as_tibble()

counts <- read_table(counts_file)
counts <- as.matrix(read.table(counts_file, header = T, sep = "\t"))

counts <- as.data.frame(counts) %>%
  rownames_to_column(var = "Gene") %>%
  as_tibble()

write_tsv(counts, file = "R/byod/2023_02_tips/dnaseq/raw_counts.tsv")

counts <- read_tsv("R/byod/2023_02_tips/dnaseq/raw_counts.tsv")
# normalize columns:
counts_normalized <- counts %>%
  mutate(across(where(is.numeric), ~ .x / sum(.x) * 10^6))

# are the colsums all 10^6 now? Yes
counts_normalized %>%
  select(-Gene, -width) %>%
  summarize_all(sum)



counts2 <- as.matrix(read.table(counts_file, header = T, sep = "\t"))
cpm <- apply(
  subset(counts2, select = c(-width)), 2,
  function(x) x / sum(as.numeric(x)) * 10^6
)
