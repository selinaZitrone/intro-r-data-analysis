library(tidyverse)
library(janitor)
library(pheatmap)
library(ggrepel)
Sys.setenv(LANGUAGE = "en")

# read in the data
de_results <- read_csv("data/DE_results.csv")
heatmap_genes <- read_csv("data/heatmap_genes.csv")
normalized_counts <- read_csv("data/normalized_counts.csv")

# make the column headers nicer
de_results <- clean_names(de_results)
heatmap_genes <- clean_names(heatmap_genes)
normalized_counts <- clean_names(normalized_counts)

# join de_results and normalized_counts by their shared columns
join_de_counts <- full_join(de_results,
  normalized_counts,
  by = c("entrezid", "symbol", "genename")
)

# remove columns don't need for analysis
join_de_counts <- select(join_de_counts, -genename)

# extract the significant genes
significant_genes <- filter(
  join_de_counts,
  adj_p_val < 0.01 & log_fc > 0.58
)

# extract the top significant genes by P value
top_20_by_p <- arrange(significant_genes, p_value)
top_20_by_p <- filter(top_20_by_p, row_number() <= 20)
top_20_by_p <- select(
  top_20_by_p, -entrezid, -log_fc,
  -ave_expr, -t, -p_value, -adj_p_val
)

# Convert the values in first column into row names
top_20_by_p <- top_20_by_p %>%
  remove_rownames() %>%
  column_to_rownames(var = "symbol")

# convert tibble into matrix
top_20_by_p <- as.matrix(top_20_by_p)

# plot the heatmap of top 20 genes
p1 <- pheatmap(top_20_by_p,
  scale = "row",
  border = "#8B0A50",
  cluster_cols = T,
  cluster_rows = T,
  show_colnames = T,
  show_rownames = T,
  legend = T,
  legend_breaks = c(-2, 0, 2),
  fontsize_row = 8,
  fontsize_col = 10,
  treeheight_row = 45,
  treeheight_col = 50,
  clustering_distance_rows = "minkowski",
  clustering_method = "complete",
  angle_col = 270,
  main = "Top 20 most significant genes",
  cellwidth = 40, cellheight = 20,
  cutree_rows = 4, cutree_cols = 5,
  display_numbers = T, fontsize_number = 8,
  number_color = "red", number_format = "%.2f"
  #filename = "documents/img/top 20 genes.png"
)

# extract the normalized counts for the genes of interest
inter_genes <- normalized_counts %>%
  filter(symbol %in% heatmap_genes$gene_id) %>%
  select(2, 4:ncol(normalized_counts))

# Convert the values in first column into row names
inter_genes <- inter_genes %>%
  remove_rownames() %>%
  column_to_rownames(var = "symbol")

# convert tibble into matrix
inter_genes <- as.matrix(inter_genes)

# transpose
inter_genes <- t(inter_genes)

# plot the heatmap of genes of interest
p2 <- pheatmap(inter_genes,
  scale = "row",
  border = "#8B0A50",
  cluster_cols = T,
  cluster_rows = T,
  show_colnames = T,
  show_rownames = T,
  legend = T,
  legend_breaks = c(-2, 0, 2),
  fontsize_row = 8,
  fontsize_col = 10,
  treeheight_row = 35,
  treeheight_col = 40,
  clustering_distance_rows = "minkowski",
  clustering_method = "complete",
  angle_col = 270,
  main = "Genes of interest",
  cellwidth = 20, cellheight = 40,
  cutree_rows = 6, cutree_cols = 7,
  display_numbers = T, fontsize_number = 6,
  number_color = "red", number_format = "%.2f"#,
  #filename = "documents/img/genes of interest.png"
)

# remove columns don't need
significant_genes_cut <- select(
  de_results,
  2, 4, 7, 8
)

# add sig column
significant_genes_cut <- mutate(significant_genes_cut,
  sig = case_when(
    adj_p_val < 0.01 & log_fc > 0.58 ~ "UP",
    adj_p_val < 0.01 & log_fc < -0.58 ~ "DOWN",
    TRUE ~ "Not Sig"
  )
)

# plot volcano plot
p3 <- ggplot(
  data = significant_genes_cut,
  mapping = aes(
    x = log_fc,
    y = -log10(adj_p_val)
  )
) +
  geom_point(aes(color = sig), size = 1) +
  scale_color_manual(
    values = c("red", "gray", "blue"),
    limits = c("DOWN", "Not Sig", "UP")
  ) +
  labs(
    x = "log2 Fold Change", y = "-log10 adjust p-value",
    title = "volcano plot"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    panel.grid = element_blank(),
    panel.background = element_rect(color = "black", fill = "transparent"),
    legend.key = element_rect(fill = "transparent"),
    legend.text = element_text(size = 10),
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(face = "bold", size = 10)
  ) +
  xlim(-10, 10) +
  geom_vline(xintercept = c(-0.58, 0.58), lty = 3, color = "black") +
  geom_hline(yintercept = 2, lty = 3, color = "black")

dat_select <- subset(significant_genes_cut, symbol %in% heatmap_genes$gene_id)

p3 <- p3 +
  geom_text_repel(
    data = dat_select,
    mapping = aes(
      x = log_fc,
      y = -log10(adj_p_val),
      label = symbol
    ),
    size = 3,
    show.legend = FALSE
  )

ggsave(filename = "documents/img/volcano plot.png", plot = p3,
       width = 16, height = 9,
       units = "cm")
