# Load required libraries
library(DESeq2) # For differential expression analysis
library(clusterProfiler) # For GO enrichment analysis
library(org.Hs.eg.db) # Database of human genes for ID conversion and annotation
library(ggplot2) # For creating plots
library(airway) # Contains the airway dataset
library(dplyr) # For data manipulation

# Load the airway dataset
data(airway) # This loads the airway SummarizedExperiment object

# Create a DESeqDataSet object from the airway data
# DESeq2 function: DESeqDataSet()
dds <- DESeqDataSet(airway, design = ~ cell + dex)

# Run DESeq2 analysis
# DESeq2 functions: DESeq() and results()
dds <- DESeq(dds) # Performs the differential expression analysis
res <- results(dds) # Extracts the results from the DESeq analysis

# Convert results to a data frame and add gene names
# Base R functions: as.data.frame() and rownames()
res_df <- as.data.frame(res)
res_df$gene <- rownames(res_df)

# Select top differentially expressed genes
sig_genes <- res_df %>%
  filter(!is.na(padj)) %>%
  filter(padj < 0.05) %>%
  arrange(padj) %>%
  head(100) %>%
  pull(gene) # Extract the gene names

# Convert gene IDs to Entrez IDs
# # clusterProfiler function: bitr()
entrez_ids <- bitr(sig_genes, fromType = "ENSEMBL", toType = "ENTREZID", OrgDb = org.Hs.eg.db)

# Perform GO enrichment analysis
# clusterProfiler function: enrichGO()
go_enrichment <- enrichGO(
  gene = entrez_ids$ENTREZID,
  OrgDb = org.Hs.eg.db,
  ont = "BP", # Biological Process
  pAdjustMethod = "BH", # Benjamini-Hochberg adjustment
  pvalueCutoff = 0.05,
  qvalueCutoff = 0.2,
  readable = TRUE
) # Convert gene IDs to symbols

# Display the first few rows of the GO enrichment results
print(head(go_enrichment@result, n = 20))

# Visualize the GO enrichment results with a bar plot
# it's a ggplot object, so in theory, you could add more layers to it
# clusterProfiler function: barplot()
barplot_go <- barplot(go_enrichment,
  showCategory = 20,
  title = "GO Enrichment Analysis: Biological Process"
)

barplot_go

# Visualize the GO enrichment results with a dot plot
# clusterProfiler function: dotplot()
dotplot_go <- dotplot(go_enrichment,
  showCategory = 20,
  title = "GO Enrichment Analysis: Biological Process"
)

# Save plots as images
ggsave("R/byod/img/barplot_go.png", barplot_go, width = 10, height = 8)
ggsave("R/byod/img/dotplot_go.png", dotplot_go, width = 10, height = 8)
