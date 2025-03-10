# Using topGo ----------------------------------------------

# BiocManager::install("topGO")
# https://bioconductor.org/packages/release/bioc/html/topGO.html
# https://bioconductor.org/packages/devel/bioc/vignettes/topGO/inst/doc/topGO.pdf
browseVignettes("topGO")


# Using clusterProfiler ------------------------------------------
# https://github.com/mousepixels/sanbomics_scripts/blob/main/GO_in_R.Rmd
# https://www.youtube.com/watch?v=JPwdqdo_tRg
# https://yulab-smu.top/biomedical-knowledge-mining-book/clusterprofiler-go.html

#BiocManager::install("clusterProfiler")
BiocManager::install("org.Mm.eg.db")
library(clusterProfiler)
library(org.Hs.eg.db)
# library(AnnotationDbi)
data(geneList, package="DOSE") # load example data
gene <- names(geneList)[abs(geneList) > 2]

# Entrez gene ID
head(gene)

# This takes some time
ggo <- groupGO(gene     = gene,
               OrgDb    = org.Hs.eg.db,
               ont      = "CC",
               level    = 3,
               readable = TRUE)

head(ggo)

ego <- enrichGO(gene          = gene,
                universe      = names(geneList),
                OrgDb         = org.Hs.eg.db,
                ont           = "CC",
                pAdjustMethod = "BH",
                pvalueCutoff  = 0.01,
                qvalueCutoff  = 0.05,
                readable      = TRUE)
head(ego)
goplot(ego)
