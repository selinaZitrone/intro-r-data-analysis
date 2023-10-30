library(tidyverse)
library(factoextra)
library(readxl)
hidago <- read_excel("R/byod/Hidago_bulkseq.xlsx")
hidago <- hidago %>%
  select(BM_rep1:ext_gene)

df_unique <- hidago[!duplicated(hidago$ext_gene), ]

df2 <- df_unique %>%
  pivot_longer(!ext_gene, names_to = "tissue") %>%
  pivot_wider(names_from = ext_gene, values_from = value) %>%
  mutate(tissue = stringr::str_replace(tissue, "_rep1|_rep2|_rep3", ""))

for_pca <- df2 %>% select(where(is.numeric)) %>% as.matrix()

pca_res <- prcomp(for_pca, scale = TRUE)

# Visualize eigenvalues (how much variance is explained by the pcs)
fviz_eig(pca_res)

fviz_pca_ind(pca_res,
             label = "none",
             habillage = df2$tissue,
             #addEllipses = TRUE,
             #ellipse.level = 0.95,
             palette = "Dark2"
)
