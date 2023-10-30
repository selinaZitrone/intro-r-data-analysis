library(ape)
# https://klausvigo.github.io/phangorn/
library(phangorn)
# https://github.com/YuLab-SMU/ggtree
# https://bioconductor.org/packages/release/bioc/html/ggtree.html
# https://yulab-smu.top/treedata-book/
# https://jeffreyblanchard.github.io/EvoGeno2020R/ggtree_tutorial.html
library(ggtree)


# Use phangorn package ----------------------------------------------------

fdir <- system.file("extdata", package = "phangorn")
mm <- read.csv(file.path(fdir, "mites.csv"), row.names = 1)
mm_pd <- phyDat(as.matrix(mm), type = "USER", levels = 0:7)

write.phyDat(mm_pd, file.path(fdir, "mites.nex"), format = "nexus")

mm_pd <- read.phyDat(file.path(fdir, "mites.nex"), format = "nexus", type = "STANDARD")

# Use ggtree --------------------------------------------------------------

# Read the nexus file I just exported using phangorn
mm_pd_ggtree <- ape::read.nexus(paste0(fdir, "/mites.nex"))

data(chiroptera, package="ape")
groupInfo <- split(chiroptera$tip.label, gsub("_\\w+", "", chiroptera$tip.label))
chiroptera <- groupOTU(chiroptera, groupInfo)
ggtree(chiroptera, aes(color=group), layout='circular') +
  geom_tiplab(size=1, aes(angle=angle)) +
  theme(legend.position = "none")

ggtree(mm_pd)
