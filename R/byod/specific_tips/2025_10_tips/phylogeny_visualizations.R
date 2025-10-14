library("treeio")
library("ggtree")

# Read an example tree
nwk <- system.file("extdata", "sample.nwk", package = "treeio")
tree <- read.tree(nwk)

ggplot(tree, aes(x, y)) + geom_tree() + theme_tree() + geom_tiplab()

# Add labels:
ggtree(tree) +
  geom_tiplab() +
  geom_cladelabel(
    node = 17,
    label = "Some random clade",
    color = "red2",
    offset = .8,
    align = TRUE
  ) +
  geom_cladelabel(
    node = 21,
    label = "A different clade",
    color = "blue",
    offset = .8,
    align = TRUE
  ) +
  theme_tree2() +
  xlim(0, 70) +
  theme_tree()
