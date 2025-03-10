library(phytools)
library(ape)
library(readr)
library(mapdata)
library(raster)
library(stringr)
library(viridis)

# https://guangchuangyu.github.io/treedata-workshop/r-treeio.html


# Read data ---------------------------------------------------------------

tree1 <- ape::read.nexus("R/byod/2024_03_tips/data/Philo_old-tree_2024.nex")
# Read coordinates (separator is ; therefore use read_csv2)
# The function recognized that you use , as decimal separator and it transforms
# it automatically into a point
phy_coord <- read_csv2("R/byod/2024_03_tips/data/Philo_coord-countries.csv")
# Have a look at the data
phy_coord
str(tree1) # Print the structure of the tree object into the console


# reorganize internal structure of the tree to get ladderized effect when plotted
clean_tree <- ladderize(tree1, right = FALSE)
plot(clean_tree)

# Add bioregion to the coordinate data
phy_coord <- phy_coord |>
  mutate(bioregion = case_when(
    country %in% c("Mexico","Guatemala","Costa Rica","Panama") ~ "red",
    country %in% c("Bolivia","Peru","Ecuador","Colombia","Venezuela","Brazil",
                   "Guyana","French Guiana","Surinam") ~ "green",
    country %in% c("Trinidad & Tobago","Guadalupe","Cuba") ~ "blue",
    .default = NA
  ))

# Reorder the coordinate table by the order of the taxa in the ladderized tree
phy_coord <- phy_coord |>
  mutate(order = match(Taxa, tree_ladderized_down$tip.label)) |>
  arrange(order)

# Turn taxa column into rownames
phy_coord <- tibble::column_to_rownames(phy_coord, var = "Taxa")

#set colours
cols<- setNames(phy_coord$bioregion, clean_tree$tip.label)


#initiate plot object
obj <- phylo.to.map(clean_tree, phy_coord, database="worldHires",
                    regions=phy_coord$country,plot=FALSE, rotate=T)


#make the plot
plot(obj, direction = "rightwards", ylim = c(-30, 40), xlim = c(-99, -10),
     lwd = 0.5, type = "phylogram", ftype = "off",
     fsize = 0.3, cex.points = c(0.1, 1.3),
     colors = cols, pts = FALSE)


# ggtree ---------------------------------------------------------------
library(ggtree)

tree <- ape::read.nexus("R/byod/2024_03_tips/data/Philo_old-tree_2024.nex")

# Add bioregion
tree$bioregion <- sample(c("red", "green", "blue"),
                         length(tree$tip.label), replace = TRUE)

bioregion <- as_tibble(phy_coord, rownames = "taxa")

bioreg <- list(
  "South_America" = bioregion |> filter(country %in% c("Bolivia","Peru",
                                                       "Ecuador","Colombia",
                                                       "Venezuela","Brazil",
                   "Guyana","French Guiana","Surinam")) |>
    pull(taxa),
  "Central_America" = bioregion |> filter(country %in% c("Mexico","Guatemala",
                                                       "Costa Rica","Panama")) |>
    pull(taxa),
  "Caribbean" = bioregion |> filter(country %in% c("Trinidad & Tobago",
                                                  "Guadalupe","Cuba")) |>
    pull(taxa)

)
tree <- as.treedata(tree)
t <- ggtree(tree) +
  # Add theme for trees
  theme_tree()# +
  # Add tip labels as text, slightly offset from the tips and aligned
  #geom_tiplab(geom = "text", align = TRUE, offset = 0.5, linetype = NA)

groupOTU(t, bioreg, "Bioregion") +
  aes(color = bioreg) +
  theme(legend.background = "top")

library(treeio)
library(ggtree)
beast_file <- system.file("examples/MCC_FluA_H3.tree",
                          package="ggtree")
rst_file <- system.file("examples/rst", package="ggtree")
mlc_file <- system.file("examples/mlc", package="ggtree")

beast_tree <- read.beast(beast_file)
codeml_tree <- read.codeml(rst_file, mlc_file)
merged_tree <- merge_tree(beast_tree, codeml_tree)

get.fields(merged_tree)
library(ggplot2)
ggtree(merged_tree, branch.length='dN', aes(color=dN)) +
  scale_color_continuous(high='#D55E00', low='#0072B2') +
  theme_tree2() + geom_tiplab(size=2)
