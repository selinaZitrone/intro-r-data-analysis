# References:
# https://natverse.org/
# Here is a project on Github with A LOT of example R scripts:
# The folder 15-hemibrain could be interesting as it uses data from the natverse
# https://github.com/natverse/nat.examples
# I took a lot from the scripts in this folder here (2014 Gregory Jefferis):
# https://github.com/natverse/nat.examples/tree/master/15-hemibrain

# First, install the natverse packages like described on the website
# install.packages('natmanager')
# natmanager::install('natverse') # or full install

# Load necessary libraries
library(natverse)
library(neuprintr)
library(tidyverse)

# Step 1: Get some data from neuprintr -------------------------------------

# 1. Login to the neuprint server ------------------------------------------

# Define the defaults
neuprint_server <- "https://neuprint.janelia.org"
# get your token: login to neuprint, go to your account, copy paste your Auth Token in to the console
neuprint_token <- "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJlbWFpbCI6InNlbGluYS5iYWxkYXVmQGZ1LWJlcmxpbi5kZSIsImxldmVsIjoibm9hdXRoIiwiaW1hZ2UtdXJsIjoiaHR0cHM6Ly9saDMuZ29vZ2xldXNlcmNvbnRlbnQuY29tL2EvQUNnOG9jSjlySUx0MUY4VjVaVTJ3dU5mU2dXU0p5YTZJS29oZ0dRNjJWcE5tN3dmOGttblVBPXM5Ni1jP3N6PTUwP3N6PTUwIiwiZXhwIjoxOTIxNDQ2MDc0fQ.PRHnpfu7bHyoNBs5INxTM2TogXUBMwfuBgSCSw5F1Us"
# set the dataset you want to access (let use this one)
neuprint_dataset <- "hemibrain:v1.2.1"

# login to the server
conn <- neuprint_login(
  server = neuprint_server,
  token = neuprint_token,
  dataset = neuprint_dataset
)

# which datasets are available?
neuprint_datasets() |> names()

# Step 2: Select some neurons to work with ---------------------------------
# There are different options to select the neurons you are interested in, but
# you always need to get the bodyid of the neurons you want to work with

# Option 1: Find info via name
# E.g. if we want to find mushroom body output neurons (MBONs) in the data
mbon_info <- neuprint_search(".*MBON.*")
# look at the table (the column bodyid can be used to fetch data)
mbon_info

# Option 2: Use regions of interest
# see which rois are available
rois <- sort(neuprint_ROIs())
rois
# E.g. get metadata from compartment a'3
ap3_info <- neuprint_find_neurons(
  input_ROIs = "a'3(R)",
  output_ROIs = NULL,
  all_segments = FALSE # if true, fragments smaller than 'neurons' are returned as well
)
head(ap3_info)
# Check out how many neurons there are in this region
nrow(ap3_info)

# Now you can actually read the neurondata for a subset of bodyids that you are
# interested in

# Let's say you want the first body ids from the MBONs
# You can also request more bodyids at once but it takes a time to run
mbon <- neuprint_read_neurons(mbon_info$bodyid[1])
# Let's say you want the first 3 of the ap3 neurons
ap3 <- neuprint_read_neurons(ap3_info$bodyid[1:3])

# 3D visualization ---------------------------------------------------------

# to check which options you can give the plot3d function, check out
?plot3d.neutron

# clear the old 3d plot (you can do this every time, or you can use replace = TRUE in the
# plot3d function)
clear3d()

# Now you can plot the mbon neurons
# the simplest plot is this:
plot3d(mbon)
# or with some other arguments
plot3d(mbon,
  WithConnectors = TRUE,
  col = "#AF6125",
  lwd = 2
)

# if you want to save a file, you can take a snapshot of your 3d plot
rgl.snapshot(filename = "img/hemibrain_an_mbon.png", fmt = "png")

# Or plot the ap3 neurons
plot3d(ap3,
  WithConnectors = TRUE,
  col = "#AF6125",
  lwd = 2
)


# Connectivity analysis ---------------------------------------------------
# See here for reference: https://github.com/natverse/nat.examples/blob/master/15-hemibrain/04-get-connectivity.R
# First get adjcency matrix for all MBONs
mbon_adj <- neuprint_get_adjacency_matrix(bodyids = mbon_info$bodyid)
# give the matrix row and column names from the mbon_info
rownames(mbon_adj) <- colnames(mbon_adj) <- mbon_info$type
# look at the matrix
mbon_adj

# Visualize the connectivity with a heatmap
# for this we can use e.g. the pheatmap package
library(pheatmap)

# plot the heatmap
pheatmap(mbon_adj,
  # Get the colors from the viridis palette
  main = "MBON connectivity matrix",
  col = colorRampPalette(c("white", "steelblue", "darkblue"))(100),
  cluster_rows = FALSE,
  cluster_cols = FALSE
)

# What if we just see what the compartment to compartment connectivity is?
# there are duplicate rows and columns in the matrix, so we ccan average them
mbon_adj_comp <- mbon_adj # make a copy of the adjacency matrix

# Convert the matrix to a tidy data frame with three columns: row, col, and value
df <- mbon_adj_comp |>
  as.data.frame() |>
  rownames_to_column("row") |>
  pivot_longer(-row, names_to = "col", values_to = "value")
# look at the data
df

# Group by the row and column identifiers and compute the mean (this aggregates duplicates)
aggregated <- df |>
  summarise(mean_value = mean(value, na.rm = TRUE), .by = c(row, col))

# Convert the aggregated tidy data frame back to a matrix (needed for pheatmap)
mbon_adj_comp_tidy <- aggregated |>
  pivot_wider(names_from = col, values_from = mean_value) |>
  column_to_rownames("row") |>
  as.matrix()

# Print the resulting matrix
print(mbon_adj_comp_tidy)

# plot a heatmap again
pheatmap(mbon_adj_comp_tidy,
  main = "MBON compartment to compartment connectivity matrix",
  col = colorRampPalette(c("white", "steelblue", "darkblue"))(100),
  cluster_rows = FALSE,
  cluster_cols = FALSE
)


# Network analysis --------------------------------------------------------

# Morphological analysis -------------------------------------------------

# Clustering and classification -------------------------------------------
