# Example script for sequence analysis using TraMineR
# https://traminer.unige.ch/
# Book with R examples using this package: https://sa-book.github.io/
# Doc of ggseqplot https://maraab23.github.io/ggseqplot/articles/ggseqplot.html
library(TraMineR)
library(ggseqplot) # for ggplot2-based visualizations
library(tidyverse)

# Load the example dataset (transition from school to work)
data(mvad)
mvad
?mvad # for more info on the dataset

# Step 1: Define sequences -----------------------------------------------

# mvad activity states are in the monthly columns (from "Jul.93" to "Jun.99")
# Extract only these columns that contain the monthly states
mvad_states <- select(
  mvad,
  starts_with(c(
    "Jan",
    "Feb",
    "Mar",
    "Apr",
    "May",
    "Jun",
    "Jul",
    "Aug",
    "Sep",
    "Oct",
    "Nov",
    "Dec"
  ))
)

# Define the state alphabet and labels
# States: school, FE, employment, joblessness, training, HE
# Either code them manually or extract the unique values in mvad_states
# Option A: Manually
activity_alphabet <- c(
  "school",
  "FE",
  "employment",
  "joblessness",
  "training",
  "HE"
)
# Option B: Extract automatically from the columns
activity_alphabet <- mvad_states |>
  unlist() |>
  unique() |>
  sort() |>
  as.character()

# Define labels and colors for the states
# Make sure the order matches the order in activity_alphabet
activity_labels <- c(
  "School",
  "FurtherEd",
  "Employment",
  "Jobless",
  "Training",
  "HigherEd"
)
# Define colors for the states
# Make sure the order matches the order in activity_alphabet
activity_colors <- c(
  "#1f77b4",
  "#ff7f0e",
  "#2ca02c",
  "#d62728",
  "#9467bd",
  "#8c564b"
)

# Build sequence object
# Checkout ?seqdef for more options
mvad_seq <- seqdef(
  mvad_states,
  alphabet = activity_alphabet,
  states = activity_alphabet,
  labels = activity_labels,
  cpal = activity_colors
)

# Quick check
mvad_seq[1:5, 1:12] # Sequence of first 5 individuals, first 12 months

# Step 2: Visualize sequences -----------------------------------------------

# There are many ways to visualize sequences
# Here I show a few examples
# You can use functions from TraMineR or ggseqplot
# Check out the online documentation for more options

# Option A: Use TraMineR plotting functions
# Index plot: individual sequences
seqIplot(
  mvad_seq,
  sortv = "from.start",
  with.legend = TRUE,
  main = "MVAD - Individual Sequences"
)

# State distribution plot (stacked proportions over time)
seqdplot(
  mvad_seq,
  with.legend = "right",
  main = "MVAD - State Distribution Over Time"
)

# Option B: Use the functions from ggseqplot
# State distribution plot
ggseqdplot(mvad_seq)

# transition rate
ggseqtrplot(mvad_seq)

# 3: Distances between sequences -----------------------------------------------
# How different are the trajectories of different individuals?
# - Idea: How many "edits" (insertions, deletions, substitutions) are needed
#   to transform one person’s sequence into another? Fewer edits = more similar
#   and smaller number.
# - We set costs for those edits. Substitution costs can be learned from data
#   (using transition rates) or kept constant.

# Build a substitution-cost matrix from observed transition rates
# - States that commonly transition into each other get lower substitution cost (more similar),
#   and rare transitions imply higher cost (more different).
submat <- seqsubm(mvad_seq, method = "TRATE")

# Choose insertion/deletion (indel) cost
# - Controls how much we “allow” shifting sequences in time to align them.
# - A typical starting value is 1; you can tune this.
indel_cost <- 1

# Compute the OM distance matrix (712 x 712), where entry (i, j) is the distance
# between person i and person j’s sequences ( we have 712 people in the original
# data).
om_diss <- seqdist(mvad_seq, method = "OM", indel = indel_cost, sm = submat)

# Step 4: Cluster sequences ----------------------------------------------------
# - Goal: group people with similar trajectories
# - We’ll use hierarchical clustering (agglomerative) with Ward’s method,
#   which tends to form compact, interpretable clusters.

# Perform hierarchical clustering on the dissimilarity matrix
hc <- hclust(as.dist(om_diss), method = "ward.D2")

# Decide how many clusters to cut (k). Start with k = 3–6 and refine as needed.
k <- 4

# Cut the tree into k number of groups
# Each persons trajectory is now assigned to one of k clusters
cl_labels <- cutree(hc, k = k)

# Attach cluster labels to the original data for later summaries/plots
mvad <- mvad |>
  mutate(
    cluster = paste0("cluster_", cl_labels)
  )

# how many people in each cluster?
table(mvad$cluster)

# Visualize clusters -----------------------------------------------
# We want to see "typical sequences" and state distributions per cluster.

# State distribution plot by cluster (stacked proportions over time)
# Shows how each cluster’s composition changes across months.
seqdplot(
  mvad_seq,
  group = mvad$cluster,
  with.legend = "right",
  main = "State Distribution by Cluster"
)

# or with ggplot
ggseqdplot(mvad_seq, group = mvad$cluster) +
  ggtitle("State Distributions by Cluster")

# Identify 1 medoid (most central real sequence) per cluster for quick interpretation
# this medoid is an actually observed sequence of the data that is most
# representative of the cluster
library(WeightedCluster)
set.seed(123)

# Assigns each sequence to one of k clusters.
# Chooses one real sequence per cluster as the medoid
# # (the most central, i.e., with minimal average distance to others in that cluster).

kmed <- wcKMedoids(as.dist(om_diss), k = k)

# Get the indices of medoid sequences (one per cluster)
medoid_ids <- unique(kmed$clustering)

# Plot those representative sequences
seqplot(
  mvad_seq,
  type = "i",
  idxs = medoid_ids,
  with.legend = TRUE,
  main = "Representative (Medoid) Sequences by Cluster"
)

# Or with ggplot
ggseqiplot(mvad_seq[medoid_ids, ], group = 1:4, facet_ncol = 1, border = TRUE)
