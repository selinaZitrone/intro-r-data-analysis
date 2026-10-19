# Purpose: Find groups of similar cases with a cluster analysis and reproduce
# the QCA from Bilalova et al. (2025), first with fuzzy sets, then with 0/1 data.
# Paper link: https://doi.org/10.5751/ES-16402-300402
# Check out https://bookdown.org/dusadrian/QCAbook/ for more about QCA in R

library(tidyverse)
library(pheatmap) # heatmap with clustering
library(QCA) # qualitative comparative analysis

# Read the data ---------------------------------------------------------------

# Calibrated data of the 41 cases from the paper (extracted from Appendix 1, Table A1.4).
# Check the paper appendix Table A1.2 for what the column headers mean
cases <- read_csv("data/bilalova_calibrated_data.csv")
cases

# Convert to 0/1 --------------------------------------------------------------

# Your own data is 0/1, so we also create a 0/1 version of the data
# Everything above 0.5 becomes 1, the rest 0
cases_01 <- cases |>
  mutate(across(CAP:OUT, \(x) if_else(x > 0.5, 1, 0)))
cases_01

# Part 1: Cluster analysis ----------------------------------------------------

# Which cases have similar characteristics?
# I excluded the variables P1 (problem context) and OUT (outcome) from the clustering
# Then we can later see if the clusters that can be found are particularly associated
# With success/no success
# But you might also include them depending on the question I guess

# Matrix with the conditions as columns and the case numbers as rownames
conditions_matrix <- cases_01 |>
  select(CASE, CAP, FIT, INTER, DEC, PART, ADAPT) |>
  column_to_rownames("CASE") |>
  as.matrix()

# Distance between all cases: for 0/1 data the Manhattan distance counts in
# how many conditions two cases differ
case_distance <- dist(conditions_matrix, method = "manhattan")
# This gives you a case-by-case distance comparison:
case_distance

# Hierarchical clustering. "average" uses the average distance between the
# cases of two clusters. Check ?hclust for other methods
case_clusters <- hclust(case_distance, method = "average")

# Dendrogram: cases that are joined lower down are more similar
plot(case_clusters)

# Split the dendrogram into groups (define how many groups you want with k)
cases_01 <- cases_01 |>
  mutate(cluster = cutree(case_clusters, k = 2))

# Optional: check the clustering ----------------------------------------------

# With 0/1 data, many cases have the same distance (distance can only be between
# 0 and 6), so the result can depend on the clustering method.
# Compare with "complete" linkage (different clustering by the largest instead of the
# average distance): if most cases end
# up in the same group, the clusters are quite stable

case_clusters_complete <- hclust(case_distance, method = "complete")
# here 37 of 41 cases are in the same group, only 4 are different
table(
  average = cutree(case_clusters, k = 2),
  complete = cutree(case_clusters_complete, k = 2)
)

# Check the silhouette: how well does each case fit into its cluster (-1 to 1, higher is
# better)? Compare the mean silhouette for different k to choose the number
# of clusters (here k = 2 has the highest value)
for (k in 2:5) {
  silhouette_k <- cluster::silhouette(
    cutree(case_clusters, k = k),
    case_distance
  )
  print(paste(
    "k =",
    k,
    "mean silhouette:",
    round(mean(silhouette_k[, "sil_width"]), 2)
  ))
}

# Heatmap of all cases and conditions, sorted by the clustering.
# The bars on the left show cluster, problem context and outcome
case_annotation <- cases_01 |>
  # Select case number (CASE), cluster (1 or 2), problem context (P!) and outcome (OUT)
  select(CASE, cluster, P1, OUT) |>
  mutate(across(everything(), as.character)) |>
  column_to_rownames("CASE")

# Create a heatmap
pheatmap(
  conditions_matrix,
  cluster_rows = case_clusters, # use our clustering from above
  cluster_cols = FALSE, # keep the order of the conditions
  annotation_row = case_annotation,
  color = c("grey90", "steelblue"), # 0 = grey, 1 = blue
  #legend = FALSE,
  fontsize_row = 6
)

# Share of cases with each condition per cluster
cases_01 |>
  summarise(
    n_cases = n(),
    across(CAP:ADAPT, mean),
    .by = cluster
  )

# Number of successful cases per cluster
cases_01 |>
  dplyr::count(cluster, OUT)

# Part 2: QCA as in the paper (fuzzy sets) ------------------------------------

# Which combinations of conditions lead to a successful outcome?

conditions <- c("P1", "CAP", "FIT", "INTER", "DEC", "PART", "ADAPT")

# QCA functions need a data.frame (not a tibble)
cases_df <- as.data.frame(cases)

# Necessary conditions: is a condition always present when the outcome is
# present?
# What the columns in the output mean:
# inclN: consistency "Of all successful cases, what share has this condition?"
# covN: coverage "Of all cases with this condition, what share is successful?"
# RoN: relevance of necessity: "Is the condition simply present almost everywhere?" low values: appears everywhere
pof(
  cases_df[, conditions],
  outcome = "OUT",
  data = cases_df,
  relation = "necessity"
)

# Truth table: one row per combination of conditions, with the number of cases
# (n) and how consistently it leads to the outcome (incl).
# Rows with incl >= 0.8 count as leading to the outcome, like in the paper
truth_table <- truthTable(
  cases_df,
  outcome = "OUT",
  conditions = conditions,
  incl.cut = 0.8,
  show.cases = TRUE,
  sort.by = "OUT"
)
truth_table

# Minimization: find the simplest combinations of conditions that lead to the
# outcome. Many combinations don't occur in the data ("?" in the truth table).
# Like the paper, we only use them if they fit the authors' expectations:
# more CAP, FIT, INTER, PART and ADAPT help success (dir.exp).
# This gives the "intermediate" solution where "?" is only used if it fites the expectations
# of the authors set in dir.exp
# Note: This takes some time to run
solution <- minimize(
  truth_table,
  include = "?",
  dir.exp = "CAP, FIT, INTER, PART, ADAPT", # more of these help success
  details = TRUE
)
solution

# In the solution: * means AND, + means OR, ~ means a condition is absent
# Paper: 3 pathways, all with capacity (consistency 0.98, coverage 0.67).
# We get 0.97 and 0.66, so almost the same

# Part 3: QCA with 0/1 data (crisp sets) --------------------------------------

# If you do QCA with a table with only 0 and 1, you can basically use the same
# function just with 0/1 data.
cases_01_df <- as.data.frame(cases_01)

pof(
  cases_01_df[, conditions],
  "OUT",
  data = cases_01_df,
  relation = "necessity"
)

truth_table_01 <- truthTable(
  cases_01_df,
  outcome = "OUT",
  conditions = conditions,
  incl.cut = 0.8,
  show.cases = TRUE,
  sort.by = "OUT"
)
truth_table_01

solution_01 <- minimize(
  truth_table_01,
  include = "?",
  dir.exp = "CAP, FIT, INTER, PART, ADAPT",
  details = TRUE
)
solution_01
