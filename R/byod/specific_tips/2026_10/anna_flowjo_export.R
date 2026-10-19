# Read a table exported from FlowJo csv, bring it into a long format
# and save it as Excel file. If you change a gate in FlowJo, just export the
# table again and re-run the script. I was not sure what you want to export from the
# FlowJo csv to Excel, so you definitely need to adapt the script.

library(tidyverse)
library(writexl) # save the output as Excel file

# Create dummy data (skip this part with your own data) -----------------------

# I found online how a FlowJo table roughtly looks like (I hope it is correct):
# one row per sample and one column per statistic.
# The column names are "gating path | statistic"
flowjo <- tibble(
  `Sample:` = c(
    "control_1.fcs",
    "control_2.fcs",
    "control_3.fcs",
    "treatment_1.fcs",
    "treatment_2.fcs",
    "treatment_3.fcs"
  ),
  `Lymphocytes/Single Cells/Live | Count` = c(
    45210,
    47890,
    46020,
    43120,
    44560,
    42980
  ),
  `Lymphocytes/Single Cells/Live/CD4+ | Freq. of Parent` = c(
    62.3,
    60.8,
    61.5,
    55.2,
    54.9,
    56.1
  ),
  `Lymphocytes/Single Cells/Live/CD8+ | Freq. of Parent` = c(
    28.1,
    29.4,
    28.8,
    33.7,
    34.2,
    32.9
  )
)
flowjo

# With your own data, read the csv file instead
# flowjo <- read_csv("data/flowjo_export.csv")

# Clean the data --------------------------------------------------------------

# If your export has summary rows at the end (e.g. "Mean" and "SD"),
# remove them like this:
flowjo <- flowjo |>
  filter(!`Sample:` %in% c("Mean", "SD"))

# Step 1: Turn into long format: one row per sample and column
flowjo_long <- flowjo |>
  pivot_longer(
    cols = !`Sample:`,
    names_to = "column",
    values_to = "value"
  ) |>
  rename(sample = `Sample:`)
flowjo_long

# Step 2: Split the column names at "|" into  gating path and statistic
flowjo_long <- flowjo_long |>
  separate_wider_delim(
    column,
    delim = " | ",
    names = c("gate", "statistic")
  )
flowjo_long

# Step 3: Get the population and the treatment
flowjo_long <- flowjo_long |>
  mutate(
    # Extract population (last part of the gating path split by /)
    population = str_split_i(gate, "/", -1),
    # second to last element split by /
    parent = str_split_i(gate, "/", -2),
    # remove ".fcs" from sample
    sample = str_remove(sample, ".fcs")
  )

# step 4: get treatment and replicate names names
# split sample column by sample name and replicate
# You need to adjust this based on how your files are named and how to get
# sample and replicate info from the filename. Here in this example it's simple
# <treatment>_<replicate>.fcs so it's easy to extract from the file name

# Note: if your treatment names can't be extracted from your filenames, you can create a
# small table in Excel with one column per sample (with the file name how it appears in
# flowjo_long). Then you can join it to the flowjo table with left_join

flowjo_long <- flowjo_long |>
  separate_wider_delim(
    sample,
    delim = "_",
    names = c("treatment", "replicate"),
    cols_remove = FALSE
  )
flowjo_long

# Other steps:
# Extract the info you want
# Reshape the data into the correct format for your excel table
# Make a barplot

# Save as Excel file ----------------------------------------------------------

write_xlsx(flowjo_long, "flowjo_long.xlsx")
