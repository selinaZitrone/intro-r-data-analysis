# load packages needed
library(tidyverse)

# Step 1: list all participant files that should be combined in one single table
# Make sure you adapt your path to the correct one
participant_files <- list.files(
  path = "R/byod/2024_10_tips/data",  # look for files in the data folder
  pattern = ".txt", # this list all txt files in the path
  full.names = TRUE # return the full path
  )

# Step 2: Read in all files into a list
# For this you can use the map function from the purrr package (also
# part of the tidyverse)
# use read_tsv if your files are tab separated and readxl::read_excel if you
# have excel files
# we read all columns first as default character (i.e. text) columns even if they are
# numbers in reality. This then allows us to concatenate them even if we have some
# duplicate column headers in between the files.
all_files <- map(participant_files, read_tsv, col_types = cols(.default = "c"))

# Step 4: Combine all files in the list to one table
combined_table <- bind_rows(all_files)

# Step 5: Get rid of the duplicate column headers in between the table
# this vector contains the column header because these are the words we want
# to find in the table and filter out
to_filter_out <- names(combined_table)

combined_table <- combined_table |>
  # filter out any row where any column has a value from to_filter_out
  filter(if_any(everything(), ~ !.x %in% to_filter_out))

# Step 6: Convert the columns to the correct data type. For now everything is
# character, but we want to convert some columns to numeric. An easy option is
# to use the type_convert function from the readr package to convert all columns
# possible to numeric
combined_table <- type_convert(combined_table)

# Look at the result
combined_table



