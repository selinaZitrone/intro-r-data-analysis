library(tidyverse)

# where you see ADJUST in a commen,t you have to adjust the code to your own
# project setup (paths and filenames)

# Step 1: List all files that you want to read in
file_list <- list.files(
  path = "R/byod/2024_03_tips/data", # ADJUST: list all files in the data folder
  pattern = "dummy", # ADUST: only files with "dummy" in the name
  full.names = TRUE # return the full path
)

# Step 2: Read in all files into a list
# For this you can use the map function from the purrr package (also
# part of the tidyverse)
# use read_tsv if your files are tab separated and readxl::read_excel if you
# have excel files
all_files <- map(file_list, read_csv)

# Step 3 (optional): Give the files in the list a name
# This name can later be used as an ID in the combined table
file_list_names <- basename(file_list) |>
  str_remove(".csv") # remove the file extension

names(all_files) <- file_list_names

# Step 4: Combine all files in the list to one table
# The files all need the same structure and column names for this to work
# bind_rows just glues all rows of the table together and adds an ID column so
# you know where the rows came from
combined_table <- bind_rows(all_files, .id = "file_id")

# other options to combine tables are
# bind_cols(): like bind_rows but glue columns together instead of rows
# left_join(): Join table by a shared column
