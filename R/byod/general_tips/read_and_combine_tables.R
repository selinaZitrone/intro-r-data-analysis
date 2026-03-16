library(tidyverse)
library(readxl) # for reading Excel files

# where you see ADJUST in a comment you have to adjust the code to your own
# project setup (paths and filenames)

# =============================================================================#
# Example 1: Read and combine multiple CSV files ==============================
# =============================================================================#

# Step 1: List all files that you want to read in
file_list <- list.files(
  path = "data/dummy", # ADJUST: list all files in the data folder
  pattern = "my_data", # ADJUST: only files with "dummy" in the name
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

# =============================================================================#
# Example 2: Read and combine multiple sheets from one Excel file =============
# =============================================================================#

# This is useful when you have one Excel file where each sheet contains data
# from a different condition/experiment/participant but with the same structure.

# ADJUST: path to your Excel file
excel_path <- "data/dummy/multi_sheet_data.xlsx"

# Step 1: Get the names of all sheets in the Excel file
sheet_names <- excel_sheets(excel_path)
sheet_names

# Step 2: Read all sheets into a list
# map iterates over each sheet name and reads it with read_excel
all_sheets <- map(sheet_names, \(sheet) read_excel(excel_path, sheet = sheet))

# Step 3: Name the list elements with the sheet names
names(all_sheets) <- sheet_names

# Look at the individual sheets
all_sheets[["group_A"]]
all_sheets[["group_B"]]

# Step 4: Combine all sheets into one table
# .id = "condition" adds a column with the sheet name so you know which
# sheet each row came from
combined_sheets <- bind_rows(all_sheets, .id = "condition")
combined_sheets
