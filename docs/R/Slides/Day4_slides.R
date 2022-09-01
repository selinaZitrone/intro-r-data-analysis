# Read excel files -------------------------------------------------------------

library(readxl)

# read one file
# by default it's reading the first sheet
# use read_xls if you use xls format, or read_excel for mor generic one
read_xlsx(path = "data/my_datafile1.xlsx")

# look at ?read_excel

# read the second sheet
read_xlsx(path = "data/my_datafile1.xlsx", sheet = 2)
read_xlsx(path = "data/my_datafile1.xlsx", sheet = "sheet2")

# read multiple sheets into one table
path_to_file <- "data/my_datafile1.xlsx"
sheets <- excel_sheets(path_to_file)

all_sheets <- lapply(sheets, function(x) read_xlsx(path_to_file, sheet = x))
names(all_sheets) <- sheets
# with an id column
do.call(bind_rows, list(all_sheets, .id = "sheet_id"))
# without id column
do.call(bind_rows, all_sheets)

# read multiple excel files together (just one sheet per excel file)

# path to a location of the xlsx files
# this code will read in all xlsx files at the current location
# if you don't want this, you have to adjust the `pattern`
# path = "path/to/where/xlsx/files/are"
path_to_files <- list.files(path = "./data", pattern = ".xlsx", full.names = TRUE)
# check if these are the right files
path_to_files
# read in the files
all_files <- lapply(path_to_files, read_xlsx)

# optional: give the files names
file_names <- c("data1", "data2")

# bind the rows togehter (without id column)
do.call(bind_rows, all_files)
# with id column
do.call(bind_rows, list(all_files, .id = "file_id"))

# multiple excel files in multiple sheets that all have to be combined


