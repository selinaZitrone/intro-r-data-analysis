# Create some dummy data and save it in a folder
dummy <- expand_grid(
  plot_id = c("a", "b"),
  transects = 1:2
)
dummy$speciesA_height <- 1:4
dummy$speciesA_chloro <- 1:4
dummy$speciesB_height <- 1:4
dummy$speciesB_chloro <- 1:4


# I save the same file 3 times under different file names. Note that I put the
# data first in YYYY-MM-DD format and then after the _ the name of the file
# This again makes it easy to read all the files in and from the file names
# extract information about the date
write_csv(dummy, "data/malia/2026-05-01_transects.csv")
write_csv(dummy, "data/malia/2026-06-01_transects.csv")
write_csv(dummy, "data/malia/2026-07-01_transects.csv")

# Example analysis of one file --------------------
dummy |>
  pivot_longer(
    cols = !c(plot_id, transects),
    values_to = "value",
    names_to = "species"
  ) |>
  separate_wider_delim(
    cols = species,
    delim = "_",
    names = c("species", "trait")
  )

# Read in and combine multiple files --------------------

# Note: This assumes your files all have the same structure, only recorded on
# different dates

# Step 1: List all files in the folder you want to read in
file_list <- list.files(
  "data/malia", # the folder where you data is,
  full.names = TRUE # give me the full path (for reading them in)
  # patterns = ".csv"
)
# This is just a vector with the file names
file_list

# Read in all the files at once and combine them by their file name
# this only makes sense if your data all has the same form (i.e.) column names
# and it makes sense to combine them

transects <- map(file_list, read_csv) |>
  set_names(basename(file_list)) |>
  bind_rows(.id = "file")
# This gives you 1 table where all input tables are glued together and the
# "file" column identifies where the file comes from
transects

# Extract the data from the file name
transects |>
  mutate(
    date = str_extract(file, "\\d{4}-\\d{2}-\\d{2}")
  ) |>
  mutate(
    # format the dat as a date
    date = ymd(date)
  )
