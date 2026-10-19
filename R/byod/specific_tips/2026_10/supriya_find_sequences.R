# Purpose: Read many txt files with processed NGS data, find the reads that
# contain a specific sequence and combine the %reads of all files into one
# output file.
# Each txt file is one sample with the following columns:
# - sequence: the read sequence
# - reads: number of reads with this sequence
# - percent_reads: share of all reads in the file (%)

library(tidyverse)
library(writexl) # save the output as Excel file

# Create dummy data (skip this part with your own data) ----------------------

# This will write 3 txt files to your project (in the folder data/ngs_dummy) in the data directory.
# You can then use this dummy data in the example script below
set.seed(1)
dir.create("data/ngs_dummy", showWarnings = FALSE, recursive = TRUE)

random_sequences <- function(n) {
  map_chr(1:n, \(i) {
    paste(sample(c("A", "C", "G", "T"), 30, replace = TRUE), collapse = "")
  })
}

walk(c("sample1", "sample2", "sample3"), \(sample_name) {
  tibble(sequence = random_sequences(50)) |>
    mutate(
      # put the motif into about 20% of the sequences
      sequence = if_else(
        runif(n()) < 0.2,
        paste0(str_sub(sequence, 1, 10), "GATTACA", str_sub(sequence, 18, 30)),
        sequence
      ),
      reads = rpois(n(), lambda = 100),
      percent_reads = reads / sum(reads) * 100
    ) |>
    write_tsv(paste0("data/ngs_dummy/", sample_name, ".txt"))
})

# Read all files --------------------------------------------------------------

# Get the names of all txt files in the folder
files <- list.files(
  path = "data/ngs_dummy",
  pattern = ".txt", # ADJUST this: pattern of your file names? Hwo to identify the files you want to read
  full.names = TRUE
)
files

# Read each file and combine them into one table.
# .id = "file" adds a column with the file name
# Note: if your files are separated by commas instead of tabs, use read_csv()
reads <- map(files, read_tsv) |>
  set_names(basename(files)) |>
  bind_rows(.id = "file")
reads

# Find the sequence -----------------------------------------------------------

# Let's say we want to look for this specific sequence
motif <- "GATTACA"

# Keep only the reads that contain the sequence
reads_motif <- reads |>
  filter(str_detect(sequence, motif))
reads_motif

# If you also want to look at the reverse sequence, you can do it like this
motif_rev <- chartr("ACGT", "TGCA", stringi::stri_reverse(motif))
motif_rev

# Filter for both versions (this is alternative to the one before, you should run only of of them)
reads_motif <- reads |>
  filter(str_detect(sequence, motif) | str_detect(sequence, motif_rev))
reads_motif

# %reads per file -------------------------------------------------------------

# Sum of the %reads of all sequences with the motif in each file
result <- reads_motif |>
  summarise(
    n_sequences = n(),
    reads = sum(reads),
    percent_reads = sum(percent_reads),
    .by = file
  )
result

# Save the output -------------------------------------------------------------

write_csv(result, "data/percent_reads_motif.csv")
write_xlsx(result, "data/percent_reads_motif.xlsx")

# More ------------------------------------------------------------------------
# str_detect only finds exact matches. If you want to allow for mismatches,
# you can check out the Biostrings package and the vcountPattern() function
# I also found 2 tutorials:
#  - https://www.geeksforgeeks.org/r-language/biostrings-in-r/
#  - The package documentation: https://kasperdanielhansen.github.io/genbioconductor/html/Biostrings.html
# Install the package form Bioconductor: https://bioconductor.org/packages//release/bioc/html/Biostrings.html

# Example: allow 1 mismatch
# Convert the sequences once into a Biostrings object
sequences_dna <- Biostrings::DNAStringSet(reads$sequence)

# vcountPattern() counts how often the motif occurs in each sequence
reads_mismatch <- reads |>
  mutate(
    n_matches = Biostrings::vcountPattern(
      motif,
      sequences_dna,
      max.mismatch = 1
    )
  ) |>
  filter(n_matches > 0)
reads_mismatch
