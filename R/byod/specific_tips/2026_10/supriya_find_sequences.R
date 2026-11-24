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
  pattern = "sample",
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

# Find multiple motifs -------------------------------------------------------
# Find the sequences ----------------------------------------------------------

# The motifs you want to look for
# Names will be used in the output table
motifs <- c(gattaca = "GATTACA", acgt = "ACGT", ttaa = "TTAA")

# For each motif, keep the reads that contain it, then combine everything
# into one table. .id = "motif" adds a column with the motif name
reads_motifs <- map(motifs, \(m) filter(reads, str_detect(sequence, m))) |>
  bind_rows(.id = "motif")
reads_motifs

# %reads per file and motif ---------------------------------------------------

result <- reads_motifs |>
  summarise(
    n_sequences = n(),
    reads = sum(reads),
    percent_reads = sum(percent_reads),
    .by = c(file, motif)
  ) |>
  # add a row with 0 if a motif was not found in a file
  complete(
    file,
    motif,
    fill = list(n_sequences = 0, reads = 0, percent_reads = 0)
  )
result

# Find the sequence -----------------------------------------------------------

# Let's say we want to look for this specific sequence
motif <- "GATTACA"
motifs <- c("GATTACA", "")

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

# Multiple motifs, one file ---------------------------------------------------

# Read only one file (no file column)
reads_one_file <- read_tsv("data/ngs_dummy/sample1.txt")
reads_one_file

# The motifs we want to look for. The names are used in the output table
# "ACGT|TTAA": reads with ACGT or TTAA ("|" means "or", no spaces around it)
motifs <- c(
  bystander_1 = "GATTACA",
  bystander_2 = "ACGT|TTAA"
)

# For each motif, keep the reads that contain it, then combine everything
# into one table. .id = "motif" adds a column with the motif name
reads_one_file_motifs <- map(motifs, \(m) {
  filter(reads_one_file, str_detect(sequence, m))
}) |>
  bind_rows(.id = "motif")
reads_one_file_motifs

# Sum of the %reads per motif
# Note: a sequence with 2 motifs is counted for both motifs
result_one_file <- reads_one_file_motifs |>
  summarise(
    percent_reads = sum(percent_reads),
    .by = motif
  )

write_xlsx(result_one_file, "data/percent_reads_motifs_one_file.xlsx")

# Multiple motifs, all files --------------------------------------------------

# For each motif, keep the reads that contain it, then combine everything
# into one table. .id = "motif" adds a column with the motif name
reads_motifs <- map(motifs, \(m) filter(reads, str_detect(sequence, m))) |>
  bind_rows(.id = "motif")
reads_motifs

# If you also want to look for the reverse complement of each motif, use this
# instead (alternative to the one before, run only one of them)
reads_motifs <- map(motifs, \(m) {
  m_rev <- chartr("ACGT", "TGCA", stringi::stri_reverse(m))
  filter(reads, str_detect(sequence, m) | str_detect(sequence, m_rev))
}) |>
  bind_rows(.id = "motif")
reads_motifs

# Sum of the %reads per file and motif
# Note: a sequence with 2 motifs is counted for both motifs, so you can't
# add up the %reads of different motifs
result_motifs <- reads_motifs |>
  summarise(
    n_sequences = n(),
    reads = sum(reads),
    percent_reads = sum(percent_reads),
    .by = c(file, motif)
  ) |>
  # add a row with 0 if a motif was not found in a file
  complete(
    file,
    motif,
    fill = list(n_sequences = 0, reads = 0, percent_reads = 0)
  )
result_motifs

# Wide format: one row per file, one column per motif
result_motifs_wide <- result_motifs |>
  pivot_wider(id_cols = file, names_from = motif, values_from = percent_reads)
result_motifs_wide

write_xlsx(result_motifs_wide, "data/percent_reads_motifs.xlsx")

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
