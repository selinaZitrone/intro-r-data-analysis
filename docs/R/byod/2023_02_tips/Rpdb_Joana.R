library(Rpdb) # to read pdb files from the data base
library(bio3d) # for pdbseq function

# Create a vector of pdb data base ids to download
# copied from the web search into a vector
# Just 3 ids as an example
ids <- c(
  "2PAD",
  "2IC1",
  "3BM5"
)

# Download pdb files for all ids in the ids vector
# path specifies the location where these files are downloaded to
# You need to adjust this to your specific project
bio3d::get.pdb(ids = ids, path = "R/byod/2023_02_tips/pdb_files/")


# Read pdb files into R ---------------------------------------------------

# Read a single pdb file (you need to adjust the path)
pdb_2IC1 <- read.pdb(file = "R/byod/2023_02_tips/pdb_files/2IC1.pdb")

# Or read all downloaded pdb files into a list (Step 1 and 2)

# Step 1: Create a vector with the paths to all your pdb files
# (you need to adjust the path)
pdb_paths <- list.files(
  path = "R/byod/2023_02_tips/pdb_files",
  pattern = ".pdb", full.names = TRUE
)
# Look at the vectors (should be the full paths to the 3 files you just downloaded)
pdb_paths

# Step 2: Use lapply to loop over every path in pdb_paths and call the read.pdb function
# This will return in list. Every element in the list will be one pdb object
my_pdbs <- lapply(pdb_paths, read.pdb)

# length of the list
length(my_pdbs) # should be 3 because you read 3 pdb files

# Access pdb files in the list
# Use [[]] elements in the list.
# E.g. you can access the first pdb file in the list and save it in a new variable
pdb_1 <- my_pdbs[[1]]

# Look at the structure of the pdb file
str(pdb_1)


# Analyse the pdb data ----------------------------------------------------

# Run the pdbseq data for a single pdb file
pdbseq(pdb_1)

# Run the function for every pdb file in the list and save the result again in a list
pdbseq_results <- lapply(my_pdbs, pdbseq)
