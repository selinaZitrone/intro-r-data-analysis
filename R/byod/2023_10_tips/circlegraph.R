library(circlize)
# https://jokergoo.github.io/circlize_book/book/initialize-genomic-plot.html

# Location of an example file that can be used with the circlize data
cytoband_file <- system.file(package = "circlize", "extdata", "cytoBand.txt")

# This is how the data looks like in a table
cyto_table <- readr::read_tsv(cytoband_file, col_names = FALSE)

# You can also use the function read.cytoband to read in the data in a
# format that the circlize package will immediately understand
cyto <- read.cytoband(
  cytoband = cytoband_file)

# The data is now in a format that can be used by the circlize package
circos.initializeWithIdeogram()


# Initialize plot from file -----------------------------------------------

cytoband.file = system.file(package = "circlize", "extdata", "cytoBand.txt")
circos.initializeWithIdeogram(cytoband.file)

cytoband.df = read.table(cytoband.file, colClasses = c("character", "numeric",
                                                       "numeric", "character", "character"), sep = "\t")
circos.initializeWithIdeogram(cytoband.df)


# ggbio -------------------------------------------------------------------
library(ggbio)
# BiocManager::install("biovizBase")
data("CRC", package = "biovizBase")

# data needs to be a GRanges object
# GenomicRanges::makeGRangesFromDataFrame() can be used for this
# https://stackoverflow.com/questions/30303813/how-to-create-grange-object-for-a-large-dataset

p <- ggbio() + circle(mut.gr, geom = "rect", color = "steelblue") +
  circle(hg19sub, geom = "ideo", fill = "gray70") +
  circle(hg19sub, geom = "scale", size = 2) +
  circle(hg19sub, geom = "text", aes(label = seqnames), vjust = 0, size = 3)
p
