# Read a tif file into R and convert it to a matrix

# Load necessary packages
library(tiff)
library(tidyverse)

# Create a dummy tiff file for demonstration
# 1. Create a dummy raster matrix (100 x 100) with random intensity values between 0 and 1
set.seed(123) # For reproducibility
dummy_matrix <- matrix(runif(100 * 100), nrow = 100, ncol = 100)

# 2. Save the matrix as a TIF file
output_file <- "img/dummy_image.tif"
writeTIFF(dummy_matrix, output_file)

# 3. Read the TIF image back into R
image_matrix <- readTIFF(output_file)

# The tif is now read in as a matrix, depending on what you want to do with it
# you can leave it as matrix or convert it to a tibble, e.g. to use with ggplot
image_matrix

# Example: Processing the tif with tidyverse

# Convert the matrix to a tibble
# Each row & col is a pixel with x and y positions. The value is intensity

# Option 1: Step-by-step (so you see what happens at every stage)

# turn the matrix into a tibble
image_tbl <- as_tibble(image_matrix)
image_tbl

# add a column to indicate the y-position
image_tbl <- image_tbl |> mutate(y = row_number())
image_tbl

# pivot the table to have one row per pixel
image_tbl <- image_tbl |>
  pivot_longer(-y, # pivot all columns but not y
    names_to = "x", # the new names column is the x coordinate
    values_to = "intensity", # the value column is called intensity
    names_prefix = "V", # remove the V prefix (from the default name of tibble columns)
    names_transform = list(x = as.numeric)
  )
image_tbl

# Option 2: Combine all steps into a single pipeline
image_tbl <- image_matrix |>
  as_tibble() |>
  mutate(
    # add a column to indicate the y-position
    y = row_number(),
  ) |>
  # pivot the table to have one row per pixel
  pivot_longer(-y, # pivot all columns but not y
    names_to = "x", # the new names column is the x coordinate
    values_to = "intensity", # the value column is called intensity
    names_prefix = "V", # remove the V prefix (from the default name of tibble columns)
    names_transform = list(x = as.numeric)
  )
image_tbl

# Plot a heatmap of the pixel intensities using ggplot2
ggplot(image_tbl, aes(x = x, y = y, fill = intensity)) +
  geom_tile() +
  scale_fill_viridis_c() +
  theme_minimal() +
  labs(
    title = "Dummy TIF Image Pixel Intensities",
    x = "Column",
    y = "Row",
    fill = "Intensity"
  )
