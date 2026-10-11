# Helpful resources can be
# https://rspatial.org/raster/sdm/index.html
# This package is not on CRAN but it looks promising and modern
# https://github.com/johnbaums/rmaxent

# Step 1: set up RJava --------------------------------------------------------
# the maxent function needs to run Java in the background.
# To get this up and running, you need to do multiple things:

# 1. Install the rJava package
# see here for package doc: https://github.com/s-u/rJava
# install.packages("rJava")

# Install Java matching your Operating system
# leave defaults at installation
# https://www.java.com/en/download/

# Load necessary libraries --------------------
library(tidyverse)
library(dismo) # for maxent model fitting

# Read and prepare data --------------------------------------------------------

# read occurence data of bradypus variegatus
occ_file <- system.file("ex/bradypus.csv", package = "dismo")
occ <- read.csv(occ_file)
# look at the data
head(occ)
# remove the species column. The maxent function needs data with only
# two columns first x and then y
occ <- occ[-1]

# List different prediction files that contain environmental data
pred_files <- list.files(system.file("ex", package = "dismo"), "\\.grd$", full.names = TRUE)
# Those are the predictors you can use
pred_files
# read in all predictor files and stack them in a raster
# each layer of the stack is on environmental variable
predictors <- stack(pred_files)

# Run a Maxent model -----------------------------------------------------------
# Checkout the maxent help to see what you can specify
?maxent

# maxent model using the maxent function from R package dismo

# hinge = false: This tells the MaxEnt model not to use hinge features.
# Hinge features allow the model to capture more complex, piece-wise linear
# relationships. Disabling them makes the model simpler.
# threshold = false: This option disables threshold features, which typically
# help account for sharp transitions in the species–environment relationship.
# Turning them off also simplifies the model.

me <- maxent(
  x = predictors, # predictors as a stack
  p = occ, # occurence data
  factors = "biome", # predictors that are categorical
  args = c("hinge=false", "threshold=false")
)

# Predict the species distribution on the environmental space:
# please note that usually you would do some validation and testing of the model
# on a subset on the data. I don't do that here
predicted <- predict(me, predictors)

# Plot the predictions --------------------------------------------------------
# A simple plot can be done with the plot function and then adding the data points
plot(predicted)
points(occ, pch = 19, col = "red")

# A more fancy plot can be done with ggplot. For this you first need to shuffle the
# data around a bit

# Convert the RasterLayer to a data frame (with 'x', 'y' coordinates)
r_points <- rasterToPoints(predicted)
pred_df <- data.frame(r_points)
# set nice column names
colnames(pred_df) <- c("x", "y", "value")

# Plot the raster and add occurrence points using ggplot2
ggplot() +
  # Plot the raster using geom_raster
  geom_raster(data = pred_df, aes(x = x, y = y, fill = value)) +
  # Add occurrence points from tibble 'occ'
  geom_point(data = occ, aes(x = lon, y = lat), color = "lightgreen", size = 2, alpha = 0.7) +
  # Use a viridis color scale for clarity of the raster values
  scale_fill_viridis_c(name = "Habitat\nSuitability", option = "plasma") +
  labs(
    title = "Predicted Distribution Map",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal() +
  coord_fixed()
