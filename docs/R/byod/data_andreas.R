# Andreas: 3D plots/raster/interpolation
library(tidyverse)

# Step 1: create dummy data -----------------------------------------------
# create dummy data with all combinations of x and y from 1:5
dummydata <- expand_grid(
  x = 1:5,
  y = 1:5
)
# add a z value to the dummy data
dummydata <- mutate(dummydata, z = c(2,2,3,4,5,1,2,3,4,4,2,2,2,3,3,3,3,
                                     2,1,1,4,3,2,1,0))

# Step 2: create some dummy minima point ----------------------------------
# find the global minimum and chose another low point
minima <- dummydata %>% arrange(z) %>% slice(1,2)

# look at the dummydata
ggplot(dummydata, aes(x = x, y = y, fill = z)) +
  geom_raster()+
  geom_point(data = minima, color = "red", size = 3)


# Step 2: Increase resolution and interpolate -------------------------------------
# For this to work easily, you can use the raster package. It provides
# functionality for spatial data and you can easily increase the resolution of
# a raster and interpolate the in-between values using linear interpolation
library(raster)

# Turn your data frame into a raster
dummyraster <- rasterFromXYZ(dummydata)

# create a new rater with more rows and columns to increase the resolution (here 30x30)
new_raster <- raster(nrows = 30, ncols = 30, crs = NA)
# give the new raster the same extent (xmin, xmax, ymin, ymax) as the original raster
extent(new_raster) <- extent(dummyraster)

# use resample to increase resolution from dummyraster to match new_raster
# by default, this uses bilinear interpolation
new_raster <- resample(dummyraster, new_raster)

# plot the new raster with increased resolution
# to plot raster data, you can use the layer_spatial function from the ggspatial package
ggplot() + ggspatial::layer_spatial(new_raster)

# another plotting option is to turn the raster back into a tibble and then plot as
# you are used to
# use
new_tibble <- as.data.frame(new_raster, xy = TRUE) %>% as_tibble()

ggplot(new_tibble, aes(x=x, y=y, fill = z))+
  geom_raster()

# Step 3: Calculate the least cost path -----------------------------------
# These are methods from spatial data analysis. Your problem is similar to
# problems that geographical analysis also faces. There is a method to
# calculate and plot the so called "least cost path" and it should calculate the
# most energy efficient way between any two red points.
# Check it out and see if this is what you intend to do
library(leastcostpath) # package to calculate the least cost path

# step 1: create a cost surface from the raster
# there are different methods to do this. Check out ?slope_cs for more info
slope_cs <- create_slope_cs(new_raster)

# step 2: take your minima points and turn them into spatial points
# also a prerequisite for the analysis to work
loc1 <- minima[1,1:2]
loc1 <- sp::SpatialPoints(loc1)
loc2 <- minima[2,1:2]
loc2 <- sp::SpatialPoints(loc2)

# Step 3: Create the least cost path from location 1 to location 2 on the map
# see ?create_lcp for more info
lcps <- create_lcp(cost_surface = slope_cs,
                   origin = loc1,
                   destination = loc2)

# Step 4: Do the plot
# plot the results
ggplot(new_tibble, aes(x = x, y = y, fill = z)) +
  geom_raster()+
  geom_point(data = minima, color = "red", size = 3)+
  ggspatial::layer_spatial(lcps, color = "red")



# 3D plot -----------------------------------------------------------------
library(plotly)

# convert into a matrix for 3D plotting
to_plot <- new_tibble %>%
  pivot_wider(values_from = z, names_from = x)
# add y values as row names
row_names <- to_plot$y
col_names <- names(to_plot)
# remove column y
to_plot <- to_plot %>% select(-y)
to_plot <- as.matrix(to_plot, dimnames = list(as.double(row_names), as.double(col_names)))



plot_ly(z=to_plot, type = "surface") %>%
  layout(
    scene=list(
      xaxis=list(title='x)'),
      yaxis=list(title='y'),
      zaxis=list(title='z')))
