# Andreas: 3D plots/raster/interpolation
library(tidyverse)
set.seed(123)


# Step 1: create dummy data -----------------------------------------------

# create dummy data with all combinations of x and y from 1:5
dummydata <- expand_grid(
  x = 1:5,
  y = 1:5
)
# add a z value to the dummy data
dummydata <- mutate(dummydata, z = c(2,2,3,4,5,1,2,3,4,4,2,2,2,3,3,3,3,
                                     2,1,1,4,3,2,1,0))

dummydata <- mutate(dummydata, z = rnorm(n()))


# Step 2: create some dummy minima point ----------------------------------

# find the global minimum and chose another low point
minima <- dummydata %>% arrange(z) %>% slice(1,2)

# look at the dummydata
ggplot(dummydata, aes(x = x, y = y, fill = z)) +
  geom_raster()+
  geom_point(data = minima, color = "red", size = 3)


# Step 3: Calculate the least cost path -----------------------------------
# These are methods from spatial data analysis. Your problem is similar to
# problems that geographical analysis also faces. There is a method to
# calculate and plot the so called "least cost path" and it should calculate the
# most enery efficient way between the two red points.
# Check it out and see if this is what you intend to do
library(raster) # needed for rasterizing data (prerequisite for the other packages)
library(leastcostpath)

# create raster from data
dummyraster <- rasterFromXYZ(dummydata)

# step 1: create a cost surface from the raster
# there are different methods to do this. Check out ?slope_cs for more info
slope_cs <- create_slope_cs(dummyraster)

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
ggplot(dummydata, aes(x = x, y = y, fill = z)) +
  geom_raster()+
  geom_point(data = minima, color = "red", size = 3)+
  ggspatial::layer_spatial(lcps, color = "red")

# create a transition layer:
library(gdistance)

altDiff <- function(x){x[2] - x
  [1]}
hd <- transition(dummyraster, altDiff, 8, symm=FALSE)
adj <- adjacent(dummyraster, cells=1:ncell(r), pairs=TRUE, directions=8)
speed <- slope
speed[adj] <- 6 * exp(-3.5 * abs(slope[adj] + 0.05))

tr1 <- gdistance::transition(dummyraster, transitionFunction = mean, directions  = 8)
sp <- cbind(c(5,2), c(2,1))
gdistance::costDistance(tr1,sp)


# Increasing the resolution -----------------------------------------------

# create a new rater with more rows and columns to increase the resolution
new_raster <- raster(nrows = 30, ncols = 30, crs = NA)
# give the new raster the same extent (xmin, xmax, ymin, ymax) than the original raster
extent(new_raster) <- extent(dummyraster)

# use resample to increase resolution from dummyraster to match new_raster
new_raster <- resample(dummyraster, new_raster)

ggplot() + ggspatial::layer_spatial(new_raster)

minima <- tibble(x = c(2,5.5),y=c(0.5,5.5))

ggplot() +
  ggspatial::layer_spatial(new_raster)+
  geom_point(data = minima ,aes(x=x,y=y), color = "red")

# step 2: take your minima points and turn them into spatial points
# also a prerequisite for the analysis to work
loc1 <- minima[1,1:2]
loc1 <- sp::SpatialPoints(loc1)
loc2 <- minima[2,1:2]
loc2 <- sp::SpatialPoints(loc2)

# Step 3: Create the least cost path from location 1 to location 2 on the map
# see ?create_lcp for more info

slope_cs <- create_slope_cs(new_raster, neighbours = 32)
lcps <- create_lcp(cost_surface = slope_cs,
                   origin = loc1,
                   destination = loc2)

# Step 4: Do the plot
# plot the results
ggplot() +
  ggspatial::layer_spatial(new_raster)+
  geom_point(data = minima ,aes(x=x,y=y), color = "red")+
  ggspatial::layer_spatial(lcps, color = "red")+
  scale_fill_gradientn(colours = terrain.colors(10))

