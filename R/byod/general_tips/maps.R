# Create maps with ggplot2 and sf
# Here I use map data from the rnaturalearth package to create two maps
# But of course you could also read in your own map data (e.g. shape files)
# using the sf package
# More info on how do maps with different levels of detail here:
# https://rspatialdata.github.io/index.html good general info and overview of a lot of things
# https://rspatialdata.github.io/osm.html specifically for open street map data
# https://r-spatial.org/r/2018/10/25/ggplot2-sf.html rough maps using global map data

library(ggplot2)
library(sf)
library(rnaturalearth) # for more coarse data of the world
library(osmdata) # to pull open street map data

# Part 1: Simple World Map
# ------------------------

# Get world map data
world <- ne_countries(scale = "medium", returnclass = "sf")

# Create a simple world map
world_map <- ggplot(data = world) +
  geom_sf(fill = "lightblue", color = "white") +
  theme_minimal() +
  ggtitle("Simple World Map")

# The world data contains already some additional information like population size.
# You can also use this to color the countries
world_map_pop <- ggplot(data = world) +
  geom_sf(aes(fill = pop_est), color = "white") +
  scale_fill_viridis_c() +
  theme_minimal() +
  ggtitle("World Map with Population Size")

# Display the world maps
world_map
world_map_pop

# Part 2: Detailed Map of North-East Germany with Research Sites
# --------------------------------------------------------------

# Get Germany map data
germany <- ne_countries(scale = "large", country = "germany", returnclass = "sf")

# Create imaginary research sites all over germany
research_sites <- data.frame(
  name = c("Site A", "Site B", "Site C"),
  lon = c(10.5, 11.0, 11.5),
  lat = c(51.0, 51.5, 52.0)
)

# Convert research sites to sf object
research_sites_sf <- st_as_sf(research_sites, coords = c("lon", "lat"), crs = 4326)

# Create a map of North-East Germany with research sites
ne_germany_map <- ggplot() +
  geom_sf(data = germany) +
  geom_sf(data = research_sites_sf, color = "red", size = 3) +
  geom_text(data = research_sites, aes(x = lon, y = lat, label = name),
            hjust = 0, vjust = -1, color = "darkred") +
  theme_minimal() +
  ggtitle("Research Sites in North-East Germany")

# Display the North-East Germany map
ne_germany_map

# More detailed map of NE-Germany with data from Open street map ----------

# This is just some dummy data. The bbox variable defines a bounding box
# of the map extent with 4 values: xmin, ymin, xmax, ymax
# Define the bounding box for the area (20x20 km around a central point)
# Example coordinates for a central point in North-East Germany
central_point <- c(13.5, 53.0) # Longitude, Latitude
buffer_distance <- 0.1 # 0.1 degrees is roughly 10 km
bbox <- c(central_point[1] - buffer_distance, central_point[2] - buffer_distance,
          central_point[1] + buffer_distance, central_point[2] + buffer_distance)

# Fetch OpenStreetMap data for the area
# with add_osm_feature you can select which feature you want to get
# check out https://wiki.openstreetmap.org/wiki/Map_Features to see which features
# are available (they are a lot!) Here just some basic examples
# there is info on land use type, buildings, different levels of roads,...

# get all the streets
osm_data_streets <- opq(bbox = bbox) |>
  add_osm_feature(key = "highway", value = c("motorway", "primary", "secondary", "tertiary")) |>
  osmdata_sf()

osm_data_administrative <- opq(bbox = bbox) |>
  add_osm_feature(key = "boundary", value = "administrative") |>
  osmdata_sf()

# get borders of protected areas
osm_data_protected <- opq(bbox = bbox) |>
  add_osm_feature(key = "boundary", value = "protected_area") |>
  osmdata_sf()

# get forest data
osm_data_forest <- opq(bbox = bbox) |>
  add_osm_feature(key = "natural", value = "wood") |>
  osmdata_sf()

# get grassland data
osm_data_grassland <- opq(bbox = bbox) |>
  add_osm_feature(key = "natural", value = "grassland") |>
  osmdata_sf()

# get water data
osm_data_water <- opq(bbox = bbox) |>
  add_osm_feature(key = "natural", value = "water") |>
  osmdata_sf()

# Get cities and villages
settlements <- opq(bbox = bbox) |>
  add_osm_feature(key = "place", value = c("city", "town", "village")) |>
  osmdata_sf()

# Create imaginary research sites
research_sites <- data.frame(
  name = c("Site X", "Site Y", "Site Z"),
  lon = c(13.45, 13.55, 13.60),
  lat = c(53.05, 53.02, 53.08)
)

# Convert research sites to sf object
research_sites_sf <- st_as_sf(research_sites, coords = c("lon", "lat"), crs = 4326)

# Create a detailed map of the area with rivers and research sites
# this does not look nice but you can play with the numbers of layers and the colors
# depending on what you are interested in.
# turn the features on and off to see the different layers
ggplot() +
  geom_sf(data = osm_data_administrative$osm_multipolygons, fill = "lightgrey") +
  geom_sf(data = settlements$osm_points, color = "black", size = 1) +
  geom_sf(data = osm_data_streets$osm_lines, color = "orange") +
  geom_sf(data = osm_data_water$osm_multipolygons, fill = "lightblue") +
  geom_sf(data = osm_data_forest$osm_multipolygons, fill = "darkgreen") +
  geom_sf_label(data = settlements$osm_points, aes(label = name), fill = NA) +
  geom_sf(data = osm_data_grassland$osm_multipolygons, fill = "lightgreen") +
  geom_sf(data = osm_data_protected$osm_multipolygons, color = "darkgreen", fill = NA) +
  geom_sf(data = research_sites_sf, color = "red", size = 3) +
  theme_minimal() +
  ggtitle("Detailed Map of North-East Germany")


# Save the maps as PNG files ----------------------------------------------
# They can be saved like any other ggplot
ggsave("world_map.png", plot = world_map, width = 10, height = 6, dpi = 300)
ggsave("ne_germany_map.png", plot = ne_germany_map, width = 8, height = 8, dpi = 300)


