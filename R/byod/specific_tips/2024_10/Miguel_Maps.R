# More info and examples online:
# https://r-spatial.org/r/2018/10/25/ggplot2-sf.html

# Load all necessary libraries ----------------------------------------------

# summarize values by country/continent and plot in map
library(tidyverse)
# for spatial geoms in the plot and other spatial operations
library(sf)
# for map data of the world
library(rnaturalearth)
library(rnaturalearthdata)
# to combine plots and tables in one
library(patchwork)
# to add repelling labels
library(ggrepel)

# set a seed so the random data creation is reproducible
set.seed(123)

# Create some dummy data from literature review -----------------------------
# Now I create some dummy data from a literature review
dummy_literature_data <- tibble(
  country = sample(
    c(
      "Germany", "France", "Italy", "Spain", "Portugal",
      "Greece", "Austria", "Switzerland",
      "Netherlands", "Belgium", "Zimbabwe", "South Africa",
      "Argentina", "Colombia", "Japan", "Vietnam", "Australia"
    ), 100,
    replace = TRUE
  )
)
# This is how the data looks like
# your real data will have more that just the country column and you will read it
# from your computer instead of creating it here, but for this example
# this is enough
dummy_literature_data

# Summarize the literature review data --------------------------------------
# we can count how many papers we have per country
literature_data_count <- count(dummy_literature_data, country)

# Get world map data  -------------------------------------------------------
# Get world map data in sf format
world <- ne_countries(scale = "medium")
# check out how the world data looks like (imagine it like a table with geo features)
# it has not just the map information but also other types of information like
# gdp or population estimates
world
# if you look at the columns, you see that there are many different columns
# that specify the country name in different ways, e.g. the columns
# name: name of the country
world$name
# Names in English language
world$name_en
# official abbreviation (iso a2) of the country
world$iso_a2

# Combine the world map data with your literature data -----------------------
# For this, you need to join the world data with your data
# Requirement is that you write the country names in your data in exactly the same
# way as they are written in one of the world data columns, e.g. in the name_en
# column
world_literature <- left_join(world, literature_data_count, by = c("name_en" = "country"))

# Plot the data -------------------------------------------------------------
# This is a basic map without additional info
ggplot(data = world_literature) +
  geom_sf()

# But you can also use the fill aesthetic to fill the country by the number of papers
ggplot(data = world_literature) +
  geom_sf(aes(fill = n))

# Or even more fancy, you can add labels and also change the color scales
# and theme
literature_map <- ggplot(data = world_literature) +
  geom_sf(aes(fill = n)) +
  geom_sf_label(aes(label = n),
    size = 2
  ) +
  scale_fill_viridis_c() +
  theme_minimal() +
  theme(
    axis.title = element_blank()
  )

# Add a table to the plot --------------------------------------------------
# Let's count the number of publications by continent
count_continent <- world_literature |>
  # turn it from a spatial table into a simple tibble
  as_tibble() |>
  count(continent)

# the new version of the patchwork package can combine plots and table objects
# see more here:
# https://www.tidyverse.org/blog/2024/09/patchwork-1-3-0/
# https://patchwork.data-imaginist.com/articles/guides/layout.html

literature_map + inset_element(wrap_table(count_continent),
  left = 0.05, right = 0.2, top = 0.55, bottom = 0.6,
  align_to = "full"
)

# Map using ggrepel for the labels ------------------------------------------

# geom_label_repel needs x and y positions where to put the labels
# The world map already has two columns that are called label_x and label_y
# So we could use those to position the labels but we can also calculate the
# centroids of each country of interest and use them as x and y positions instead
# You can try both positions and see which looks better

# Step 1: Calculate the centroids of each country
world_centroids <- world_literature |>
  # remove those rows where the count of papers is NA -> These countries have no
  # papers and therefore do not get any labels
  filter(!is.na(n)) |>
  # select only the columns of interest
  # n: the count of papers
  # geometry: the centroid point of the country
  # The world data already has values for label_x and label_y
  # They are not exaclty the centroids but maybe they also work, so we try them
  # as well
  select(geometry, n, label_x, label_y) |>
  as_tibble() |>
  # now we manually calculate the centroids and add them as a new column
  mutate(
    centroid = st_centroid(geometry)
  ) |>
  # We can extract the x and y values from the centroid object
  # and add them as new columns
  mutate(
    label_x_2 = st_coordinates(centroid)[, 1],
    label_y_2 = st_coordinates(centroid)[, 2]
  )

# Now we can plot again using the centroids from the previous step
# You can try both positions for the ggrepel lables either label_x or label_x_2
# Check out ?geom_label_repel for mor info on which parameters to use
# Also check out the website for more examples of ggrepel
# https://ggrepel.slowkow.com/
ggplot(data = world_literature) +
  geom_sf(aes(fill = n)) +
  ggrepel::geom_label_repel(
    data = world_centroids,
    mapping = aes(label = n, x = label_x_2, y = label_y_2),
    nudge_x = .15,
    nudge_y = 1,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20,
    max.overlaps = Inf,
    size = 2.5
  ) +
  # Change color of the points. You can chose your own plaette on
  # https://r-charts.com/color-palettes/
  paletteer::scale_fill_paletteer_c(
    palette = "ggthemes::Blue-Green Sequential", direction = -1,
    name = "Number of studies",
    guide = guide_colorbar(
      title.position = "top",
      direction = "horizontal"
    )
  ) +
  theme_void() +
  theme(
    legend.background = element_rect(fill = "white", color = "white"),
    legend.position = "inside",
    legend.position.inside = c(0.5, 0.1),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 8)
  )
