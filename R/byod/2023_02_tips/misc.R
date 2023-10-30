library(data.table)
library(tidyverse)
file_names_to_read <- list.files(path = "data", pattern = "birdabund", full.names = TRUE)
file_names <- list.files(path = "data", pattern = "birdabund")
file_names <- stringr::str_replace(file_names, ".csv", "")

# Read all files
my_files <- lapply(file_names_to_read, read_csv)
names(my_files) <- file_names

# put them together
data.table::rbindlist(my_files, idcol = "id")


# pivot longer ------------------------------------------------------------
time_permin <- c(45, 90, 180)
first <- c(9 * 10^10, 9 * 10^10, 0 * 10^10)
second <- c(9 * 10^10, 9 * 10^10, 6 * 10^10)
third <- c(8 * 10^10, 10 * 10^10, 5 * 10^10)
my_data <- data.frame(
  time_permin = time_permin, first = first,
  second = second, third = third
)
# point plot with one data set
my_data %>%
  pivot_longer(c(first,second,third), names_to = "end_point") %>%
  ggplot(aes(x = time_permin, y = value, color = end_point)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(breaks = c(5 * 10^10, 8 * 10^10, 9 * 10^10))


# Temperature data --------------------------------------------------------
library(tidyverse)
temp <- read_csv("data/ntl_temp.csv")

# Look at the data and it'S columns
temp

# Simple scatterplot -> Too many points because daily measurements
# This might take some time to actually plot

# ggplot(temp, aes(x = year, y = ave_air_temp_adjusted)) +
#   geom_point() +
#   geom_smooth(method = "lm")

# Solution: Plot mean annual temperature

# Plot annual mean and standard error of mean
ggplot(temp, aes(x = year, y = ave_air_temp_adjusted)) +
  stat_summary() +
  geom_smooth(method = "lm")

# Use dplyr to summarize the temperature to the mean temperature
mapt <- temp %>%
  group_by(year) %>%
  summarize(temp = mean(ave_air_temp_adjusted, na.rm = TRUE))

# plot the mean temperature from the new table
ggplot(mapt, aes(x = year, y = temp)) +
  geom_point() +
  geom_smooth(method = "lm")


dummy <- tibble(genes = c("a", "b", "c"),
                tissue1_a = c(1,2,3),
                tissue1_b =c(1,2,3),
                tissue2_a = c(1,2,3),
                tissue2_b = c(1,2,3))

for_pca <- dummy %>%
  pivot_longer(tissue1_a:tissue2_b, names_to = "tissue") %>%
  pivot_wider(names_from = genes, values_from = value) %>%
  mutate(tissue = stringr::str_replace(tissue, "_a|_b", ""))

pca_res <- for_pca %>%
  select(a:c) %>%
  as.matrix() %>%
  prcomp()

factoextra::fviz_pca_var(pca_res,
         habillage = for_pca$tissue,
         addEllipses = TRUE,
         ellipse.level = 0.95,
         palette = "Dark2"
)

