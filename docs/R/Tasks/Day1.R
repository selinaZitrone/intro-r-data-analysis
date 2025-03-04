# 1. Rstudio project ------------------------------------------------------

# Look at the first lines of the iris dataset
head(iris)
# What is the iris dataset -> Call the help
?iris
# How many rows and columns does the data set have?
rownum <- nrow(iris)
colnum <- ncol(iris)
print(paste0("The iris dataset has ", rownum, " rows and ", colnum, " columns."))
# Some summary statistics on the iris data set
summary(iris)

# create a plot
plot(iris$Petal.Length, iris$Petal.Width,
  xlab = "Petal Length",
  ylab = "Petal Width",
  main = "Petal Width vs Petal Length",
  pch = 20,
  col = ifelse(iris$Species == "setosa", "coral1",
    ifelse(iris$Species == "virginica", "cyan4",
      ifelse(iris$Species == "versicolor",
        "darkgoldenrod2", "grey"
      )
    )
  )
)
# add a legend
legend("bottomright", c("setosa", "virginica", "versicolor"),
  col = c("coral1", "cyan4", "darkgoldenrod2"), pch = 20
)


# 2. Working with Vectors -----------------------------------------------

species <- c(
  "MountainBeaver", "Cow", "GreyWolf", "Goat",
  "GuineaPig", "Diplodocus", "AsianElephant", "Donkey",
  "Horse", "PotarMonkey", "Cat", "Giraffe",
  "Gorilla", "Human", "AfricanElephant", "Triceratops",
  "RhesusMonkey", "Kangaroo", "GoldenHamster", "Mouse",
  "Rabbit", "Sheep", "Jaguar", "Chimpanzee",
  "Rat", "Brachiosaurus", "Mole", "Pig"
)

bodywt_kg <- c(
  1.4, 465, 36.3, 27.7, 1., 11700, 2547, 187.1,
  521, 10, 3.3, 529, 207, 62, 6654, 9400,
  6.8, 35, 0.1, 0.02, 2.5, 55.5, 100, 52.2,
  0.3, 87000, 0.1, 192
)

brainwt_kg <- c(
  0.0081, 0.423, 0.1195, 0.115, 0.0055, 0.05,
  4.603, 0.419, 0.655, 0.115, 0.0256, 0.68,
  0.406, 1.32, 5.712, 0.07, 0.179, 0.056,
  0.001, 0.0004, 0.0121, 0.175, 0.157, 0.44,
  0.0019, 0.1545, 0.003, 0.18
)

# Goes through every element in species and returns TRUE if it appears in animals_to_check
animals_to_check <- c("Snail", "Goat", "Chimpanzee", "Rat", "Dragon", "Eagle")
# Goes through every element in animals_to_check and returns TRUE if it appears in species
animals_to_check %in% species
animals_to_check[animals_to_check %in% species]

# Or using the species vector instead
species %in% animals_to_check
species[species %in% animals_to_check]

# Calculate some descriptive statistics
mean(brainwt_kg) # mean
sd(brainwt_kg) # standard deviation
median(brainwt_kg) # median

# species with brain weight larger than mean
species[brainwt_kg > mean(brainwt_kg)]

# ratio brain / body weight (%)
brain_body_ratio <- brainwt_kg / bodywt_kg * 100

# Animals with larger brain to body ratio than humans
# New variable for human brain to body ratio
bbr_human <- brain_body_ratio[species == "Human"]
# Are there animals that have a larger brain to body ratio than humans?
brain_body_ratio > bbr_human
# Which are these animals
species[brain_body_ratio > bbr_human]

# or short
species[brain_body_ratio > brain_body_ratio[species == "Human"]]

### Extras

brain_body_ratio <- round(brain_body_ratio, digits = 4)

# Animal with smallest ratio
species[brain_body_ratio == min(brain_body_ratio)]

# Adding new species
species_new <- c("Eagle", "Snail", "Lion")
brainwt_kg_new <- c(0.0004, NA, 0.5)
bodywt_kg_new <- c(18, 0.01, 550)

species <- c(species, species_new)
brainwt_kg <- c(brainwt_kg, brainwt_kg_new)
bodywt_kg <- c(bodywt_kg, bodywt_kg_new)

# na.rm = FALSE
mean(brainwt_kg)
# na.rm = TRUE
mean(brainwt_kg, na.rm = TRUE)

# 3. Tibbles -----------------------------------------------------------------
# install.packages("tibble")
library(tibble)

species <- c(
  "MountainBeaver", "Cow", "GreyWolf", "Goat",
  "GuineaPig", "Diplodocus", "AsianElephant", "Donkey",
  "Horse", "PotarMonkey", "Cat", "Giraffe",
  "Gorilla", "Human", "AfricanElephant", "Triceratops",
  "RhesusMonkey", "Kangaroo", "GoldenHamster", "Mouse",
  "Rabbit", "Sheep", "Jaguar", "Chimpanzee",
  "Rat", "Brachiosaurus", "Mole", "Pig"
)

bodywt_kg <- c(
  1.4, 465, 36.3, 27.7, 1., 11700, 2547, 187.1,
  521, 10, 3.3, 529, 207, 62, 6654, 9400,
  6.8, 35, 0.1, 0.02, 2.5, 55.5, 100, 52.2,
  0.3, 87000, 0.1, 192
)

brainwt_kg <- c(
  0.0081, 0.423, 0.1195, 0.115, 0.0055, 0.05,
  4.603, 0.419, 0.655, 0.115, 0.0256, 0.68,
  0.406, 1.32, 5.712, 0.07, 0.179, 0.056,
  0.001, 0.0004, 0.0121, 0.175, 0.157, 0.44,
  0.0019, 0.1545, 0.003, 0.18
)
# Creating the tibble
animals <- tibble(
  species = species,
  bodywt_kg = bodywt_kg,
  brainwt_kg = brainwt_kg
)

view(animals)

summary(animals)
str(animals)
nrow(animals)
ncol(animals)
names(animals)

# rows 1, 5, and 7 and the columns `species` and `bodywt_kg`
animals[c(1, 5, 7), c("species", "bodywt_kg")]

# select rows 1 to 10, all columns
animals[1:10, ]

# select the column `bodywt_kg` as a vector using `$`
animals$bodywt_kg

## Extras
# last row and column
animals[nrow(animals), ncol(animals)]

# mean body weight
mean_wt <- mean(animals$bodywt_kg)
mean_wt

# add new column
animals$ratio_body_brain <- animals$bodywt_kg / animals$brainwt_kg
animals


# 4. Readr ----------------------------------------------------------------

# library(tidyverse)
library(readr)

# write (Note: Make sure that the /data folder exists in your working directory
write_csv(x = animals, file = "data/animals.csv") # write as csv
write_tsv(x = animals, file = "data/animals.txt") # write as txt

# Read the same data back into R:
animals_csv <- read_csv("data/animals.csv") # read the csv
animals_tsv <- read_tsv("data/animals.txt") # read the txts

# Readr challanging datasets

# in my case, the datasets are in data/read_challenge. Adjust the path if for
# you, this is different

# dataset 1: Dataset 1 with Metadata on top and messy headers
insect_counts <- read_csv("data/read_challenge/metadata_and_messy_header.csv",
                          skip = 3 # skip the metadata on top
                          )
# check out the dataset
insect_counts
# Fix the column headers with clean_names
insect_counts <- janitor::clean_names(insect_counts)
# check out the dataset
insect_counts

# dataset 2: Same dataset but as excel
insect_counts_excel <- readxl::read_excel("data/read_challenge/metadata_and_messy_header.xlsx",
                                  skip = 3, # skip the metadata on top
                                  sheet = "Data" # read the second sheet, not the first
                                  )
# Fix the column headers with clean_names
insect_counts_excel <- janitor::clean_names(insect_counts_excel)
# check out the dataset
insect_counts_excel
