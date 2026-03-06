# 1. RStudio project -------------------------------------------------------

# Look at the first rows of the built-in penguins dataset
head(penguins)

# How many rows and columns does it have?
nrow(penguins)
ncol(penguins)

# Summary statistics
summary(penguins)

# Create a variable
my_variable <- "Hello, RStudio!"

# Create a simple plot
plot(
  penguins$flipper_len,
  penguins$body_mass,
  xlab = "Flipper Length (mm)",
  ylab = "Body Mass (g)",
  main = "Penguin Body Mass vs Flipper Length",
  pch = 20,
  col = "steelblue"
)

# 2. Introduction to R ----------------------------------------------------

species <- c(
  "MountainBeaver",
  "Cow",
  "GreyWolf",
  "Goat",
  "GuineaPig",
  "Diplodocus",
  "AsianElephant",
  "Donkey",
  "Horse",
  "PotarMonkey",
  "Cat",
  "Giraffe",
  "Gorilla",
  "Human",
  "AfricanElephant",
  "Triceratops",
  "RhesusMonkey",
  "Kangaroo",
  "GoldenHamster",
  "Mouse",
  "Rabbit",
  "Sheep",
  "Jaguar",
  "Chimpanzee",
  "Rat",
  "Brachiosaurus",
  "Mole",
  "Pig"
)

bodywt_kg <- c(
  1.4,
  465,
  36.3,
  27.7,
  1.,
  11700,
  2547,
  187.1,
  521,
  10,
  3.3,
  529,
  207,
  62,
  6654,
  9400,
  6.8,
  35,
  0.1,
  0.02,
  2.5,
  55.5,
  100,
  52.2,
  0.3,
  87000,
  0.1,
  192
)

brainwt_kg <- c(
  0.0081,
  0.423,
  0.1195,
  0.115,
  0.0055,
  0.05,
  4.603,
  0.419,
  0.655,
  0.115,
  0.0256,
  0.68,
  0.406,
  1.32,
  5.712,
  0.07,
  0.179,
  0.056,
  0.001,
  0.0004,
  0.0121,
  0.175,
  NA,
  0.44,
  0.0019,
  0.1545,
  NA,
  0.18
)

## Variables and vectors

# 1. What is the 13th species?
species[13]

# 2. Species at positions 6, 13, and 14
species[c(6, 13, 14)]

# 3. How many species? Save in n_species
n_species <- length(species)
n_species

## Vector arithmetic

# 4. Brain-to-body weight ratio
ratio <- brainwt_kg / bodywt_kg
ratio

# 5. Convert body weight to grams
bodywt_g <- bodywt_kg * 1000
bodywt_g

## Functions and missing values

# 6. Mean body weight and brain weight
mean(brainwt_kg) # returns NA because of missing values
mean(brainwt_kg, na.rm = TRUE) # remove NAs before calculating

# 7. sum and median brain weight
sum(brainwt_kg, na.rm = TRUE)
median(brainwt_kg, na.rm = TRUE)

## Optional tasks

# Round ratio to 2 decimal places
round(ratio, digits = 2)

# Other functions
min(brainwt_kg, na.rm = TRUE)
max(brainwt_kg, na.rm = TRUE)
sum(brainwt_kg, na.rm = TRUE)
sd(brainwt_kg, na.rm = TRUE)

# Count missing values
sum(is.na(brainwt_kg))

# 3. Tibbles ---------------------------------------------------------------

# install.packages("tibble")
library(tibble)

# Create the tibble
animals <- tibble(
  species = species,
  bodywt_kg = bodywt_kg,
  brainwt_kg = brainwt_kg
)

# Explore the tibble
view(animals)
summary(animals)
str(animals)
nrow(animals)
ncol(animals)
names(animals)

# Rows 1, 5, and 7 and the columns species and bodywt_kg
animals[c(1, 5, 7), c("species", "bodywt_kg")]

# Rows 1 to 10, all columns
animals[1:10, ]

# Select the column bodywt_kg as a vector
animals$bodywt_kg

## Optional tasks

# Last row and column without using numbers
animals[nrow(animals), ncol(animals)]

# Mean body weight
mean_wt <- mean(animals$bodywt_kg)
mean_wt

# Add new column with body/brain ratio
animals$ratio_body_brain <- animals$bodywt_kg / animals$brainwt_kg
animals

# 4. Readr -----------------------------------------------------------------

library(tidyverse)

## Read a CSV file
trees <- read_csv("data/tree_growth.csv")
trees
summary(trees)
mean(trees$height_m)

## Read an Excel file
library(readxl)
birds <- read_excel("data/bird_observations.xlsx")
birds
summary(birds)

## Write data to a file
write_csv(trees, file = "data/trees_copy.csv")

## Challenge: slightly messy file (metadata lines on top)
# This doesn't work correctly:
# read_csv("data/water_quality.csv")

# Skip the 4 metadata lines at the top:
water <- read_csv("data/water_quality.csv", skip = 4)
water

## Challenge: even messier file (metadata + semicolon delimiter)
soil <- read_delim(
  "data/soil_nutrients_messy.csv",
  delim = ";",
  skip = 4
)
soil

# Clean messy column headers with janitor
# install.packages("janitor")
soil |> janitor::clean_names()
