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


# 2. Data types and vectors -----------------------------------------------

v1 <- c(1, "a", 2, 3)
v2 <- c(TRUE, TRUE, 1L, FALSE, 0L)
v3 <- c(0, "23", 5, 7)
v4 <- c(4L, 6L, 23.5345)
v5 <- c(TRUE, "a", FALSE, "FALSE")

# Checking the data types of the vectors
typeof(v1)
typeof(v2)
typeof(v3)
typeof(v4)
typeof(v5)

# explicit conversion to int
as.integer(v1)
as.integer(v2)
as.integer(v3)
as.integer(v4)
as.integer(v5)

# explicit conversion to string
as.character(v1)
as.character(v2)
as.character(v3)
as.character(v4)
as.character(v5)


# 2.2 Working with vectors ----------------------------------------------------

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

brainwt_g <- c(
  8.1, 423, 119.5, 115, 5.5, 50, 4603, 419, 655, 115, 25.6,
  680, 406, 1320, 5712, 70, 179, 56, 1, 0.4, 12.1, 175,
  157, 440, 1.9, 154.5, 3, 180
)

# Goes through every element in species and returns TRUE if it appears in animals_to_check
animals_to_check <- c("Snail", "Goat", "Chimpanzee", "Rat", "Dragon", "Eagle")
species %in% animals_to_check
species[species %in% animals_to_check]

# Goes through every element in animals_to_check and returns TRUE if it appears in species
animals_to_check %in% species

#Convert brain weight from g to kg
brainwt_kg <- brainwt_g / 1000

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
brainwt_g_new <- c(40, NA, 500)
bodywt_kg_new <- c(18, 0.01, 550)

species <- c(species, species_new)
brainwt_g <- c(brainwt_g, brainwt_g_new)
bodywt_kg <- c(bodywt_kg, bodywt_kg_new)

# na.rm = FALSE
mean(brainwt_g)
# na.rm = TRUE
mean(brainwt_g, na.rm = TRUE)



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

brainwt_g <- c(
  8.1, 423, 119.5, 115, 5.5, 50, 4603, 419, 655, 115, 25.6,
  680, 406, 1320, 5712, 70, 179, 56, 1, 0.4, 12.1, 175,
  157, 440, 1.9, 154.5, 3, 180
)
# Creating the tibble
animals <- tibble(
  species = species,
  bodywt_kg = bodywt_kg,
  brainwt_kg = brainwt_g / 1000
)

view(animals)

summary(animals)
str(animals)
nrow(animals)
ncol(animals)
names(animals)

# rows 1, 5, and 7 and the columns `species` and `bodywt_kg`
animals[c(1,5,7), c("species", "bodywt_kg")]

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

# add category
animals$bodywt_cat <- ifelse(animals$bodywt_kg > mean_wt,
                             "heavy",
                             "light")

# 4. Readr ----------------------------------------------------------------

#library(tidyverse)
library(readr)

# write
write_csv(x = animals, file = "./data/animals.csv") # write as csv
write_tsv(x = animals, file = "./data/animals.txt") # write as txt

#Read the same data back into R:
animals_csv <- read_csv("./data/animals.csv") # read the csv
animals_tsv <- read_tsv("./data/animals.txt") # read the txt

