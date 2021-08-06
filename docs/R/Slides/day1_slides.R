# Code for slides from day 3
# Author: Selina Baldauf (selina.baldauf@fu-berlin.de)
# Hint: navigate the script by opening the document outline (top right corner, next to Source button)

# 1. R as calculator ---------------------------------------------------------

# Arithmetic operators ----------------------------------------------------

# Addition
2 + 2
# Subtraction
5.432 - 34234
# Multiplication
33 * 42
# Division
3 / 42
# Modulo (Remainder)
2 %% 2
# Power
2^2
# Combine operations
((2 + 2) * 5)^(10 %% 10)


# Relational operators ----------------------------------------------------

2 == 2
2 != 2
33 <= 32
20 < 20


# Logical operators -------------------------------------------------------

(3 < 1) & (3 == 3) # FALSE & TRUE = FALSE
(1 < 3) & (3 == 3) # TRUE & TRUE = TRUE
(3 < 1) & (3 != 3) # FALSE & FALSE = FALSE
(3 < 1) | (3 == 3) # FALSE | TRUE = TRUE
(1 < 3) | (3 == 3) # TRUE | TRUE = TRUE
(3 < 1) | (3 != 3) # FALSE | FALSE = FALSE


# 2. Variables and data types ---------------------------------------------


# Variables ---------------------------------------------------------------

# create a variable
radius <- 5
# use it in a calculation and save the result
circumference <- 2 * pi * radius
# change value of variable radius
radius <- radius + 1
# print a variable's value -----------------------
radius # just use the name to print the value
# or print it in a sentence
print(paste0("With a radius of ", radius, " the circumference is ",
             circumference))
# Remove variables from global environment -------
rm(radius) # remove variable radius
rm(list = ls()) # remove all elements


# Data types --------------------------------------------------------------

var <- 123L
typeof(var)
is.double(var)
is.integer(123)

var2 <- TRUE
is.logical(var2)
is.character(var2)

var3 <- "TRUE"
is.logical(var3)
is.character(var3)

as.character(1L)

as.numeric(TRUE)
as.numeric(FALSE)

as.integer("hello")
as.integer("2")


# Implicit type conversion ------------------------------------------------

typeof(1L + 2.5) # integer -> double
typeof(1L + TRUE) # logical -> integer (TRUE = 1, FALSE = 0)
typeof(1.34 + FALSE) # logical -> double
typeof("hello" + FALSE) # Error: no implicit conversion from string to other data types



# 3. Vectors --------------------------------------------------------------


# c() ---------------------------------------------------------------------

lgl_var <- c(TRUE, TRUE, FALSE)
dbl_var <- c(2.5, 3.4, 4.3)
int_var <- c(1L, 45L, 234L)
chr_var <- c("These are", "just", "some strings")

# Combine multiple vectors
v1 <- c(1,2,3)
v2 <- c(800, 83, 37)
v3 <- c(v1, v2)

c(int_var, lgl_var)

# : and seq and rep -------------------------------------------------------

1:10
seq(from = 1, to = 10, by = 1) # specify increment of sequence with by
seq(from = 1, to = 10, length.out = 10) # specify desired length with length.out

rep("hello", times = 5)
rep(c(TRUE, FALSE, TRUE), times = 2) # repeat the whole vector twice
rep(c(1, 2 ,3), each = 2) # repeat each element of the vector twice


# Working with vectors ----------------------------------------------------

# list of 10 biggest cities in Europe
cities <- c("Istanbul", "Moscow", "London", "Saint Petersburg", "Berlin", "Madrid", "Kyiv", "Rome", "Bucharest", "Paris")
population <- c(15.1e6, 12.5e6, 9e6, 5.4e6, 3.8e6, 3.2e6, 3e6, 2.8e6, 2.2e6, 2.1e6)
area_km2 <- c(2576, 2561, 1572, 1439,891,604, 839, 1285, 228, 105 )

length(cities)
# dividing by a vector of the same length
population / area_km2
# dividing by a vector of lenght 1
mean_population <- mean(population) # calculate the mean of vector population
population / mean_population


# relational and logical operators ----------------------------------------

population > mean_population
# population larger than mean population OR population larger than 3 million
population > mean_population | population > 3e6

cities == "Istanbul" | cities == "Berlin" | cities == "Madrid"
# same as
to_check <- c("Istanbul", "Berlin", "Madrid")
cities %in% to_check


# Indexing vectors --------------------------------------------------------

cities[5]
cities[1:3] # the three most populated cities
cities[length(cities)] # the last entry of the cities vector

# changing values in vectors
# first copy the original vector to leave it untouched
population_new <- population
# Update Istanbul (1) and Rome(8)
population_new[c(1, 8)] <- c(20e6, NA) # NA means missing value
# Update Paris (10)
population_new[10] <- population_new[10] - 200000
population_new


# Indexing with logical vectors -------------------------------------------

mega_city <- population > mean_population
mega_city
cities[mega_city] # or short: cities[population > mean_population]
population[ cities %in% c("Berlin", "Paris", "Stockholm", "Madrid")]



# 2. Tibbles --------------------------------------------------------------
library(tibble)
# data frame
data.frame(cities = cities,
           population = population,
           area_km2 = area_km2)
# tibble
cities_tbl <- tibble(
  cities = cities,
  population = population,
  area_km2 = area_km2
)

cities_tbl

str(cities_tbl)
nrow(cities_tbl)
ncol(cities_tbl)
names(cities_tbl)
view(cities_tbl)


# 3. Importing data -------------------------------------------------------
library(readr)
# or
#library(tidyverse)
#write_csv(dat, file = "./data-clean/your_data.csv") # comma delimiter
#dat <- read_csv("./data/your_data.csv") # comma delimiter




