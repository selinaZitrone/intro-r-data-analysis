library(tidyverse)
# Create some dummy data
dummy_data <- tibble(
  x = seq(1, 10, length.out = 20),
  y = rnorm(20)
)

# Scatterplot of the dummy data
dummy_data %>%
  ggplot(aes(x = x, y = y)) +
  geom_point()

# Define a custom function that predicts y-values based on x-values
# and some parameters a, b
# The function has default values for a and b defined. If not specified otherwise
# a and b are 1.

empirical_function <- function(x, a = 1, b = 1) {
  # Define the formula
  Y <- (a * x^b) / (x^b + exp(b))
  return(Y)
}

# Examples of how to use the custom function
# Use it just like any other function
empirical_function(x = 5) # x = 5, a and b are default values 1
empirical_function(x = 5, a = 5, b = 3) # change the default values of a and b in the calculation


# Add a line for the empirical function to the scatterplot ----------------

# Option 1: Use geom_function

# https://ggplot2.tidyverse.org/reference/geom_function.html
# With default values of a and b
dummy_data %>%
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  geom_function(fun = empirical_function, color = "red")

# If you want to change the default values for a and b, you can do it inside
# geom_function
dummy_data %>%
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  geom_function(fun = empirical_function,
                args = list(a = 5, b = 3), # args of the fun can be changed here
                color = "red")

# Option 2: Add predictions to the tibble first and then plot them using geom_line
dummy_data <- dummy_data %>% mutate(
  y_pred = empirical_function(x = x), # with default a and b
  y_pred_ab = empirical_function(x = x, a = 5, b = 3) # values of a and b changed
)

dummy_data %>%
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  geom_line(aes(y = y_pred), color = "red") +
  geom_line(aes(y = y_pred_ab), color = "blue")
