1 + 1

# Look at the air quality dataset
head(airquality)

n_data_points <- nrow(airquality)

# Summary statistics
summary(airquality)

# Make a simple plot
plot(airquality$Wind, airquality$Temp, col = "steelblue")
