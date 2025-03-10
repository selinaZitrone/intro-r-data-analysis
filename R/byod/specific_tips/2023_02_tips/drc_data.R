library(drc)
# Have a look at this tutorial:
# They don't use ggplot, so I adapted it here
# drc package has data included (the ryegrass data I'm using here also comes
# from the package)
# http://www.darrenkoppel.com/2020/09/04/dose-response-modelling-and-model-selection-in-r/

# Show all functions that can be fit
# All these function can be fit with drm
getMeanFunctions()

# Example with one model (log-logistic) -----------------------------------

# plot the data
ryegrass %>%
  ggplot(aes(x = conc, y = rootl)) +
  geom_point()

# Log-logistic (LL.4)
model_1 <- drm(rootl ~ conc,
  data = ryegrass,
  fct = LL.4() # which model you want to fit
)

# Plot model with base plot (but not so flexible)
plot(model_1, type = "all")

# Plot using ggplot (more flexible but needs some steps)

# Step 1: predict root length using the model
# Step 1.1: Create a data frame that contains some concentration values to use
# in the predict function
predict_data <- data.frame(
  conc = seq(min(ryegrass$conc), max(ryegrass$conc), by = 0.05)
)
# Step 1.2: Predict root length values for the predict data using the model
predict_data$LL4_pred <- predict(model_1, newdata = predict_data)

# Step 2: Add predictions to the plot
ryegrass %>%
  ggplot(aes(x = conc, y = rootl)) +
  geom_point() +
  geom_line(data = predict_data, aes(y = LL4_pred)) # Add a line with the predictions


# Compare multiple models to each other -----------------------------------

# fit some models
# I took them from the example in the tutorial
# Read more about the models here: http://www.darrenkoppel.com/2020/09/04/dose-response-modelling-and-model-selection-in-r/
model.W23 <- drm(rootl ~ conc,
  data = ryegrass, fct = W2.3()
)
model.W24 <- drm(rootl ~ conc,
  data = ryegrass, fct = W2.4()
)
model.LL4 <- drm(rootl ~ conc,
  data = ryegrass, fct = LL.4()
)
model.W14 <- drm(rootl ~ conc,
  data = ryegrass, fct = W1.4()
)

# Predict new root length for all 4 models
predict_data <- data.frame(
  conc = seq(min(ryegrass$conc), max(ryegrass$conc), by = 0.05)
)

predict_data$W23 <- predict(model.W23, newdata = predict_data)
predict_data$W24 <- predict(model.W24, newdata = predict_data)
predict_data$LL4 <- predict(model.LL4, newdata = predict_data)
predict_data$W14 <- predict(model.W14, newdata = predict_data)

head(predict_data)

# To plot the different models in different colors in the plot, use pivot longer
predict_data <- predict_data %>%
  pivot_longer(!conc, names_to = "model", values_to = "rootl")

# Add the models to the ggplot
ryegrass %>%
  ggplot(aes(x = conc, y = rootl)) +
  geom_point() +
  geom_line(data = predict_data,
            aes(color = model)) # Add a line with the predictions

