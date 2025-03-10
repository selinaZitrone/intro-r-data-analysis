# Load necessary libraries --------------------
library(tidyverse)
library(caret)       # For model evaluation like confusion matrix, AUC, etc.
library(pROC)        # For AUC computation

# Simulated data -------------------------------------------------
# Creating a fake dataset with environmental variables and a species presence/absence response
set.seed(123)  # for reproducibility
n <- 200
sim_data <- tibble(
  temperature = rnorm(n, mean = 15, sd = 5),
  precipitation = rnorm(n, mean = 100, sd = 20),
  altitude = runif(n, min = 0, max = 1000)
) |>
  # Create a presence/absence response variable
  mutate(
    presence = if_else(0.1 * temperature + 0.05 * precipitation - 0.001 * altitude + rnorm(n) > 5, 1, 0)
  )

# Check the first rows of the dataset
print(head(sim_data))

# Fitting the Logistic Regression Model --------------------
# Here we use a generalized linear model with binomial family for presence/absence data
model <- glm(presence ~ temperature + precipitation + altitude, family = binomial, data = sim_data)
summary(model)

# Interpret the output:
# The model output shows the coefficient estimates for each predictor.
# A positive coefficient suggests that as the predictor increases, the likelihood of species presence increases.
# Look for statistically significant predictors (p-value < 0.05) to determine which predictors are important.

# Predicting on the simulated dataset --------------------
# Get predicted probabilities of presence
sim_data <- sim_data |>
  mutate(pred_prob = predict(model, type = "response"))

# For evaluation, we can compute a cutoff (e.g., 0.5) to classify presence vs absence
sim_data <- sim_data |>
  mutate(pred_presence = if_else(pred_prob >= 0.5, 1, 0))
print(head(sim_data))

# Model Evaluation and Performance --------------------
# Create a confusion matrix using caret's function
conf_mat <- table(observed = sim_data$presence, predicted = sim_data$pred_presence)
print(conf_mat)

# Compute AUC for model performance
roc_obj <- roc(sim_data$presence, sim_data$pred_prob)
auc_value <- auc(roc_obj)
print(paste("AUC:", auc_value))

# Interpretation:
# A higher AUC (closer to 1) indicates better model performance at distinguishing between presence and absence.
# The confusion matrix provides insight into the balance between true positives and false negatives.

# END OF SCRIPT --------------------
