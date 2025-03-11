# Load necessary libraries
library(tidyverse)
library(poLCA) # for latent class analysis

# The poLCA package contains a dataset called "values"
data(values)
# checkout what the data is about
# 4 questions A-D with 2 response options 1 & 2
head(values)
?values

# Convert the data to tibble for nicer printing in the console
values <- as_tibble(values)

# Check for missing values in the dataset
# All values are 0 so we have no missing values
colSums(is.na(values))

# LCA --------------------------------------------------------------------------

# Build the LCA formula
# left formula side: combining indicator variables to use (i.e. column headers)
# here we use all four indicator variables (A, B, C, D)
# these are the indicators that will determine class membership
# right formula side: the predictors, in our case 1 because we are only interested
# in the class membership and don't use external covariates to predict class membership
# if you want to use preditors, the formula would e.g. look like this:
# cbind(A, B, C, D) ~ age + gender + education
lca_formula <- cbind(A, B, C, D) ~ 1

# Run the latent class analysis with 2 latent classes
# check the help of poLCA to see which options you can give
?poLCA
# Try a model with 2 classes
lca_model <- poLCA(lca_formula, data = values, nclass = 2, na.rm = FALSE)

# Interpretation of the results:
# The conditional item response probabilities
# (the chance of a response in a specific category) differ between the latent classes.
# For example, for variable A, class 1 has about a 28.6% probability for category 1,
# whereas class 2 shows only about 0.7% for that category.

# 72% are in class 1 and 28% in class 2

# Model fit: Have a look at AIC and BIc -> These values can also be compared between
# models (e.g. with a different number of classes)

# Plot response patterns by class ----------------------------------------------

# Add the predicted class memberships to the original dataset
values_with_class <- values |>
  mutate(predicted_class = lca_model$predclass)

# Convert to long format for easier plotting
values_long <- values_with_class |>
  pivot_longer(-predicted_class,
    names_to = "Variable",
    values_to = "Response"
  )

# Create a visualization of response patterns by class
ggplot(values_long, aes(x = Variable, fill = factor(Response))) +
  # position = "fill" shows proportions instead of counts
  geom_bar(position = "fill") +
  facet_wrap(~predicted_class) +
  labs(
    title = "Response Patterns by Latent Class",
    x = "Variable",
    y = "Proportion",
    fill = "Response"
  ) +
  scale_fill_manual(values = c("cyan4", "darkorange")) +
  theme_minimal()

# More complex LCA with predictors ---------------------------------------------
# Election dataset from the poLCA package
data(election)
?election

# Convert to tibble for nicer printing
election <- as_tibble(election)

# simplify the data for this example (no need to follow this)

election <- janitor::clean_names(election)

election <- election |>
  mutate(
    # simplify the parties into 3 categories
    party_simple = case_when(
      party %in% c(1, 2, 3) ~ "Democrat",
      party == 4 ~ "Independent",
      party %in% c(5, 6, 7) ~ "Republican",
      .default = NA_character_
    ),
    # make the gender variable more readable
    gender_f = case_when(
      gender == 1 ~ "Male",
      gender == 2 ~ "Female",
      .default = NA_character_
    )
  )
# select only the columns needed for the analysis
election <- dplyr::select(
  election, moralg,
  caresg, knowg, leadg, age, educ, gender_f, party_simple
)

# Create the formula for the LCA
lca_formula <- cbind(moralg, caresg, knowg, leadg) ~ age + educ + gender_f + party_simple

# na.rm = TRUE because now we have NAs
lca_model <- poLCA(lca_formula,
  data = election, nclass = 2, maxiter = 3000,
  na.rm = TRUE
)

# Interpretation of the results:
str(lca_model)

# Extract results and put in nice table ----------------------------------------

# Extract probabilites as tibble for each item in the list
step_1 <- map_dfr(
  lca_model$probs,
  as.data.frame
)

# Convert to tibble, convert rownames to a column
step_2 <- as_tibble(step_1, rownames = "class")

# turn class into a single number
step_2 <- mutate(
  step_2,
  class = case_when(
    str_detect(class, "class 1") ~ "class_1",
    str_detect(class, "class 2") ~ "class_2"
  )
)


# Add motiviation as a column from the names of the probability list
motivation <- rep(names(lca_model$probs), each = 2) # in your case 3

step_3 <- mutate(step_2, motivation = motivation)

# clean up the column names
step_4 <- janitor::clean_names(step_3)

# Reformat the table:
step_5 <- pivot_longer(step_4,
  cols = !c(class, motivation),
  names_to = "likert_scale",
  values_to = "probs"
)
step_6 <- pivot_wider(step_5,
  names_from = class,
  values_from = probs
)

# Plot the results -----------------------------------------------------------

# First we can extract the model coefficients (i.e. the effect size)
lca_model$coeff

lca_coef <- as_tibble(lca_model$coeff, rownames = "term") |>
  rename(estimate = V1)

# Plot the coefficients using ggplot2 in an easy way
ggplot(lca_coef, aes(
  x = term,
  y = estimate
)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Effect of Covariates on Class Membership",
    subtitle = "Positive values increase probability of being in Class 2 vs Class 1",
    x = "",
    y = "Coefficient (log-odds)"
  ) +
  theme_minimal()


# Plot of the response patterns by class ---------------------------------------
# Extract the item response probabilities
# we have 2 classes, 4 response options and 4 items
probs <- lca_model$probs
# this is how the probabilities look like
probs

# Create a tibble
probs_df <- tibble(
  class = rep(c("class 1", "class 2"), each = 16),
  item = rep(rep(c("moralg", "caresg", "knowg", "leadg"), each = 4), 2),
  response = rep(c("Extremely well", "Quite well", "Not too well", "Not well at all"), 8),
  probability = c(
    probs$moralg[1, ], probs$caresg[1, ],
    probs$knowg[1, ], probs$leadg[1, ],
    probs$moralg[2, ], probs$caresg[2, ],
    probs$knowg[2, ], probs$leadg[2, ]
  )
)

# Visualize the item response probabilities
ggplot(probs_df, aes(x = response, y = probability, fill = class)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~item, scales = "free_y") +
  labs(
    title = "Item Response Probabilities by Latent Class",
    x = "Response Category",
    y = "Probability"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


library(haven)

master_data <- read_sav("R/byod/specific_tips/2025_03_tips/data/Master data.sav")

# Step 1: Simplify and pivot the master data
master_data_simple <- master_data %>%
  select(ID, RM1:RM8) |>
  # Convert haven_labelled to regular R types before pivoting
  mutate(across(RM1:RM8, ~ as.numeric(zap_labels(.)))) %>%
  # Now pivot the data
  pivot_longer(cols = RM1:RM8, names_to = "Motivation", values_to = "likert_scale")

# Step 2: Calculate how many answers in each Motivation cat

master_data %>%
  select(ID, RM1:RM8) |>
  # Convert haven_labelled to regular R types before pivoting
  mutate(across(RM1:RM8, ~ as.numeric(zap_labels(.)))) %>%
  # Now pivot the data
  pivot_longer(cols = RM1:RM8, names_to = "Motivation", values_to = "likert_scale") %>%
  group_by(Motivation, likert_scale) %>%
  summarise(
    count_answers = n(),
    percentage_answers = n() / nrow(master_data) * 100,
    .groups = "drop"
  )
