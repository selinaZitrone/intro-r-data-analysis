# Purpose: How to do linear models (including ANOVAs) in R


library(palmerpenguins)

# ANOVA example -----------------------------------------------------------
# ANOVA is just a special case of linear models, where we only have
# categorical predictors. We can use the lm function to fit a linear model
