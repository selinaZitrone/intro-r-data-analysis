# correlation plots -------------------------------------------------------
# Many options. To see an article showing correlation plots with different packages,
# see this website: https://r-coder.com/correlation-plot-r/

# Some cool packages and options are shown below

# 1. Using corrplot package ---------------------------------------------
# many different options. To see more, check out the website
# # https://taiyun.github.io/corrplot/
library(corrplot)

# select the numeric columns from the penguins data set
# drop all rows containing NAs (correlation coefficients cannot be calculated if
# vectors contain NA)
# calculate pearson correlation matrix
M <- cor(penguins %>% dplyr::select(bill_length_mm:body_mass_g) %>% drop_na())
# Two different options of correlation plots
corrplot::corrplot(M)
corrplot::corrplot.mixed(M)


# Using GGally ------------------------------------------------------------
library(GGally)
# Plot correlations for the numeric columns (3:6) of the penguins data
ggpairs(penguins, columns = 3:6)
# Add a color aesthetic to distinguish between the species:
ggpairs(penguins, columns = 3:6, mapping = aes(color = species))
# for more information, check out the help function of ggpairs:
?ggpairs
