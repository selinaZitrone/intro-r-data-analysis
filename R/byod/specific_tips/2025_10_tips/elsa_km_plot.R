# Code for the paper (Figure 2c) can be found here:
# https://github.com/fraunhofer-izi/Fandrei_et_al_2024
# You can download the project, open it in R and run the code
# You can even have a look at their data. Since you do not have your own,
# maybe you could have a look at their data to see if you can
# work with it and try things that would also be interesting for you

# In this script, I try to reproduce the kaplan meyer plot from the paper
# There are also specific R packages for this
# Here is a nice tutorial showing how to do the plot and how to calculate survival curves
# I took my example from there and combined it a bit with the code from the paper
# https://www.emilyzabor.com/survival-analysis-in-r.html#kaplan-meier-plots

# Links for packages
# https://www.danieldsjoberg.com/ggsurvfit/index.html
# https://rpkgs.datanovia.com/survminer/reference/ggsurvplot.html

# Load necessary libraries
library(tidyverse)
library(survival) # for the lung dataset
library(ggsurvfit) # for plotting survival curves (Kaplan-Meier)
library(survminer) # alternative for plotting survival curves (Kaplan-Meier)

# Look at example data
# Survival in patients with advanced lung cancer
head(lung)
# check the help for info on the data
?lung

# Calculate survival curves using Kaplan-Meier method
# Here: Compare survival between sexes
surv_fit <- survfit(Surv(time, status) ~ sex, data = lung)
# Key metrics in the survival fit

# time: the timepoints at which the curve has a step, i.e. at least one event occurred
# surv: the estimate of survival at the corresponding time
str(surv_fit)

# Kaplan-Meier plot ------------------------------------------------------------

# Option 1: Use ggsurvfit package
ggsurvfit(surv_fit) +
  labs(
    x = "Days",
    y = "Overall survival probability"
  ) +
  add_confidence_interval() +
  add_risktable()

# Option 2: Use R package survminer (this is what the paper uses)
# check ?ggsurvplot for more options
# You can also check the code in the paper to see what they did

# basic plot
ggsurvplot(
  surv_fit,
  data = lung
)

# some more options (customize and turn options on/off)
# Male=1 Female=2
# Define a color palette
palette_sex <- c(
  "1" = "#1f77b4", #
  "2" = "#ff7f0e" # orange
)

g <- ggsurvplot(
  surv_fit,
  data = lung,
  color = "sex",
  legend.title = "Sex",
  legend.labs = c("Male", "Female"),
  # size = 1,
  conf.int = FALSE,
  pval = TRUE,
  pval.coord = c(0.1, 0.1), # coordinates for p-value (bottom left)
  palette = c("#1f77b4", "#ff7f0e"), # custom colors
  pval.size = 8,
  surv.median.line = "hv",
  risk.table = TRUE,
  tables.col = "sex",
  tables.y.text = FALSE,
  risk.table.title = "No. at risk",
  risk.table.fontsize = 5,
  tables.height = 0.2
)

g

# Further customize the elements (plot and risk table) separately ------------

# Change the plot part of the ggsurvplot object
g_plot <- g$plot +
  theme_classic() +
  theme(
    legend.key.width = unit(1.1, "cm"),
    legend.position = "top"
  ) +
  labs(x = "Time (days)", y = "Overall Survival Probability") +
  guides(color = guide_legend(override.aes = list(shape = NA)))

g_plot

# Extract and modify the survival table part of the ggsurvplot
g_table <- g$table +
  theme_void() +
  theme(
    plot.title = element_text(size = 12, vjust = -2),
    plot.margin = unit(c(-0.5, 0, 0, 0), "cm"),
    legend.position = "none"
  )
g_table
# Combine the survival table and the plot in a custom way

# Or solve it with patchwork and an inset
library(patchwork)
# This does not look nice yet, but I did not have time to adjust it further
# You can play around with the heights and the positions
g_plot +
  theme(legend.position = "top") +
  inset_element(
    g_table,
    left = 0.01,
    right = 0.99,
    bottom = -0.25,
    top = 0
  ) +
  plot_layout(heights = c(5, 1.4))
