library(tidyverse)


# remove missing sex
penguins_sex <- filter(penguins, !is.na(sex))

# Fit ANOVA lm with interaction
lm3 <- lm(body_mass ~ sex + species + sex:species, data = penguins_sex)

# Post-hoc TukeyHSD
tk_result <- TukeyHSD(aov(lm3))

# have a look at the Results of the tukey test
tk_result


# Convert the results of the Tukey test to a tibble -----------------------
# 3 variables for the 3 types of comparisons

# results of all comparisons (interaction between sex and species)
interaction <- tk_result$`sex:species` %>%
  as.data.frame() %>%
  rownames_to_column(var = "comparison") %>%
  as_tibble()
# results of comparison between sex
sex <- tk_result$sex %>%
  as.data.frame() %>%
  rownames_to_column(var = "comparison") %>%
  as_tibble()
# resuls of comparison between species
species <- tk_result$species %>%
  as.data.frame() %>%
  rownames_to_column(var = "comparison") %>%
  as_tibble()


# Optional: bind all comparisons together ---------------------------------

all_comp <- bind_rows(
  interaction = interaction,
  sex = sex,
  species = species,
  .id = "id"
)

# write the table to a csv file
write_csv(all_comp, file = "data/Tukey_results.csv")


# Plot the results --------------------------------------------------------

# Option 1: plot with base plot (easy but not so nice)
plot(TukeyHSD(aov(lm3)), las = 1, col = "brown")

# Option2: plot with ggplot using the all_comp table
# simplest form of the plot
ggplot(all_comp, aes(y = comparison)) +
  geom_point(aes(x = diff)) +
  geom_errorbar(aes(xmin = lwr, xmax = upr)) +
  facet_grid(id ~ ., scales = "free_y", space = "free_y")

# Make the plot look nicer
ggplot(all_comp, aes(y = comparison)) +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
  geom_errorbar(aes(xmin = lwr, xmax = upr), width = 0.2) +
  geom_point(aes(x = diff), color = "cyan4", size = 3) +
  facet_grid(id ~ ., scales = "free_y", space = "free_y") +
  labs(x = "Difference in body mass (g)", y = "Compared groups") +
  theme_bw()
