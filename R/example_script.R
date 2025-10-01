library(tidyverse)

head(penguins)

g <- ggplot(penguins, aes(x = flipper_len, y = body_mass, color = species)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values = c("darkorange", "purple", "cyan4")) +
  theme_bw()
g
