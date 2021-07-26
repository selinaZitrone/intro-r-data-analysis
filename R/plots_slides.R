cols <- c("#4C7488", "#D78974", "#D7BC74")

library(tidyverse)
chicken <- chickwts %>%
  mutate(feed = as.factor(feed)) %>%
  ggplot(aes(x = feed, y = weight, color = feed)) +
  geom_jitter(size = 3, alpha = 0.25, width = 0.2) +
  ggsci::scale_color_uchicago() +
  labs(y = "weight [g]", x = "Diet") +
  theme(legend.position = "none") +
  stat_summary(fun = mean, geom = "point", size = 5)

ggsave("./slides/img/day3/chicken.png", width = 16, height = 9, units = "cm")



# binomial distribution ---------------------------------------------------
binom_list <- list()
x <- 0:20
N <- 20
p_vec <- c(0.1, 0.5, 0.95)
for (i in 1:length(p_vec)) {
  binom_list[[i]] <- data.frame(
    x = x,
    Density = dbinom(x, prob = p_vec[i], size = N),
    p = p_vec[i]
  )
}

binom_dat <- bind_rows(binom_list)
binom_dat$p <- as.factor(binom_dat$p)

binom_plot <- ggplot(binom_dat, aes(x = x, y = Density, color = p)) +
  geom_point() +
  geom_line(linetype = 2) +
  scale_color_manual(values = cols) +
  ggtitle("N = 20") +
  labs(x = "k", y = "P(x = k)")

ggsave("./slides/img/day3/binom_plot.png", binom_plot, width = 16, height = 9, unit = "cm")

# poisson -----------------------------------------------------------------

pois_list <- list()
x <- 0:20
lambda_vec <- c(1, 4, 10)
for (i in 1:length(lambda_vec)) {
  pois_list[[i]] <- data.frame(
    x = x,
    Density = dpois(x, lambda = lambda_vec[i]),
    lambda = lambda_vec[i]
  )
}
pois_dat <- bind_rows(pois_list)
pois_dat$lambda <- as.factor(pois_dat$lambda)

poiss_plot <- ggplot(pois_dat, aes(x = x, y = Density, color = lambda)) +
  geom_point() +
  geom_line(linetype = 2) +
  scale_color_manual(values = cols, name = expression(lambda)) +
  labs(x = "k", y = "P(x = k)")

ggsave("./slides/img/day3/poiss_plot.png", poiss_plot, width = 16, height = 9, unit = "cm")
