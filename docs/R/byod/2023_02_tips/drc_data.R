library(drc)
ryegrass
drc::acidiq
drc::auxins



drc::terbuthylazin %>%
  ggplot(aes(x=dose, y=rgr)) +geom_point()

ToothGrowth %>%
  ggplot(aes(x=dose, y=len)) +
  geom_point()

