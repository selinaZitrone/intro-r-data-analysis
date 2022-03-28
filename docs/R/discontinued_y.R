# https://stackoverflow.com/questions/7194688/using-ggplot2-can-i-insert-a-break-in-the-axis

# https://groups.google.com/g/ggplot2/c/jSrL_FnS8kc/m/MvzM_2_jiSIJ


library(tidyverse)

# data for a barplot:

df <- tibble(y = c(2,10,2,2,70,4,2,6,2,100),
             x=rep(paste0("x", 1:5),2),
             groups = rep(c("A","B"), 5))

g <- ggplot(data = df, aes(x = x, y = y, fill = groups)) +
  geom_col(position = position_dodge(0.8))

g
# Option 1: Zooming with ggforce ------------------------------------------
# more on zooming: https://ggforce.data-imaginist.com/reference/facet_zoom.html
# install.packages("ggforce")
library(ggforce)

g + facet_zoom(ylim = c(0,10))

# remove the last bar from the zoom
g + facet_zoom(
  ylim = c(0,10),
  zoom.data = ifelse(y <= 10, NA, FALSE))


# Option 2: creating a separate plot and stacking it on top ---------------

# create a plot with limited y axis
g2 <- g +
  coord_cartesian(ylim = c(50,100)) + # limit y axis
  theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank()
  )

g2 / g + coord_cartesian(ylim = c(0,15)) &


# arrange the plots:
library(patchwork)



