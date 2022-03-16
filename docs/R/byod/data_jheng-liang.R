# Step 1: Create some dummy data

dummydata <- tibble(
  time = seq(0, 120, length.out = 100),
)

# describing curves like that
# https://math.stackexchange.com/questions/3749993/an-equation-for-a-graph-which-resembles-a-hump-of-a-camel-pulse-in-a-string
dummydata <- dummydata %>%
  mutate(
    data1 = 0.02/(1+0.01*(time-60)^2),
    data2 = 0.01/(1+0.03*(time-62)^2)
  ) %>% pivot_longer(
    data1:data2
  )

ggplot(dummydata, aes(x=time, y = value, color = name))+
  geom_line()

# find offset and scale values:
# - make steeper/less steep
# - shift left
# - make peek higher


