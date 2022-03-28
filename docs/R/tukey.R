# https://www.r-bloggers.com/2018/09/tukeys-test-for-post-hoc-analysis/
# https://www.statology.org/anova-post-hoc-tests/

chickwts
ggplot(chickwts, aes(x = feed, y = weight)) +
  geom_boxplot()

# lm and aov do the same thing here

lm_chick <- lm(weight ~ feed, chickwts)
drop1(lm_chick, test = "F")

# same as
aov_chick <- aov(weight ~ feed, chickwts)
summary(aov_chick)

# Post-hoc TukeyHSD test for differences between all groups

TukeyHSD(aov_chick)
TukeyHSD(aov(lm_chick))

plot(TukeyHSD(aov(lm_chick)))
