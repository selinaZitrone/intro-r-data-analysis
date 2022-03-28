# extract palues from tests

treeheights <- readr::read_csv("https://raw.githubusercontent.com/selinaZitrone/intro-r-data-analysis/master/data/treeheight.csv")

forest1 <- filter(treeheights, region == "forest1")$height
forest2 <- filter(treeheights, region == "forest2")$height
forest3 <- filter(treeheights, region == "forest3")$height


# Extract p-values from Shapiro Wilk test ---------------------------------

sw_test1 <- shapiro.test(forest1)
# have a look at the structure of the sw_test1 object
str(sw_test1)
# extract pvalue
sw_test1$p.value


# Extract p-value from F-Test ---------------------------------------------

F_test1 <- var.test(forest1, forest2)
# look at the structure of the F_test1 object
str(F_test1)
# extact the p-value
F_test1$p.value

# Extract p-value from Wilcoxon test --------------------------------------

w_test1 <- wilcox.test(forest1, forest2)
w_test1$p.value

# t.test ------------------------------------------------------------------

t_test1 <- t.test(forest1, forest2, var.equal = FALSE)
t_test1$p.value


# Function that automatically selects the right test to compare me --------

compare_means <- function(groupA, groupB) {

  # first test for normalit
  norm1 <- shapiro.test(groupA)
  norm2 <- shapiro.test(groupB)

  # If one of the two is not normal -> Immediately perform a Wilcoxon test
  if (norm1$p.value < 0.05 | norm2$p.value < 0.05) {
    result <- wilcox.test(groupA, groupB)
  } else {
    # Test for equal variance
    vari <- var.test(groupA, groupB)
    # if variance is not equal -> Welch-Test
    if (vari$p.value < 0.05) {
      result <- t.test(groupA, groupB, var.equal = FALSE)
    # else use T test
    } else {
      result <- t.test(groupA, groupB, var.equal = TRUE)
    }
  }
  return(result)
}

compare_means(forest1, forest2)
compare_means(forest1, forest3)
compare_means(forest2, forest3)
