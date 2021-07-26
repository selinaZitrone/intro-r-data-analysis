deps <- renv::dependencies(errors = "ignored")
# unique dependencies
deps_unique <- unique(deps$Package)
# remove packages that are covered by tidyverse
tidyverse_pkg <- c("ggplot2", "tibble", "readr", "tidyr")
deps_to_install <- deps_unique[!(deps_unique %in% c(tidyverse_pkg, "renv"))]
install.packages(deps_to_install)
