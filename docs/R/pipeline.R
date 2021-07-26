# Render slides -----------------------------------------------------------

source("./R/render_slides.R")

# Render site -------------------------------------------------------------

rmarkdown::render_site(encoding = "UTF-8")

git2r::add(path = here::here("docs"))

# Commit ------------------------------------------------------------------


