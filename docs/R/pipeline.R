# Render slides -----------------------------------------------------------

source("./R/render_slides.R")

# Render site -------------------------------------------------------------

#rmarkdown::render_site(encoding = "UTF-8")

# Commit ------------------------------------------------------------------

git2r::add(path = here::here("slides"))
git2r::commit(message = "re-render slides")
git2r::push(credentials = git2r::cred_token())

