slides <- list.files("./slides",pattern = "[[:digit:]]{2}_[[:digit:]]{2}_[[:graph:]]*.Rmd")

# This works only with Edge and Chrome
render_slides <- function(slide = slides){
  rmarkdown::render(
    here::here("slides",slide),
    output_dir = here::here("slides", "rendered_slides")
  )
}

# This works with all browsers
render_slides <- function(slide = slides){
  rmarkdown::render(
    here::here("slides",slide)
  )
}
# Render all slides
lapply(slides, function(x) {rmarkdown::render(here::here("slides",x))})
