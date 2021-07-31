slides_rmd <- list.files("./slides",pattern = "[[:digit:]]{2}_[[:digit:]]{2}_[[:graph:]]*.Rmd")

# Render all slides
lapply(slides_rmd, function(x) {rmarkdown::render(here::here("slides",x))})

# Print slides to pdf

# Print slides to pdf -----------------------------------------------------
slides_html <- list.files("./slides",pattern = "[[:digit:]]{2}_[[:digit:]]{2}_[[:graph:]]*.html")

lapply(slides_html, function(x) {pagedown::chrome_print(here::here("slides",x),
                                                   format = "pdf")})

# # This works only with Edge and Chrome
# render_slides <- function(slide = slides){
#   rmarkdown::render(
#     here::here("slides",slide),
#     output_dir = here::here("slides", "rendered_slides")
#   )
# }
#
# # This works with all browsers
# render_slides <- function(slide = slides){
#   rmarkdown::render(
#     here::here("slides",slide)
#   )
# }
