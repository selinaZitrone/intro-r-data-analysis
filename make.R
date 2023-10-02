quarto::quarto_render()

# render slides as pdf
slides_html <- list.files(here::here("docs/sessions/slides"),
                          pattern = "*.html", full.names = TRUE
)

# does not work
slides_html <- slides_html[4]

lapply(slides_html[5], function(x) {
  print(paste0("Printing ", x))
  pagedown::chrome_print(x,
                         format = "pdf"
  )
})

# commit and push
git2r::add(path = here::here("docs"))
git2r::add(path = here::here("_freeze"))
git2r::commit(message = "re-render site")
git2r::push(credentials = git2r::cred_token())

