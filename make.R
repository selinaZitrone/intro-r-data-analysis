quarto::quarto_render()

# render slides as pdf
slides_html <- list.files(here::here("docs/slides"),
                          pattern = "!00_", full.names = TRUE
)

lapply(slides_html, function(x) {
  pagedown::chrome_print(x,
                         format = "pdf"
  )
})

# commit and push
git2r::add(path = here::here("docs"))
git2r::add(path = here::here("_freeze"))
git2r::commit(message = "re-render site")
git2r::push(credentials = git2r::cred_token())

