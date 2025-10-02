system("quarto render")

# render slides as pdf
slides_html <- list.files(
  here::here("docs/sessions/slides/"),
  pattern = "*.html",
  full.names = TRUE
)

lapply(slides_html, function(x) {
  pagedown::chrome_print(x, format = "pdf")
  print(paste0("Printed ", x))
})

# commit and push
git2r::add(path = here::here("docs"))
git2r::commit(message = "re-render site")
system("git push")
