slides_rmd <- list.files("./slides",pattern = "*.Rmd")

slides_rmd <- c("1_Organization.Rmd", "3_Day1.Rmd", "4_Day2.Rmd", "5_General_tips.Rmd")

# Render all slides
lapply(slides_rmd, function(x) {rmarkdown::render(here::here("slides",x))})

# Print slides to pdf

# Print slides to pdf -----------------------------------------------------
slides_html <- gsub("Rmd", "html", slides_rmd)
#slides_html <- list.files("./slides",pattern = "[[:digit:]]{2}_[[:digit:]]{2}_[[:graph:]]*.html")

lapply(slides_html, function(x) {pagedown::chrome_print(here::here("slides",x),
                                                   format = "pdf")})


# Zip pdf slides ----------------------------------------------------------

#utils::zip(zipfile = 'testZip', files = 'testDir/test.csv')
