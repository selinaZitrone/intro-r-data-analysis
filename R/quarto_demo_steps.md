# Live demo steps

## 1. Create the file

- File -> New File -> Quarto Document
- Enter title "Gapminder Analysis", author name
- Click Create
- Tell students to ignore the YAML header for now — we come back to it later
- Delete the default template text (keep the YAML header)

## 2. Markdown text

- Add a second-level header: `## Introduction`
- Write a sentence using **bold** and *italic*
- Add a bullet list
- Add a link: `[Gapminder website](https://www.gapminder.org/)`
- **Render** to show the formatted output

## 3. Code chunks

- Add an `## Analysis` header
- Insert a code chunk (Ctrl+Alt+I) with the setup code:
  ```r
  library(gapminder)
  library(ggplot2)
  ```
- Insert another code chunk with a ggplot:
  ```r
  ggplot(gapminder_2007, aes(x = gdpPercap, y = lifeExp)) +
    geom_point()
  ```
- Show that you can run the chunk with the green arrow
- **Render** to show code + output in the document

## 4. Chunk options

- Add `#| echo: false` to the plot chunk — render to show the difference
- Go back to the setup chunk and add `#| include: false` — render again
- Mention other options: `eval`, `warning`, `message`

## 5. Inline code

- Add a sentence with inline code:
  ```
  The gapminder data contains `r nrow(gapminder)` observations.
  ```
- **Render** to show the number appears in the text

## 6. YAML header

- Now go back to the YAML header and explain what each field does
- Add execute options:
  ```yaml
  execute:
    warning: false
    message: false
  ```
- Explain that these set defaults for all chunks (can be overwritten per chunk)
- **Render** one final time

---

# After the task: Additional features

## 7. Document options

- Change the YAML `format` from `html` to a nested format with options:
  ```yaml
  format:
    html:
      toc: true
      toc-location: left
      number-sections: true
      code-fold: true
  ```
- **Render** to show table of contents, numbered sections, and folded code

## 8. Figure options & cross-references

- Add options to the plot chunk:
  ```r
  #| label: fig-life-exp
  #| fig-cap: "Life expectancy vs. GDP per capita in 2007"
  #| fig-align: center
  #| out-width: "80%"
  ```
- Also add color by continent to the plot: `geom_point(aes(color = continent))`
- Reference the figure in the text: `As we can see in @fig-life-exp, ...`
- **Render** to show the caption and cross-reference

## 9. Nice tables with `kable()`

- Add a `### Summary by continent` header
- Insert a code chunk with a summary table:
  ```r
  gapminder_2007 |>
    group_by(continent) |>
    summarize(
      n_countries = n(),
      mean_life_exp = round(mean(lifeExp), 1),
      mean_gdp = round(mean(gdpPercap), 0)
    ) |>
    knitr::kable()
  ```
- **Render** to show the formatted table (compare with/without `kable()`)

## 10. Statistical test with `broom`

- Add a `### Statistical test` header
- Insert a code chunk with a t-test:
  ```r
  europe <- gapminder_2007 |>
    filter(continent == "Europe") |>
    pull(lifeExp)

  africa <- gapminder_2007 |>
    filter(continent == "Africa") |>
    pull(lifeExp)

  t.test(europe, africa) |>
    broom::tidy() |>
    knitr::kable()
  ```
- Show the difference: messy test output vs. clean table
- **Render** one final time
