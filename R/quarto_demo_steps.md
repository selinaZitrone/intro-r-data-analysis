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
