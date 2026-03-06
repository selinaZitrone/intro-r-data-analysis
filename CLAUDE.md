# CLAUDE.md — Project Instructions

## Project Overview

This is a Quarto-based teaching website for a 4-day introductory R workshop ("Introduction to Data Analysis with R"). The site is hosted at https://selinazitrone.github.io/intro-r-data-analysis/ and built from `.qmd` source files. The `docs/` directory is the rendered output and should not be edited directly.

## Build Commands

Render the entire site:
```bash
quarto render
```

Render a single file (faster for development):
```bash
quarto render sessions/07_ggplot.qmd
```

Full build including PDF slides and git push: run `make.R` in R (uses `pagedown::chrome_print` + `git2r`).

**Always ask before running `quarto render`** — rendering can be slow and may overwrite cached outputs.

## Directory Structure

```
index.qmd               # Home page — schedule hub, links to session pages
sessions/               # Session pages (numbered: 01_, 02_, ...)
  byod.qmd              # BYOD session hub page (Day 4)
  slides/               # Revealjs slide decks (.qmd → HTML/PDF)
  tasks/                # Standalone task pages (own YAML headers, link back to session)
  solutions/            # Solution sheets
byod/                   # "Bring Your Own Data" dataset pages
R/
  Tasks/                # Downloadable R scripts for each day (Day1.R, Day2.R, Day3.R)
  byod/
    general_tips/       # Example R scripts linked from the website
    solutions_for_my_datasets/
data/                   # Data files used in sessions
docs/                   # Rendered site output — DO NOT edit directly
_quarto.yml             # Quarto project config (navbar, format settings, render exclusions)
theme.scss              # Custom CSS theme
timer_solution.js       # JS timer — controls solution and download visibility
make.R                  # Full build script (render + PDF + git push)
```

## Navigation Flow

```
index.qmd (schedule hub)
  └── sessions/NN_name.qmd  (slides + task link + further reading)
        └── sessions/tasks/name.qmd  (task text + timer-gated solution link)
              └── sessions/solutions/name.qmd

index.qmd → sessions/byod.qmd  (BYOD hub: slides + dataset links + example scripts)
  └── byod/*.qmd
```

## Content Conventions

### Session pages (`sessions/NN_name.qmd`)
Each session page contains:
- A slide deck embedded via `<iframe>`
- A link to the standalone task page: `Go to the [task page](tasks/name-task.qmd) for the exercises.`
- A `## Further reading` section with relevant links (for sessions that have resources)

Session pages do **not** use `{{< include >}}` — task content lives entirely in the task page.

### Task pages (`sessions/tasks/name-task.qmd`)
Standalone pages with their own YAML header (`title`, `number-sections: false`). Each task page contains:
- A back link to the parent session page
- A callout-note with the timer-gated solution link
- The task content

Task pages must **not** be `{{< include >}}`'d — Quarto prohibits including files with YAML headers.

### Solution pages (`sessions/solutions/name-solution.qmd`)
Linked from task pages after the session timer expires. Not linked from the navbar.

### Slides (`sessions/slides/name.qmd`)
Use revealjs format with `self-contained: true`. The `slides/template.qmd` is a starting point for new slide decks.

### BYOD pages (`byod/name.qmd`)
Dataset-specific exercises. Corresponding R scripts live in `R/byod/`. Linked from `sessions/byod.qmd`.

### File naming
Session files are numbered (e.g., `01_intro-rstudio.qmd`). Slide files use the same base name without the number (e.g., `intro-rstudio.qmd`). Task files add a `-task` suffix (e.g., `intro-rstudio-task.qmd`) and solution files add a `-solution` suffix (e.g., `intro-r-solution.qmd`).

### Internal links
- Use `.qmd` extensions for links in markdown (Quarto rewrites to `.html` on render)
- Use `.html` extensions only inside `{=html}` raw blocks
- Non-QMD files (`.R`, `.pdf`) use their actual extension

## R Code Style

- Prefer **tidyverse** packages over base R equivalents
- Use the **native pipe `|>`** (not `%>%`)
- Follow tidyverse style: snake_case names, spaces around operators
- Use **Quarto-style chunk options** (`#|` comments inside the chunk), not knitr/Rmd-style options in the chunk header (e.g., `{r echo=TRUE}`)
- `echo: true` is the default — do not add it explicitly unless overriding a different setting

## Solution Timer Workflow

Solutions on task pages and R script downloads on the home page are revealed automatically via `timer_solution.js`.

### How it works

**`timer_solution.js`** (project root, copied to `docs/` as a Quarto resource) contains:
- `session_end_times`: an array of datetimes, one per course day, with Berlin timezone offset (`+01:00` CET / `+02:00` CEST)
- A guard clause (`if (typeof course_day === 'undefined') return;`) so pages without `course_day` don't error

**Task pages** set `const course_day = N` inline and load the script. When the current time passes `session_end_times[course_day - 1]`, the `.r_class_solution` element becomes visible (hidden by default via `theme.scss`).

**`index.qmd`** loads the script and has its own inline script that iterates over all days, revealing `.r_day1_content`, `.r_day2_content`, `.r_day3_content` elements (also hidden by default via `theme.scss`) as each day passes.

### For a new course run

1. Edit **`timer_solution.js`** (project root) with the new session end datetimes
2. Copy it to **`docs/timer_solution.js`** — no re-render needed for date changes

### Script path conventions

- From `sessions/tasks/*.qmd`: `../../timer_solution.js`
- From `index.qmd`: `timer_solution.js`

### To add a new session with a solution

Follow the pattern in any existing task page (e.g. `sessions/tasks/ggplot-task.qmd`).

## Important Rules

- **Do not edit `docs/`** — it is auto-generated by `quarto render` (exception: `docs/timer_solution.js` for date-only updates between course runs)
- **Do not touch `_cache` directories** — these are Quarto's render cache
- **Do not commit or push** without explicit instruction
- `.Rmd` files are excluded from rendering (project only uses `.qmd`)
- Data files in `data/` are gitignored — do not assume they can be committed
