library(xaringanthemer)
white_color <- "#FFFFFF"
black_color <- "#272822"


# Color palette -------------------------------------------------------------------------

# colorA <- "#CAE4DB" # inverse Text
# colorB <- "#DCAE1D" # links?
# colorC <- "#00303F" # inverse BG and header Text
# colorD <- "#7A9D96" # header BG color
#
# colorE <- "#C7818B"

# colorA <- "#F0FBE0"
# colorB <- "#0050FF"
# colorB_light <- "#B5FCFF"
# colorC <- "#252035"
# colorD <- "#A3B4C8"


colorA <- "#FFFFFF"
colorB <- "#0050FF"
colorB_light <- "#B5FCFF"
colorC <- "#FFFFFF"
colorD <- "#284185"

# Base colors  ----------------------------------------------------------
content_bg <- "#F8FAFC"
inverse_bg <- colorD

link_inverse <- colorB_light
link_content <- colorB

# Content slides ----------------------------------------------------------

# text content
text_content <- black_color
# links, bold, inline code
inline_code_content <- black_color
# title
title_content <- white_color
# header
header_bg_color <- colorD
header_bg_text_color <- white_color

# Inverse slides ----------------------------------------------------------
# text inverse
text_inverse <- colorA
# links, bold, inline code
bold_content <- black_color
bold_inverse <- white_color
inline_code_inverse <- white_color
# title
title_inverse <- white_color


style_xaringan(
  text_color = text_content,
  header_color = title_content,
  background_color = content_bg,
  link_color = link_content,
  text_bold_color = bold_content,
  padding = "16px 64px 16px 64px",
  code_highlight_color = "rgba(255,255,0,0.5)",
  #code_inline_color = inline_code_content,
  #code_inline_background_color = NULL,
  code_inline_font_size = "1.1em",
  inverse_background_color = inverse_bg,
  inverse_text_color = text_inverse,
  inverse_text_shadow = FALSE,
  inverse_header_color = title_inverse,
  inverse_link_color = link_inverse,
  title_slide_text_color = text_inverse,
  title_slide_background_color = inverse_bg,
  title_slide_background_position = "bottom right",
  footnote_color = NULL,
  footnote_font_size = "0.9em",
  footnote_position_bottom = "60px",
  #left_column_subtle_color = apply_alpha(base_color, 0.6),
  #left_column_selected_color = base_color,
  #blockquote_left_border_color = apply_alpha(base_color, 0.5),
  table_border_color = "#666",
  table_row_border_color = "#ddd",
  #table_row_even_background_color = lighten_color(base_color, 0.8),
  base_font_size = "20px",
  text_font_size = "18pt",
  header_h1_font_size = "30pt",
  header_h2_font_size = "24pt",
  header_h3_font_size = "20pt",
  header_background_auto = TRUE,
  header_background_color = header_bg_color,
  header_background_text_color = header_bg_text_color,
  #header_background_text_color = background_color,
  header_background_padding = NULL,
  header_background_content_padding_top = "7rem",
  header_background_ignore_classes = c("normal", "inverse", "title", "middle",
                                       "bottom"),
  text_font_google   = google_font("Roboto", "300", "300i"),
  text_font_family = xaringanthemer_font_default("text_font_family"),
  text_font_weight = xaringanthemer_font_default("text_font_weight"),
  text_bold_font_weight = "bold",
  text_font_url = xaringanthemer_font_default("text_font_url"),
  text_font_family_fallback = xaringanthemer_font_default("text_font_family_fallback"),
  text_font_base = "sans-serif",
  header_font_google = google_font("Roboto"),
  header_font_family = xaringanthemer_font_default("header_font_family"),
  header_font_weight = xaringanthemer_font_default("header_font_weight"),
  header_font_family_fallback = "Georgia, serif",
  header_font_url = xaringanthemer_font_default("header_font_url"),
  #code_font_google   = google_font("JetBrains Mono"),
  code_font_family = "Courier New",
  # code_font_family = xaringanthemer_font_default("code_font_family"),
  code_font_size = "16pt",
  code_font_url = xaringanthemer_font_default("code_font_url"),
  code_font_family_fallback = "Courier New",
  # code_font_family_fallback = xaringanthemer_font_default("code_font_family_fallback"),
  link_decoration = "none",
  colors = NULL,
  extra_css = NULL,
  extra_fonts = NULL,
  outfile = here::here("slides/css/xaringan-themer.css")
)
