### Define custom theme
theme_ks <- bslib::bs_theme(
  version = ,
  bg = "#FfFfFf",
  fg = "#183a5a",
  # dark navy (title, base font and stuff)
  primary = "#183a5a",
  # tab font color and hover block color etc
  secondary = "#353842",
  # (for messages that don't need to stand out)
  success = "#0b5b67",
  # dark green
  info = "#3e83a8",
  # ligher dark blue (text that are informative not critical)
  warning = "#EFb758",
  # mustard-ish yellow
  danger = "#C34129",
  # blood orange
  base_font = font_google("Source Sans Pro"),
  code_font = font_google("Fira Mono"),
  heading_font = font_google("Oswald"),
  font_scale = 0.9
)
