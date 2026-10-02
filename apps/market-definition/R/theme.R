# Shared look for all Antitrust Lab tools (copied into each app's R/ folder by build.R)
# Palette chosen with options(lab.palette = "<name>") before the app starts; default below.

lab_palettes <- list(
  okabe = c(name = "Okabe-Ito", bg = "#ffffff", ink = "#1f2933", muted = "#6b7280", grid = "#e5e7eb",
            critical = "#0072B2", actual = "#E69F00", pass = "#009E73", fail = "#D55E00",
            primary = "#0072B2", navbar = "#f8f9fa", navbar_theme = "light", pass_fg = "#ffffff", fail_fg = "#ffffff"),
  slate = c(name = "Slate and ochre", bg = "#ffffff", ink = "#1f2933", muted = "#64748b", grid = "#e2e8f0",
            critical = "#1F3A5F", actual = "#C8963E", pass = "#2A9D8F", fail = "#B5473A",
            primary = "#1F3A5F", navbar = "#1F3A5F", navbar_theme = "dark", pass_fg = "#ffffff", fail_fg = "#ffffff"),
  editorial = c(name = "Editorial", bg = "#FFF9F2", ink = "#33302E", muted = "#66605C", grid = "#EBDFD3",
            critical = "#0F5499", actual = "#E8833A", pass = "#0D7680", fail = "#B3262B",
            primary = "#0F5499", navbar = "#F2E5D7", navbar_theme = "light", pass_fg = "#ffffff", fail_fg = "#ffffff"),
  mono = c(name = "Graphite and blue", bg = "#ffffff", ink = "#111827", muted = "#6b7280", grid = "#e5e7eb",
            critical = "#111827", actual = "#9CA3AF", pass = "#2563EB", fail = "#4B5563",
            primary = "#2563EB", navbar = "#ffffff", navbar_theme = "light", pass_fg = "#ffffff", fail_fg = "#ffffff"),
  nordic = c(name = "Nordic", bg = "#ffffff", ink = "#2E3440", muted = "#4C566A", grid = "#E5E9F0",
            critical = "#5E81AC", actual = "#D08770", pass = "#4F7F52", fail = "#BF616A",
            primary = "#5E81AC", navbar = "#2E3440", navbar_theme = "dark", pass_fg = "#ffffff", fail_fg = "#ffffff"),
  ed_ink = c(name = "Editorial: ink and sand", bg = "#FFF9F2", ink = "#33302E", muted = "#66605C", grid = "#EBDFD3",
            critical = "#0F5499", actual = "#E8833A", pass = "#33302E", fail = "#E3D5C3",
            primary = "#0F5499", navbar = "#F2E5D7", navbar_theme = "light", pass_fg = "#ffffff", fail_fg = "#33302E"),
  ed_blue = c(name = "Editorial: blue and sand", bg = "#FFF9F2", ink = "#33302E", muted = "#66605C", grid = "#EBDFD3",
            critical = "#33302E", actual = "#E8833A", pass = "#0F5499", fail = "#E3D5C3",
            primary = "#0F5499", navbar = "#F2E5D7", navbar_theme = "light", pass_fg = "#ffffff", fail_fg = "#33302E"),
  ed_plum = c(name = "Editorial: teal and plum", bg = "#FFF9F2", ink = "#33302E", muted = "#66605C", grid = "#EBDFD3",
            critical = "#0F5499", actual = "#E8833A", pass = "#0D7680", fail = "#6E4C7E",
            primary = "#0F5499", navbar = "#F2E5D7", navbar_theme = "light", pass_fg = "#ffffff", fail_fg = "#ffffff")
)

lab_colors <- lab_palettes[[getOption("lab.palette", "ed_blue")]]

lab_theme <- function() {
  bslib::bs_theme(
    version = 5,
    bg = lab_colors[["bg"]], fg = lab_colors[["ink"]],
    primary = lab_colors[["primary"]],
    base_font = bslib::font_collection("system-ui", "-apple-system", "Segoe UI", "Roboto", "Helvetica Neue", "Arial", "sans-serif"),
    "font-size-base" = "1.05rem"
  ) |>
    # On wide screens the charts would stretch across the whole window; keep the content column readable
    bslib::bs_add_rules(".bslib-sidebar-layout > .main { max-width: 1150px; } .tab-content > .tab-pane { max-width: 1500px; }")
}

lab_navbar <- function() {
  bslib::navbar_options(bg = lab_colors[["navbar"]], theme = lab_colors[["navbar_theme"]])
}

lab_gg <- function(base_size = 15) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = lab_colors[["bg"]], color = NA),
      panel.background = ggplot2::element_rect(fill = lab_colors[["bg"]], color = NA),
      text = ggplot2::element_text(color = lab_colors[["ink"]]),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(color = lab_colors[["grid"]]),
      plot.title = ggplot2::element_text(face = "bold"),
      legend.position = "bottom"
    )
}

# Feedback form (Google Forms; answers are private)
lab_feedback_url <- "https://docs.google.com/forms/d/e/1FAIpQLSdvIhiTo6BXO_Byt6Qrma8wXMDMbbSGmEHWPUL70CylOBTwGg/viewform"

# Link that opens the form with the tool already selected in the first question.
# `tool` must match an option of that dropdown exactly.
lab_feedback_link <- function(tool = "General / Landing page") {
  paste0(lab_feedback_url, "?usp=pp_url&entry.439990338=", utils::URLencode(tool, reserved = TRUE))
}

lab_header <- function(question, topic, tool = "General / Landing page") {
  htmltools::div(
    class = "lab-header",
    style = "padding: .6rem 0 .2rem 0;",
    htmltools::h4(question, style = "margin-bottom:.2rem;"),
    htmltools::div(style = paste0("color:", lab_colors[["muted"]], ";"),
      topic, " · ", htmltools::a("Antitrust Lab home", href = "../../", target = "_top",
        # The published tool runs inside a frame; go one level up from the page that holds it.
        onclick = "try { window.top.location.href = new URL('../', window.top.location.href).href; return false; } catch (e) {}"),
      " · ", htmltools::a("Feedback", href = lab_feedback_link(tool), target = "_blank", rel = "noopener"))
  )
}

# Percent with half-up rounding (formatC alone rounds 1.75 to 1.7); + 0 turns -0 into 0
pct <- function(x, digits = 1) {
  v <- sign(x) * floor(abs(100 * x) * 10^digits + 0.5 + 1e-9) / 10^digits + 0
  paste0(formatC(v, format = "f", digits = digits), "%")
}

# References in American Economic Review style. `...` is the reference text (use htmltools::em() for
# journal and book titles); `url` adds a link shown as the DOI or as the host name.
lab_ref <- function(..., url = NULL) {
  link <- if (is.null(url)) NULL else htmltools::tagList(" ",
    htmltools::a(if (grepl("doi.org/", url, fixed = TRUE)) url else sub("^https?://(www\\.)?([^/]+).*$", "\\2", url),
                 href = url, target = "_blank", rel = "noopener"))
  # collapse the line breaks htmltools puts between tags, so that no space appears before a comma
  htmltools::tags$li(htmltools::HTML(gsub("\n", "", as.character(htmltools::tagList(..., link)), fixed = TRUE)))
}
