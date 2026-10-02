# Build the Antitrust Lab site into docs/ (served by GitHub Pages from main/docs)
# Run from the repository root: source("build.R")

stopifnot(file.exists("_quarto.yml"))
apps <- list.dirs("apps", recursive = FALSE, full.names = FALSE)

# 1. Copy shared code into each app
for (a in apps) {
  dir.create(file.path("apps", a, "R"), showWarnings = FALSE)
  file.copy(list.files("shared", full.names = TRUE), file.path("apps", a, "R"), overwrite = TRUE)
}

# 2. Render the Quarto pages
quarto_bin <- Sys.which("quarto")
if (quarto_bin == "") quarto_bin <- "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/quarto"
stopifnot(system2(quarto_bin, "render") == 0)

# 3. Export each app with shinylive into docs/<app>/
for (a in apps) shinylive::export(file.path("apps", a), "docs", subdir = a, quiet = TRUE)

# 4. Visit counter: add the script from counter.html to each tool's page
counter <- paste(readLines("counter.html"), collapse = "\n")
for (a in apps) {
  f <- file.path("docs", a, "index.html")
  h <- paste(readLines(f, warn = FALSE), collapse = "\n")
  stopifnot(grepl("</head>", h, fixed = TRUE))
  # Browser-tab title: shinylive's wrapper page says "Shiny App"; use the title the app sets (window_title)
  wt <- regmatches(paste(readLines(file.path("apps", a, "app.R"), warn = FALSE), collapse = "\n"),
                   regexpr('window_title = "[^"]*"', paste(readLines(file.path("apps", a, "app.R"), warn = FALSE), collapse = "\n")))
  wt <- eval(parse(text = sub("window_title = ", "", wt)))
  h <- sub("<title>[^<]*</title>", paste0("<title>", wt, "</title>"), h)
  icon <- '<link rel="icon" type="image/svg+xml" href="../images/favicon.svg">'
  if (!grepl("goatcounter", h, fixed = TRUE)) writeLines(sub("</head>", paste0(icon, "\n", counter, "\n</head>"), h, fixed = TRUE), f)
}

# 5. GitHub Pages: no Jekyll processing
file.create(file.path("docs", ".nojekyll"))
message("Built: ", paste(apps, collapse = ", "))
