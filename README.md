# Antitrust Lab

Interactive tools for teaching competition economics: <https://bganglmair.github.io/antitrust-lab/>

Each tool is an R Shiny app in `apps/<tool>/`, compiled with [shinylive](https://posit-dev.github.io/r-shinylive/) so it runs in the browser without a server.

## Build

```r
source("build.R")                     # renders the site and exports all apps into docs/
httpuv::runStaticServer("docs")       # preview locally
```

Commit `docs/` and push; GitHub Pages serves `main/docs`.

## Develop a tool

```r
shiny::runApp("apps/market-definition")
```

Shared look and helpers live in `shared/` and are copied into each app's `R/` folder by `build.R`.

AI assistance (Claude) was used in writing the code.
