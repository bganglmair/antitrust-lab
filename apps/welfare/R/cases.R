# Presets for the welfare sandbox. All numbers are illustrative.

src_link <- function(text, href) htmltools::a(text, href = href, target = "_blank")

welfare_cases <- list(
  constant = list(
    label = "Constant marginal cost",
    a = 7, b = 1, c = 1, d = 0,
    amin = 2, amax = 12, astep = 0.5, cmin = 0, cmax = 6, cstep = 0.5,
    note = "Demand p = 7 − Q and a constant marginal cost of 1. Illustrative numbers."),
  rising = list(
    label = "Rising marginal cost",
    a = 80, b = 1.5, c = 20, d = 2,
    amin = 40, amax = 120, astep = 5, cmin = 0, cmax = 60, cstep = 5,
    note = "Demand p = 80 − 1.5 Q; marginal cost starts at 20 and rises by 2 with every unit. Illustrative numbers.")
)
