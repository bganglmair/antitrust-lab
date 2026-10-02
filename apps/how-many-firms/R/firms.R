# Model functions for "How many firms?".
# Cournot with n equal firms, demand p = a - Q, marginal cost c, fixed cost F per firm.
# Notation follows competition-tools/NOTATION.md.

cournot_n <- function(n, a, c, F) {
  q <- (a - c) / (n + 1)
  p <- (a + n * c) / (n + 1)
  profit <- q^2 - F
  cs <- n^2 * (a - c)^2 / (2 * (n + 1)^2)
  ps <- n * profit
  data.frame(n = n, q = q, p = p, profit = profit, cs = cs, ps = ps, w = cs + ps,
             lerner = (p - c) / p, hhi = 10000 / n)
}

# Largest number of firms that still break even (free entry); Inf without fixed costs
n_free_entry <- function(a, c, F) {
  if (a <= c) return(0)
  if (F <= 0) return(Inf)
  max(0, floor((a - c) / sqrt(F) - 1 + 1e-9))
}

# Whole number of firms with the highest welfare (0 = no firm); Inf without fixed costs
n_welfare <- function(a, c, F, nmax = 2000) {
  if (a <= c) return(0)
  if (F <= 0) return(Inf)
  n <- 1:nmax
  w <- cournot_n(n, a, c, F)$w
  if (max(w) <= 1e-9) 0 else as.numeric(min(n[w >= max(w) - 1e-9]))   # smallest n if tied
}

# cap is kept for the callers; the exact number is shown even beyond the chart range
fmt_n <- function(n, cap = 20) {
  if (is.infinite(n)) "As many as possible" else as.character(n)
}

# Price competition with differentiated products (Shubik-Levitan demand).
# Each of n firms sells one variety: q_i = (1/n) [a - p_i - g (p_i - average price)].
# g >= 0 measures how close substitutes the varieties are (0: independent; large: nearly identical).
# At equal prices total demand is Q = a - p whatever n is, so variety has no value of its own.
bertrand_n <- function(n, a, c, F, g) {
  k <- 1 + g * (n - 1) / n
  p <- (a + c * k) / (1 + k)
  Q <- a - p
  profit <- (p - c) * Q / n - F
  cs <- Q^2 / 2
  ps <- n * profit
  data.frame(n = n, q = Q / n, p = p, profit = profit, cs = cs, ps = ps, w = cs + ps,
             lerner = (p - c) / p, hhi = 10000 / n)
}

n_free_entry_b <- function(a, c, F, g, nmax = 2000) {
  if (a <= c) return(0)
  if (F <= 0) return(Inf)
  ok <- which(bertrand_n(1:nmax, a, c, F, g)$profit >= -1e-9)
  if (length(ok) == 0) 0 else as.numeric(max(ok))
}

n_welfare_b <- function(a, c, F, g, nmax = 2000) {
  if (a <= c) return(0)
  if (F <= 0) return(if (g > 0) Inf else 1)
  w <- bertrand_n(1:nmax, a, c, F, g)$w
  if (max(w) <= 1e-9) 0 else as.numeric(min(which(w >= max(w) - 1e-9)))   # smallest n if tied
}
