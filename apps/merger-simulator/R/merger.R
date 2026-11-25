# Logit merger simulation with single-product firms and an outside option.
# All shares are fractions inside the code and percent on screen.

# Calibrate from inside shares (any positive numbers, scaled to sum 1), the share
# of customers lost if all prices rise (s0), prices, and one observed margin m_k.
ms_calib <- function(size, s0, p, k, mk) {
  inside <- size / sum(size)
  s <- (1 - s0) * inside
  theta <- 1 / (mk * p[k] * (1 - s[k]))
  c <- p - 1 / (theta * (1 - s))
  delta <- log(s / s0) + theta * p
  list(inside = inside, s = s, s0 = s0, p = p, theta = theta, c = c, delta = delta, m = (p - c) / p)
}

# Highest observed margin that keeps every implied cost above zero
ms_max_margin <- function(size, s0, p, k) {
  s <- (1 - s0) * size / sum(size)
  min(p * (1 - s) / (p[k] * (1 - s[k])))
}

ms_shares <- function(p, theta, delta, on = rep(TRUE, length(p))) {
  e <- ifelse(on, exp(delta - theta * p), 0)
  e / (1 + sum(e))
}

# Equilibrium prices for a given ownership vector (owner[j] = firm that sets p_j).
# Products with on = FALSE are not sold. Damped fixed-point iteration.
ms_equil <- function(theta, c, delta, owner, on = rep(TRUE, length(c)), p0 = c + 1 / theta,
                     damp = 0.5, tol = 1e-10, maxit = 10000) {
  r <- ms_equil1(theta, c, delta, owner, on, p0, damp, tol, maxit)
  if (!r$converged) r <- ms_equil1(theta, c, delta, owner, on, p0, 0.1, tol, 100000)
  r
}
ms_equil1 <- function(theta, c, delta, owner, on, p0, damp, tol, maxit) {
  p <- p0
  for (it in seq_len(maxit)) {
    s <- ms_shares(p, theta, delta, on)
    S <- sapply(seq_along(p), function(j) sum(s[owner == owner[j] & on]))
    target <- c + 1 / (theta * (1 - S))
    pn <- ifelse(on, (1 - damp) * p + damp * target, p)
    if (max(abs(pn - p)) < tol) return(list(p = pn, converged = TRUE))
    p <- pn
  }
  list(p = p, converged = FALSE)
}

# Consumer surplus per customer, in money (logit inclusive value)
ms_cs <- function(p, theta, delta, on = rep(TRUE, length(p))) log(1 + sum(ifelse(on, exp(delta - theta * p), 0))) / theta

# Cost saving (share of marginal cost) each party needs for prices to stay unchanged
ms_needed_cut <- function(k, A, B) {
  S <- k$s[A] + k$s[B]
  markup <- 1 / (k$theta * (1 - S))
  cut <- 1 - (k$p[c(A, B)] - markup) / k$c[c(A, B)]
  setNames(cut, c("A", "B"))
}

ms_diversion <- function(s, j, l) s[l] / (1 - s[j])
ms_guppi <- function(k, A, B) c(A = ms_diversion(k$s, A, B) * k$m[B] * k$p[B] / k$p[A],
                                B = ms_diversion(k$s, B, A) * k$m[A] * k$p[A] / k$p[B])
ms_hhi <- function(inside, A, B) c(pre = sum((100 * inside)^2), delta = 2 * (100 * inside[A]) * (100 * inside[B]),
                                   post = sum((100 * inside)^2) + 2 * (100 * inside[A]) * (100 * inside[B]))
# Percent fall in sales of all listed firms if all their prices rise by 1 percent
ms_market_elast <- function(k) k$theta * sum(k$s * k$p) / sum(k$s) * k$s0

# Simulate the merger of A and B, with proportional cost saving E for both parties
ms_merge <- function(k, A, B, E = 0) {
  n <- length(k$p); own <- seq_len(n); own[B] <- A
  c2 <- k$c; c2[c(A, B)] <- c2[c(A, B)] * (1 - E)
  eq <- ms_equil(k$theta, c2, k$delta, own, p0 = k$p)
  s2 <- ms_shares(eq$p, k$theta, k$delta)
  list(p = eq$p, s = s2, c = c2, converged = eq$converged,
       profit = (eq$p - c2) * s2, cs = ms_cs(eq$p, k$theta, k$delta))
}

# Divestiture: the product of party B goes to a new owner whose cost is higher by x (share)
ms_divest <- function(k, A, B, sold, x = 0, E = 0) {
  n <- length(k$p); own <- seq_len(n)  # all independent again
  c2 <- k$c; keep <- setdiff(c(A, B), sold); c2[keep] <- c2[keep] * (1 - E)
  c2[sold] <- k$c[sold] * (1 + x)
  eq <- ms_equil(k$theta, c2, k$delta, own, p0 = k$p)
  s2 <- ms_shares(eq$p, k$theta, k$delta)
  list(p = eq$p, s = s2, c = c2, converged = eq$converged, profit = (eq$p - c2) * s2, cs = ms_cs(eq$p, k$theta, k$delta))
}

# Exit: the failing firm leaves the market and the others reprice
ms_exit <- function(k, gone) {
  n <- length(k$p); on <- seq_len(n) != gone
  eq <- ms_equil(k$theta, k$c, k$delta, seq_len(n), on = on, p0 = k$p)
  s2 <- ms_shares(eq$p, k$theta, k$delta, on)
  list(p = ifelse(on, eq$p, NA), s = s2, c = k$c, converged = eq$converged,
       profit = ifelse(on, (eq$p - k$c) * s2, 0), cs = ms_cs(eq$p, k$theta, k$delta, on))
}

# Half-up formatting
num <- function(x, digits = 2, big = ",") {
  v <- sign(x) * floor(abs(x) * 10^digits + 0.5 + 1e-9) / 10^digits + 0
  formatC(v, format = "f", digits = digits, big.mark = big)
}
nz <- function(x) sub("\\.?0+$", "", num(x, 2))

# Common cost saving E (share of marginal cost) at which each party's price is back at its old level
ms_cross <- function(k, A, B) {
  f <- function(E, j) { r <- ms_merge(k, A, B, E); r$p[j] - k$p[j] }
  sapply(c(A, B), function(j) if (f(0.95, j) > 0) NA_real_ else uniroot(f, c(0, 0.95), j = j, tol = 1e-8)$root)
}
