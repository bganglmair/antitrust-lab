# Tacit collusion: model functions
# Notation (deck 07): pi^c collusive profit, pi^d deviation profit, pi^p profit after a deviation,
# delta discount factor, delta-bar critical discount factor, Pi^m monopoly profit, phi probability.

# ---- Tab 1: incentive constraint ----------------------------------------------
ic_ok_inputs <- function(pc, pd, pp) all(is.finite(c(pc, pd, pp))) && pp < pc && pc < pd
ic_dbar <- function(pc, pd, pp) (pd - pc) / (pd - pp)
ic_loss <- function(d, pc, pp) d / (1 - d) * (pc - pp)   # discounted future loss from cheating
ic_sustainable <- function(d, dbar) d >= dbar - 1e-9

# ---- Tab 2: what makes collusion easier? --------------------------------------
# n sellers, identical products, price competition. A price cut takes the whole market until rivals
# answer (response time T in years), then zero profit. With profit flows and annual interest r,
# collusion can be sustained if delta^T >= 1 - 1/n, with delta = 1/(1 + r).
fe_times <- c("1 day" = 1 / 365, "1 week" = 7 / 365, "1 month" = 1 / 12, "3 months" = 0.25,
              "6 months" = 0.5, "1 year" = 1, "2 years" = 2, "3 years" = 3)
fe_eff <- function(r, T) (1 + r)^(-T)
fe_threshold <- function(n) 1 - 1 / n
fe_max_n <- function(eff) if (eff < 0.5 - 1e-12) 0 else floor(1 / (1 - eff) + 1e-9)

# ---- Tab 3: multi-market contact ----------------------------------------------
mm_dbar <- function(pc, pd, pp) (pd - pc) / (pd - pp)
mm_pooled <- function(pc, pd, pp) {
  gain <- pmax(0, pd - pc); stake <- pd - pp
  if (length(pc) == 0 || sum(stake) <= 0) return(NA_real_)
  sum(gain) / sum(stake)
}
# Largest set of markets in which collusion can be sustained at delta (most markets; ties: lowest threshold)
mm_best_set <- function(pc, pd, pp, d) {
  k <- length(pc); best <- integer(0); best_v <- NA_real_
  if (k == 0) return(list(set = best, value = best_v))
  for (m in k:1) {
    sets <- utils::combn(k, m, simplify = FALSE)
    vals <- sapply(sets, function(s) mm_pooled(pc[s], pd[s], pp[s]))
    okk <- which(vals <= d + 1e-9)
    if (length(okk)) { j <- okk[which.min(vals[okk])]; return(list(set = sets[[j]], value = vals[j])) }
  }
  list(set = best, value = best_v)
}

# ---- Tab 4: price wars without cheating ---------------------------------------
# Tirole (1988, section 6.7.1.1): two sellers, demand collapses with probability phi; after a period
# without sales, both compete for T periods. Collusion can be sustained if
# delta^(T + 1) (1 - 2 phi) <= 2 (1 - phi) delta - 1.
pw_ok <- function(phi, d, T) d^(T + 1) * (1 - 2 * phi) <= 2 * (1 - phi) * d - 1 + 1e-12
pw_min_T <- function(phi, d, Tmax = 10) {
  if (2 * (1 - phi) * d - 1 <= 0) return(list(T = NA_integer_, state = "never"))
  for (T in 1:Tmax) if (pw_ok(phi, d, T)) return(list(T = T, state = "ok"))
  list(T = NA_integer_, state = "long")
}
pw_share_war <- function(phi, T) phi * T / (1 + phi * T)
# Average profit relative to collusion without demand risk
pw_rel_profit <- function(phi, T) (1 - phi) / (1 + phi * T)  # long-run average profit relative to stable demand
pw_simulate <- function(phi, T, periods = 40, seed = 1) {
  set.seed(seed)
  collapse <- stats::runif(periods) < phi
  war <- logical(periods); left <- 0
  for (t in seq_len(periods)) {
    if (left > 0) { war[t] <- TRUE; left <- left - 1 }
    else if (collapse[t] && !is.na(T)) left <- T
  }
  data.frame(t = seq_len(periods), collapse = collapse, war = war)
}

# Half-up rounding for display
num <- function(x, digits = 2, big = ",") {
  v <- sign(x) * floor(abs(x) * 10^digits + 0.5 + 1e-9) / 10^digits + 0
  formatC(v, format = "f", digits = digits, big.mark = big)
}
# Up to two decimals, trailing zeros dropped (10, 10.5, 10.25)
nz <- function(x) sub("\\.?0+$", "", num(x, 2))
