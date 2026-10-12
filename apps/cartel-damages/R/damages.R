# Cartel damages: model functions
# Notation: p^cartel cartel price, p^bf but-for price, tau cartel effect, rho pass-on rate,
# n sellers, n_d direct purchasers, dw overcharge on the input.

# ---- Tab 1: before and after ------------------------------------------------
ba_tau <- 0.30
ba_world <- function(preset = "clean") {
  t <- 1:84
  g <- if (preset %in% c("rising", "rising_over")) 0.2 else 0
  cost <- 100 + g * (t - 42.5) + 2 * sin(2 * pi * t / 12)
  cartel <- t >= 25 & t <= 60
  bf <- 0.30 + 0.005 * cost
  p <- bf + ba_tau * cartel
  if (preset == "war") p <- p - 0.30 * (t >= 13 & t <= 24)
  if (preset %in% c("over", "rising_over")) p <- p - 0.10 * (t >= 61 & t <= 72)
  data.frame(t = t, cost = cost, cartel = cartel, bf = bf, p = p)
}

# period: "before", "after", "both"; ctrl: control for cost; excl: leave out the 12 months next to the cartel
ba_estimate <- function(d, period = "before", ctrl = FALSE, excl = FALSE) {
  comp <- switch(period, before = d$t <= 24, after = d$t > 60, both = d$t <= 24 | d$t > 60)
  if (excl) comp <- comp & !((d$t >= 13 & d$t <= 24) | (d$t >= 61 & d$t <= 72))
  s <- d[comp | d$cartel, ]
  if (ctrl) {
    fit <- stats::lm(p ~ cost + cartel, data = s)
    tau <- unname(stats::coef(fit)[3])
    bf_hat <- unname(stats::coef(fit)[1] + stats::coef(fit)[2] * d$cost)
  } else {
    tau <- mean(d$p[d$cartel]) - mean(d$p[comp])
    bf_hat <- rep(mean(d$p[comp]), nrow(d))
  }
  pc <- mean(d$p[d$cartel])
  list(tau = tau, bf_hat = bf_hat, comp = comp, p_cartel = pc,
       pct_bf = tau / (pc - tau), pct_cartel = tau / pc, damages = tau * sum(d$cartel))
}

# ---- Tab 2: comparator market -----------------------------------------------
# gap: comparator is cheaper by this amount; trend: common trend per month;
# extra: comparator's own extra trend per month; share: cartel effect on the comparator as a share of tau
cm_tau <- 0.30
cm_world <- function(gap = 0.10, trend = 0.002, extra = 0, share = 0) {
  t <- 1:60
  cartel <- t >= 25
  data.frame(t = t, cartel = cartel,
             treat = 0.80 + trend * t + cm_tau * cartel,
             comp = 0.80 - gap + (trend + extra) * t + share * cm_tau * cartel)
}
cm_estimate <- function(d) {
  m <- c(tb = mean(d$treat[!d$cartel]), td = mean(d$treat[d$cartel]),
         cb = mean(d$comp[!d$cartel]), cd = mean(d$comp[d$cartel]))
  list(means = m, ba = unname(m["td"] - m["tb"]), yard = unname(m["td"] - m["cd"]),
       did = unname((m["td"] - m["tb"]) - (m["cd"] - m["cb"])))
}

# ---- Tab 3: which competition without the cartel? ---------------------------
# Demand p = 200 - 0.5 Q, marginal cost c, n sellers, 5 years
st_cartel <- function(c) { p <- (200 + c) / 2; list(p = p, Q = 2 * (200 - p)) }
st_cournot <- function(n, c) (200 + n * c) / (n + 1)
st_bertrand <- function(n, c) rep(c, length(n))
# Bertrand with differentiated products (Shubik-Levitan demand, as in the tool How many firms?):
# q_i = (1/n) [400 - 2 (1 + g) p_i + 2 g pbar]; at equal prices total demand is p = 200 - 0.5 Q.
# With k = 1 + g (n - 1)/n the equilibrium price is (200 + k c)/(1 + k); g = 0 gives the cartel price, g -> Inf gives c.
st_bertrand_diff <- function(n, c, g) { k <- 1 + g * (n - 1) / n; (200 + k * c) / (1 + k) }
st_damages <- function(n, c, model = "cournot", years = 5, g = 3) {
  k <- st_cartel(c)
  bf <- switch(model, cournot = st_cournot(n, c), bertrand = c, diff = st_bertrand_diff(n, c, g))
  oc <- k$p - bf
  list(p_cartel = k$p, Q = k$Q, bf = bf, overcharge = oc, pct_bf = oc / bf, pct_cartel = oc / k$p,
       damages = oc * k$Q * years, lost_units = 2 * (200 - bf) - k$Q)
}

# ---- Tab 4: who bears the harm? ---------------------------------------------
# n_d direct purchasers compete in quantities; downstream demand p = 100 - Q; unit cost 40 + dw
po_harm <- function(nd, dw, a = 100, c0 = 40) {
  p0 <- (a + nd * c0) / (nd + 1); Q0 <- a - p0
  p1 <- (a + nd * (c0 + dw)) / (nd + 1); Q1 <- a - p1
  paid <- dw * Q1
  passed <- (p1 - p0) * Q1
  lost <- (Q0 - Q1) * (p0 - c0)
  list(p0 = p0, p1 = p1, Q0 = Q0, Q1 = Q1, rho = nd / (nd + 1), margin0 = p0 - c0,
       paid = paid, passed = passed, lost = lost, net = paid - passed + lost,
       profit0 = (p0 - c0) * Q0, profit1 = (p1 - c0 - dw) * Q1,
       indirect_lost = 0.5 * (Q0 - Q1) * (p1 - p0))
}

# Half-up rounding for display
num <- function(x, digits = 2, big = ",") {
  v <- sign(x) * floor(abs(x) * 10^digits + 0.5 + 1e-9) / 10^digits + 0
  formatC(v, format = "f", digits = digits, big.mark = big)
}
