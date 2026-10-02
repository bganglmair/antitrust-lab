# Model functions for the welfare sandbox.
# Inverse demand p = a - b Q; marginal cost MC = c + d Q (d = 0: constant cost).
# Notation follows competition-tools/NOTATION.md.

mc_at <- function(Q, c, d) c + d * Q
demand_p <- function(Q, a, b) a - b * Q

surplus <- function(Q, p, a, b, c, d) {
  cs <- b * Q^2 / 2
  ps <- p * Q - (c * Q + d * Q^2 / 2)
  list(cs = cs, ps = ps, w = cs + ps)
}

# Competitive and monopoly outcomes and the welfare comparison
welfare_outcomes <- function(a, b, c, d) {
  if (a <= c) return(NULL)
  Qc <- (a - c) / (b + d); pc <- mc_at(Qc, c, d)   # equals demand_p(Qc); exactly c when d = 0
  Qm <- (a - c) / (2 * b + d); pm <- demand_p(Qm, a, b)
  mcm <- mc_at(Qm, c, d)
  sc <- surplus(Qc, pc, a, b, c, d); sm <- surplus(Qm, pm, a, b, c, d)
  list(
    Qc = Qc, pc = pc, Qm = Qm, pm = pm, mcm = mcm,
    cs_c = sc$cs, ps_c = sc$ps, w_c = sc$w,
    cs_m = sm$cs, ps_m = sm$ps, w_m = sm$w,
    dwl = sc$w - sm$w,                 # equals (pm - mcm) * (Qc - Qm) / 2
    lerner = (pm - mcm) / pm,
    eta_abs = pm / (b * Qm),           # elasticity of demand at the monopoly price
    transfer = (pm - pc) * Qm,         # paid by buyers to the firm (rectangle)
    rents = sm$ps - sc$ps              # profit above the competitive level (= transfer when d = 0)
  )
}

# Social cost of monopoly when a share alpha of the rents is spent on rent seeking
social_cost <- function(dwl, rents, alpha) dwl + alpha * rents
