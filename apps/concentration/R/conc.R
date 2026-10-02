# Model functions for the concentration tool.
# Shares are fractions inside every function; the app shows percent.
# Notation follows competition-tools/NOTATION.md.

round_half_up <- function(x, digits = 0) {
  m <- 10^digits
  sign(x) * floor(abs(x) * m + 0.5 + 1e-9) / m   # 1e-9: products such as 2 * 0.5 * 14.5 land just below .5 in floating point
}

# HHI on the 0-10,000 scale
hhi <- function(s) 10000 * sum(s^2)

# Change in HHI when firms with shares sA and sB merge (shares add)
delta_hhi <- function(sA, sB) 10000 * 2 * sA * sB

# Concentration ratio of the k largest firms, in percent
cr_k <- function(s, k = 4) 100 * sum(utils::head(sort(s, decreasing = TRUE), k))

# Market as a vector of shares: named firms plus "others" split into n_oth equal firms
market_shares <- function(named, others, n_oth = 1) {
  n_oth <- max(1, round(n_oth))
  c(named, if (others > 1e-9) rep(others / n_oth, n_oth) else numeric(0))
}

# Cournot: firm Lerner index and share-weighted industry Lerner index
lerner_firm <- function(s, eta_abs) s / eta_abs
lerner_industry <- function(s, eta_abs) sum(s^2) / eta_abs
cournot_consistent <- function(s, eta_abs) eta_abs > max(s) + 1e-12

# Symmetric markets: n equal firms
lerner_sym <- function(n, eta_abs) {
  data.frame(
    n = n, hhi = 10000 / n,
    cournot = 1 / (n * eta_abs),
    bertrand = ifelse(n >= 2, 0, 1 / eta_abs),
    coordination = 1 / eta_abs
  )
}

# US 2023 Merger Guidelines, Guideline 1 (section 2.1): presumption of harm
us_presumption <- function(post, delta, comb) {
  (post > 1800 & delta > 100) | (comb > 0.30 & delta > 100)
}

# EU 2004 Horizontal Merger Guidelines, paras 18-20: below the levels where concerns are unlikely
eu_below <- function(post, delta, comb) {
  comb <= 0.25 | post < 1000 | (post <= 2000 & delta < 250) | (post > 2000 & delta < 150)
}

# Which EU route applies (for the explanation text)
eu_route <- function(post, delta, comb) {
  if (comb <= 0.25) return("the combined share is 25% or less")
  if (post < 1000) return("the HHI after the merger is below 1,000")
  if (post <= 2000 && delta < 250) return("the HHI is between 1,000 and 2,000 and rises by less than 250")
  if (post > 2000 && delta < 150) return("the HHI is above 2,000 and rises by less than 150")
  NA_character_
}

# Merger screen for a market and a merging pair (indices into s)
merger_screen <- function(s, iA, iB) {
  pre <- hhi(s)
  post_s <- c(s[iA] + s[iB], s[-c(iA, iB)])
  # Thresholds are applied to the values shown on screen: HHI rounded to whole points,
  # combined share rounded to basis points (avoids 0.2 + 0.1 > 0.3 in floating point)
  pre <- round_half_up(pre)
  post <- round_half_up(hhi(post_s))
  delta <- round_half_up(delta_hhi(s[iA], s[iB]))
  comb <- round(10000 * (s[iA] + s[iB])) / 10000
  list(pre = pre, post = post, delta = delta, comb = comb,
       cr4 = cr_k(s, 4), cr3 = cr_k(s, 3), cr4_post = cr_k(post_s, 4),
       us = us_presumption(post, delta, comb), eu = eu_below(post, delta, comb),
       eu_route = eu_route(post, delta, comb))
}

# GUPPI with equal prices: GUPPI_A = D_AB * m_B, where m_B = (p_B - c_B)/p_B
guppi <- function(D, m_other) D * m_other

# Diversion in proportion to shares, no customer leaving the market: D_AB = s_B / (1 - s_A)
diversion_share_benchmark <- function(s_own, s_other) s_other / (1 - s_own)

# Rivals rescaled to fill what the merging pair leaves (equal proportions as in the preset)
rescale_rivals <- function(rivals, sA, sB) {
  rest <- 1 - sA - sB
  if (rest <= 1e-9 || sum(rivals) <= 0) return(numeric(0))
  rivals / sum(rivals) * rest
}

# Dominance benchmark for the largest firm (EU, Art. 102; Commission Guidelines on exclusionary abuses 2026, para 24)
dominance_text <- function(smax) {
  smax <- round(10000 * smax) / 10000
  if (smax >= 0.50) "50% or more: strong indication of dominance (AKZO)"
  else if (smax >= 0.40) "40-50%: dominance possible, other factors decide"
  else "Below 40%: dominance generally unlikely"
}
