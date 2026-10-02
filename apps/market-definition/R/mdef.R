# Market definition: model functions (notation: m margin, dp SSNIP size)

# Critical loss: largest share of sales the hypothetical monopolist can lose
# before a price increase of dp (fraction) becomes unprofitable.
cl_star <- function(dp, m) dp / (dp + m)

# Predicted loss from an own-price elasticity of the candidate set
loss_from_elasticity <- function(eps, dp) abs(eps) * dp

# Predicted loss from margins and aggregate diversion (Katz and Shapiro 2003):
# linear demand, pre-SSNIP prices profit-maximizing for each product,
# A = share of lost sales recaptured by the other products in the candidate set.
loss_from_diversion <- function(dp, m, A) pmin(dp * (1 - A) / m, 1)

# Exact percentage change in the hypothetical monopolist's profit
# when price rises by dp and a share L of sales is lost.
profit_change <- function(dp, m, L) ((m + dp) * (1 - L) - m) / m

ssnip_verdict <- function(L, cl) ifelse(L < cl - 1e-9, "profitable", "unprofitable")

# Wording for students
ssnip_word <- function(L, cl) ifelse(abs(L - cl) < 1e-9, "breaks even", ifelse(L < cl, "is profitable", "is not profitable"))

# Linear demand for the cellophane tab: Q = 100 * (1 - p / pmax)
lin_q    <- function(p, pmax) 100 * (1 - p / pmax)
lin_eps  <- function(p, pmax) p / (pmax - p)          # absolute value
lin_pm   <- function(pmax, c) (pmax + c) / 2           # monopoly price

cellophane_row <- function(label, p, c, pmax, dp) {
  m  <- (p - c) / p
  e  <- lin_eps(p, pmax)
  L  <- loss_from_elasticity(e, dp)
  cl <- cl_star(dp, m)
  data.frame(start = label, price = p, margin = m, elasticity = e,
             critical_loss = cl, predicted_loss = L,
             verdict = ssnip_verdict(L, cl), stringsAsFactors = FALSE)
}

# Wrap long axis labels (base R, no stringr dependency)
stringr_wrap <- function(x, width = 18) vapply(x, function(s) paste(strwrap(s, width), collapse = "\n"), "")
