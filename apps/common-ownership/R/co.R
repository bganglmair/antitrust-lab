# Common ownership: profit weights (kappa) and the modified HHI, one share one vote.
# beta: matrix investors x firms, stakes as fractions. s: market shares as fractions.

co_kappa <- function(beta) {
  n <- ncol(beta)
  num <- t(beta) %*% beta            # [f, g] = sum_k beta_kf * beta_kg
  den <- diag(num)                   # sum_k beta_kf^2
  K <- matrix(0, n, n)
  for (f in seq_len(n)) if (den[f] > 0) K[f, ] <- num[f, ] / den[f]
  diag(K) <- 1
  K
}
co_hhi <- function(s) sum((100 * s)^2)
co_delta <- function(K, s) { S <- 100 * s; M <- K * outer(S, S); diag(M) <- 0; sum(M) }
num <- function(x, digits = 2, big = ",") {
  v <- sign(x) * floor(abs(x) * 10^digits + 0.5 + 1e-9) / 10^digits + 0
  formatC(v, format = "f", digits = digits, big.mark = big)
}
