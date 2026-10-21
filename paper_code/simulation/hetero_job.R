# ============================================================================
# One job of the heteroscedastic experiment: (noise structure, sigma, rep).
#
# Usage:  Rscript hetero_job.R <mode> <sigma> <rep> <outdir>
#
# Setting: m=500, n=250, rho=0.2, r=s=5, sigma as given,
#   noise H = N(0,1/2) + Rademacher/sqrt(2) (Var=1), then multiplied by the
#   mode-specific heteroscedastic index:
#     mode 1: row-wise Bernoulli   index = (Bern(0.2)+0.2)/sqrt(0.32)
#     mode 2: row-wise Gaussian    index = N(0,1)
#     mode 3: row-col Bernoulli    index = i_row (x) i_col, both as mode 1
#     mode 4: row-col Gaussian     index = a (x) b, a~N(0,1), b~N(0,1)
#   M_0 = u diag(1,1.2,1.4,1.6,1.8) v' * lambda_plus.
#
# Arms: base_fac / base_nuc (the two baselines), rsm_* (RSM), and the
# whitened RSM arms hetW_* (estimated noise profile) / hetWtrue_* (true law).
#
# ENV: RSM_STOP / RSM_INIT / RSM_CAP as in homo_job.R; SKIP_EXISTING=1 skips a
# complete CSV.
# ============================================================================

a <- commandArgs(trailingOnly = TRUE)
mode <- as.integer(a[1]); sigma <- as.numeric(a[2])
rep_i <- as.integer(a[3]); outdir <- a[4]

if (identical(Sys.getenv('SKIP_EXISTING'), '1')) {
  fn <- file.path(outdir, sprintf("hetero_m%d_s%g_rep%03d.csv", mode, sigma, rep_i))
  if (file.exists(fn)) {
    ncf <- tryCatch(count.fields(fn, sep = ','), error = function(e) integer(0))
    if (length(ncf) == 2L && length(unique(ncf)) == 1L) {
      cat(sprintf("SKIP %s\n", fn)); quit(save = 'no')
    }
  }
}

source('../R/baseline_methods.R')   # baselines
source('../R/rsm.R')             # residual spectral matching estimators

m <- 500; n <- 250; rho <- 0.2; r <- 5; s <- 5
p_ratio <- n / m
lambda_plus <- (m * n)^0.5 * (1 + p_ratio^0.5)
stepsize2 <- 10
tor_v <- 1e-4 * (sigma^2 + 1e-10)

set.seed(rep_i)
H <- matrix(rnorm(m * n), m, n) * 0.5^0.5 +
     (matrix(rbinom(m * n, 1, 0.5), m, n) * 2 - 1) * 0.5^0.5
if (mode == 1) idx_r <- (rbinom(m, 1, 0.2) + 0.2) / 0.32^0.5
if (mode == 2) idx_r <- rnorm(m)
if (mode == 3) idx_r <- (rbinom(m, 1, 0.2) + 0.2) / 0.32^0.5
if (mode == 4) idx_r <- rnorm(m)
if (mode == 1 || mode == 2) {
  H <- H * idx_r
} else {
  if (mode == 3) idx_c <- (rbinom(n, 1, 0.2) + 0.2) / 0.32^0.5
  if (mode == 4) idx_c <- rnorm(n)
  H <- H * (matrix(idx_r, m, 1) %*% idx_c)
}
S <- matrix(rnorm(m * n), m, n)
svd_S <- svd(S)
M_0 <- svd_S$u[1:m, 1:r] %*% diag(c(1, 1.2, 1.4, 1.6, 1.8)) %*%
       t(svd_S$v[1:n, 1:r]) * lambda_plus
M <- H * sigma + M_0
obs <- sample.int(m * n, round(m * n * rho), replace = FALSE)
X1 <- (obs - 1) %/% n + 1
X2 <- obs %% n + 1
Y <- M[cbind(X1, X2)]
M_init <- M_0 + matrix(rnorm(m * n), m, n) * 5

mb0            <- if (identical(Sys.getenv('RSM_INIT'), 'oracle')) M_init else NULL
init_iters_rsm <- as.integer(Sys.getenv('RSM_INIT_ITERS', '100'))

rsm_stop <- Sys.getenv('RSM_STOP', 'none')
rsm_cap  <- as.integer(Sys.getenv('RSM_CAP', '1500'))

ef <- function(A) if (is.null(A) || any(!is.finite(A))) NA_real_ else
                  sqrt(mean((M_0 - A)^2) / mean(M_0^2))
sp_den  <- svd(M_0)$d[1]
inf_den <- max(abs(M_0))
esp <- function(A) {
  if (is.null(A) || any(!is.finite(A))) return(NA_real_)
  D <- M_0 - A
  d <- tryCatch(svd(D)$d[1], error = function(e) NA_real_)
  if (!is.finite(d)) {
    s <- max(abs(D)); if (!is.finite(s) || s == 0) s <- 1
    ev <- tryCatch(eigen(crossprod(D / s), symmetric = TRUE, only.values = TRUE)$values[1],
                   error = function(e) NA_real_)
    d <- s * sqrt(max(ev, 0))
  }
  d / sp_den
}
einf <- function(A) {
  if (is.null(A) || any(!is.finite(A))) return(NA_real_)
  v <- tryCatch(max(abs(M_0 - A)), error = function(e) NA_real_)
  v / inf_den
}
safe <- function(e) tryCatch(suppressWarnings(e),
                             error = function(x) { cat('ARM ERROR:', conditionMessage(x), '\n'); NULL })
timed <- function(e) { t0 <- proc.time()[3]; v <- safe(e)
                       list(v = v, t = proc.time()[3] - t0) }

out <- list(mode = mode, sigma = sigma, rep = rep_i)

r1 <- timed(Matrix_factor_func(X1, X2, Y, length(X1), m, n, 0, s,
              init = TRUE, M_input = M_init, step_size = stepsize2,
              itertime = 30000, penalty = 'None', tor = tor_v))
out$base_fac <- ef(r1$v); out$base_fac_sp <- esp(r1$v); out$base_fac_inf <- einf(r1$v); out$t_base_fac <- r1$t

r4 <- timed(Nuclear_opt_func(X1, X2, Y, length(X1), m, n, 0.5 * lambda_plus, 0.2,
              init = TRUE, M_input = M_init, itertime = 30000, tor = tor_v))
out$base_nuc <- ef(r4$v); out$base_nuc_sp <- esp(r4$v); out$base_nuc_inf <- einf(r4$v); out$t_base_nuc <- r4$t

lam <- rsm_reference(X1, X2, m, n, T = 100, seed = 1000 + rep_i)

rpa <- if (rsm_stop == 'val') {
  timed(Matrix_factor_rsm_val(X1, X2, Y, length(X1), m, n, s = s, rho = rho,
        lambda_hat = lam, cap = rsm_cap, Mb_init = mb0))
} else {
  timed(Matrix_factor_rsm(X1, X2, Y, length(X1), m, n, branch = "ssa", s = s, rho = rho,
        lambda_hat = lam, init_iters = init_iters_rsm,
        Mb_init = mb0))
}
out$rsm_fac <- ef(rpa$v); out$rsm_fac_sp <- esp(rpa$v); out$rsm_fac_inf <- einf(rpa$v)
out$t_rsm_fac <- rpa$t
bkpa <- attr(rpa$v, 'k_best')
out$rsm_fac_k <- if (is.null(bkpa)) NA_integer_ else as.integer(bkpa)

nuc_start <- if (rsm_stop == 'val') rpa$v else mb0
nuc_iters <- if (rsm_stop == 'val') 0L else init_iters_rsm
bn <- NA_real_; bl <- NA_real_; tb <- NA_real_; bA <- NULL; bk <- NA_integer_
for (la in 10^seq(-7, -2, by = 1)) {
  rt <- timed(Nuclear_opt_rsm(X1, X2, Y, length(X1), m, n, branch = "ssa", la, rho, s = s,
              lambda_hat = lam, init_iters = nuc_iters,
              svt_iters = 100, Mb_init = nuc_start))
  e <- ef(rt$v)
  if (!is.na(e) && (is.na(bn) || e < bn)) {
    bn <- e; bl <- la; tb <- rt$t; bA <- rt$v
    bk <- attr(rt$v, 'k_best'); bk <- if (is.null(bk)) NA_integer_ else as.integer(bk)
  }
}
out$rsm_nuc_k <- if (rsm_stop == 'val') {
  if (is.null(bkpa)) NA_integer_ else as.integer(bkpa)
} else bk
out$rsm_nuc <- bn; out$rsm_nuc_sp <- esp(bA); out$rsm_nuc_inf <- einf(bA)
out$rsm_nuc_lam <- bl; out$t_rsm_nuc <- tb

if (is.null(rpa$v)) {
  out$hetW_fac <- NA_real_; out$hetW_fac_sp <- NA_real_; out$hetW_fac_inf <- NA_real_
  out$t_hetW_fac <- NA_real_; out$hetW_fac_k <- NA_integer_
  out$hetW_nuc <- NA_real_; out$hetW_nuc_sp <- NA_real_; out$hetW_nuc_inf <- NA_real_
  out$hetW_nuc_lam <- NA_real_; out$hetW_nuc_k <- NA_integer_
  out$t_hetW_nuc <- NA_real_
} else {
  W_hat <- 1 / rsm_estimate_tau(X1, X2, Y - rpa$v[cbind(X1, X2)], m, n)
  rpc <- if (rsm_stop == 'val') {
    timed(Matrix_factor_rsm_val(X1, X2, Y, length(X1), m, n, s = s, rho = rho,
          lambda_hat = lam, W = W_hat, cap = rsm_cap, Mb_init = mb0))
  } else {
    timed(Matrix_factor_rsm(X1, X2, Y, length(X1), m, n, branch = "ssa", s = s, rho = rho,
          lambda_hat = lam, W = W_hat,
          init_iters = init_iters_rsm, Mb_init = mb0))
  }
  out$hetW_fac <- ef(rpc$v); out$hetW_fac_sp <- esp(rpc$v); out$hetW_fac_inf <- einf(rpc$v)
  out$t_hetW_fac <- rpc$t
  bkpc <- attr(rpc$v, 'k_best')
  out$hetW_fac_k <- if (is.null(bkpc)) NA_integer_ else as.integer(bkpc)

  nucW_start <- if (rsm_stop == 'val') rpc$v else mb0
  nucW_iters <- if (rsm_stop == 'val') 0L else init_iters_rsm
  bn <- NA_real_; bl <- NA_real_; tb <- NA_real_; bA <- NULL; bk <- NA_integer_
  for (la in 10^seq(-7, -2, by = 1)) {
    rt <- timed(Nuclear_opt_rsm(X1, X2, Y, length(X1), m, n, branch = "ssa", la, rho, s = s,
                lambda_hat = lam, W = W_hat,
                init_iters = nucW_iters, svt_iters = 100, Mb_init = nucW_start))
    e <- ef(rt$v)
    if (!is.na(e) && (is.na(bn) || e < bn)) {
      bn <- e; bl <- la; tb <- rt$t; bA <- rt$v
      bk <- attr(rt$v, 'k_best'); bk <- if (is.null(bk)) NA_integer_ else as.integer(bk)
    }
  }
  out$hetW_nuc_k <- if (rsm_stop == 'val') {
    if (is.null(bkpc)) NA_integer_ else as.integer(bkpc)
  } else bk
  out$hetW_nuc <- bn; out$hetW_nuc_sp <- esp(bA); out$hetW_nuc_inf <- einf(bA)
  out$hetW_nuc_lam <- bl; out$t_hetW_nuc <- tb
}

tau_true <- if (mode <= 2) outer(abs(idx_r), rep(1, n)) else
                           outer(abs(idx_r), abs(idx_c))
tau_true <- pmax(tau_true, quantile(tau_true, 0.25, names = FALSE))
W_true <- 1 / tau_true
rpd <- if (rsm_stop == 'val') {
  timed(Matrix_factor_rsm_val(X1, X2, Y, length(X1), m, n, s = s, rho = rho,
        lambda_hat = lam, W = W_true, cap = rsm_cap, Mb_init = mb0))
} else {
  timed(Matrix_factor_rsm(X1, X2, Y, length(X1), m, n, branch = "ssa", s = s, rho = rho,
        lambda_hat = lam, W = W_true,
        init_iters = init_iters_rsm, Mb_init = mb0))
}
out$hetWtrue_fac <- ef(rpd$v); out$hetWtrue_fac_sp <- esp(rpd$v); out$hetWtrue_fac_inf <- einf(rpd$v)
out$t_hetWtrue_fac <- rpd$t
bkpd <- attr(rpd$v, 'k_best')
out$hetWtrue_fac_k <- if (is.null(bkpd)) NA_integer_ else as.integer(bkpd)

nucT_start <- if (rsm_stop == 'val') rpd$v else mb0
nucT_iters <- if (rsm_stop == 'val') 0L else init_iters_rsm
bn <- NA_real_; bl <- NA_real_; tb <- NA_real_; bA <- NULL; bk <- NA_integer_
for (la in 10^seq(-7, -2, by = 1)) {
  rt <- timed(Nuclear_opt_rsm(X1, X2, Y, length(X1), m, n, branch = "ssa", la, rho, s = s,
              lambda_hat = lam, W = W_true,
              init_iters = nucT_iters, svt_iters = 100, Mb_init = nucT_start))
  e <- ef(rt$v)
  if (!is.na(e) && (is.na(bn) || e < bn)) {
    bn <- e; bl <- la; tb <- rt$t; bA <- rt$v
    bk <- attr(rt$v, 'k_best'); bk <- if (is.null(bk)) NA_integer_ else as.integer(bk)
  }
}
out$hetWtrue_nuc_k <- if (rsm_stop == 'val') {
  if (is.null(bkpd)) NA_integer_ else as.integer(bkpd)
} else bk
out$hetWtrue_nuc <- bn; out$hetWtrue_nuc_sp <- esp(bA); out$hetWtrue_nuc_inf <- einf(bA)
out$hetWtrue_nuc_lam <- bl; out$t_hetWtrue_nuc <- tb

dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
df <- as.data.frame(out, stringsAsFactors = FALSE)
fn <- file.path(outdir, sprintf("hetero_m%d_s%g_rep%03d.csv", mode, sigma, rep_i))
write.csv(df, fn, row.names = FALSE)

cat(sprintf("OK %s\n", fn))
