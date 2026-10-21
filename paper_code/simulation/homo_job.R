# One homoscedastic simulation job: a single (family, configuration, rep).
# Writes one CSV with E_F / E_sp / E_inf of every arm and each arm's wall time.
#
# Usage:
#   Rscript homo_job.R <family> <rep> <outdir> <p1> [p2] [p3]
#
#   family = size    p1 = m,   p2 = r              (n = m/2, rho=.2, sigma=1, s=r)
#   family = cond    p1 = kappa, p2 = r            (m=500, n=250, rho=.05, sigma=1, s=r)
#   family = noise   p1 = r,   p2 = sigma          (m=500, n=250, rho=.2, s=r)
#   family = supp    p1 = sigma                    (m=500, n=250, rho=.2, r=s=5,
#                                                   M_0 = L t(R) factor model)
#   family = robust  p1 = m,   p2 = r, p3 = h      (n=m/2, rho=.2, sigma=1, s=r,
#                                                   H <- H * noise_index)
#   family = dep     p1 = r,   p2 = mode, p3 = ratio  (m=500, n=250, rho=.2,
#                                                   sigma=1, s=r, H <- Sigma %*% H)
#   family = misrank p1 = r,   p2 = s              (m=500, n=250, rho=.2, sigma=1)
#
# Arms: base_fac / base_nuc (the two baselines) and rsm_fac / rsm_nuc (RSM).
#
# ENV:
#   RSM_STOP   "none" (default) or "val" (Section 5 held-out stopping rule).
#   RSM_CAP    stage-1 iteration cap under RSM_STOP=val (default 1500);
#              RSM_INIT_ITERS is the fixed budget under RSM_STOP=none.
#   RSM_INIT   unset (data-driven start) or "oracle" (the paper's init).
#   HOMO_TOR   convergence tolerance of the baselines (default 1e-4).
#   SKIP_EXISTING=1  skip a job whose CSV is already complete.
# ============================================================================

a <- commandArgs(trailingOnly = TRUE)
family <- a[1]
rep_i  <- as.integer(a[2])
outdir <- a[3]
p1 <- if (length(a) >= 4) as.numeric(a[4]) else NA_real_
p2 <- if (length(a) >= 5) as.numeric(a[5]) else NA_real_
p3 <- if (length(a) >= 6) as.numeric(a[6]) else NA_real_

source('../R/baseline_methods.R')     # baselines: Matrix_factor_func / Nuclear_opt_func
source('../R/rsm.R')               # residual spectral matching estimators

sigma <- 1; rho <- 0.2
kappa <- NA_real_; hetero_size <- NA_real_
dep_mode <- NA_real_; dep_ratio <- NA_real_

if (family == "size")   { m <- p1;            r <- p2; n <- m / 2; s <- r }
if (family == "cond")   { kappa <- p1;        r <- p2; m <- 500; n <- 250
                          rho <- 0.05;        s <- r }
if (family == "noise")  { r <- p1; sigma <- p2; m <- 500; n <- 250; s <- r }
if (family == "supp")   { sigma <- p1;        m <- 500; n <- 250; r <- 5; s <- 5 }
if (family == "robust") { m <- p1;            r <- p2; hetero_size <- p3
                          n <- m / 2;         s <- r }
if (family == "dep")    { r <- p1; dep_mode <- p2; dep_ratio <- p3
                          m <- 500; n <- 250; s <- r }
if (family == "misrank") { r <- p1;           s <- p2; m <- 500; n <- 250 }

m <- as.integer(m); n <- as.integer(n); r <- as.integer(r); s <- as.integer(s)
pr <- n / m
lambda_plus <- (m * n)^0.5 * (1 + pr^0.5)

out_file <- function() {
  key <- c(family, m, n, rho, r, s)
  key <- c(key, if (family == "cond") paste0("k", kappa) else sigma)
  if (family == "robust") key <- c(key, paste0("h", hetero_size))
  if (family == "dep")    key <- c(key, paste0("d", dep_mode, "_", dep_ratio))
  file.path(outdir,
            sprintf("homo_%s_rep%03d.csv", paste(key, collapse = "_"), rep_i))
}
ncol_expected <- 29L
fn <- out_file()
if (identical(Sys.getenv('SKIP_EXISTING'), '1') && file.exists(fn)) {
  ncf <- tryCatch(count.fields(fn, sep = ','), error = function(e) integer(0))
  if (length(ncf) == 2L && all(ncf == ncol_expected)) {
    cat(sprintf("SKIP %s\n", fn)); quit(save = 'no')
  }
}

stepsize2 <- if (family == "cond") 10 / kappa else 10
nuc_step_base <- if (family == "cond") 0.2 / kappa else 0.2
tor_v <- as.numeric(Sys.getenv("HOMO_TOR", "1e-4")) * (sigma^2 + 1e-10)

if (family == "robust") {
  set.seed(123456)
  noise_index <- matrix(runif(m * n, 1 - hetero_size, 1 + hetero_size), m, n)
}
if (family == "dep") {
  ii <- row(matrix(0, m, m)); jj <- col(matrix(0, m, m))
  if (dep_mode == 1) Sn <- dep_ratio^abs(ii - jj)
  if (dep_mode == 2) Sn <- (abs(ii - jj) %% m <= dep_ratio) * 1
  Sn <- Sn / (sum(Sn^2) / m)^0.5
}

set.seed(rep_i)
H <- matrix(rnorm(m * n), m, n) * 0.5^0.5 +
     (matrix(rbinom(m * n, 1, 0.5), m, n) * 2 - 1) * 0.5^0.5
if (family == "robust") H <- H * noise_index
if (family == "dep")    H <- Sn %*% H

if (family == "supp") {
  L <- matrix(rnorm(m * r), m, r)
  R <- matrix(rnorm(n * r), n, r)
  M_0 <- L %*% t(R)
} else {
  S <- matrix(rnorm(m * n), m, n)
  svd_S <- svd(S)
  if (family == "cond") {
    kappa_ <- kappa - 1
    if (r == 5)  D0 <- c(kappa_ + 1, 0.75 * kappa_ + 1, 0.5 * kappa_ + 1,
                         0.25 * kappa_ + 1, 1)
    if (r == 10) D0 <- c(kappa_ + 1, 8/9*kappa_ + 1, 7/9*kappa_ + 1,
                         6/9*kappa_ + 1, 5/9*kappa_ + 1, 4/9*kappa_ + 1,
                         3/9*kappa_ + 1, 2/9*kappa_ + 1, 1/9*kappa_ + 1, 1)
    if (r == 20) D0 <- c(kappa, (18:1) / 19 * kappa_ + 1, 1)
  } else {
    D0 <- seq(1, 2 - 1 / r, length.out = r)
  }
  M_0 <- svd_S$u[1:m, 1:r] %*% diag(D0) %*% t(svd_S$v[1:n, 1:r]) * lambda_plus
}
M <- H * sigma + M_0
obs <- sample.int(m * n, round(m * n * rho), replace = FALSE)
X1 <- (obs - 1) %/% n + 1
X2 <- obs %% n + 1
Y <- M[cbind(X1, X2)]

ef <- function(A) {
  if (is.null(A) || !is.matrix(A) || any(!is.finite(A))) NA_real_
  else sqrt(mean((M_0 - A)^2) / mean(M_0^2))
}
sp_den  <- svd(M_0)$d[1]
inf_den <- max(abs(M_0))
esp <- function(A) {
  if (is.null(A) || !is.matrix(A) || any(!is.finite(A))) return(NA_real_)
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
  if (is.null(A) || !is.matrix(A) || any(!is.finite(A))) return(NA_real_)
  v <- tryCatch(max(abs(M_0 - A)), error = function(e) NA_real_)
  v / inf_den
}
safe <- function(e) tryCatch(suppressWarnings(e),
                             error = function(x) { cat('ARM ERROR:', conditionMessage(x), '\n'); NULL })
timed <- function(e) { t0 <- Sys.time(); v <- safe(e)
                       list(v = v, t = as.numeric(Sys.time() - t0, units = "secs")) }

out <- list(family = family, rep = rep_i, m = m, n = n, rho = rho, r = r,
            s = s, sigma = sigma, kappa = kappa, hetero_size = hetero_size,
            dep_mode = dep_mode, dep_ratio = dep_ratio)

M_init <- M_0 + matrix(rnorm(m * n), m, n) * 5
mb0 <- if (identical(Sys.getenv('RSM_INIT'), 'oracle')) M_init else NULL
init_iters_rsm <- as.integer(Sys.getenv('RSM_INIT_ITERS', '100'))
rsm_stop <- Sys.getenv('RSM_STOP', 'none')
rsm_cap <- as.integer(Sys.getenv('RSM_CAP', '1500'))

A <- safe(Matrix_factor_func(X1, X2, Y, length(X1), m, n, 0, s,
            init = TRUE, M_input = M_init, step_size = stepsize2,
            itertime = 30000, penalty = 'None', tor = tor_v))
out$base_fac <- ef(A); out$base_fac_sp <- esp(A); out$base_fac_inf <- einf(A)
A <- safe(Nuclear_opt_func(X1, X2, Y, length(X1), m, n,
            0.5 * lambda_plus, nuc_step_base, init = TRUE,
            M_input = M_init, itertime = 30000, tor = tor_v))
out$base_nuc <- ef(A); out$base_nuc_sp <- esp(A); out$base_nuc_inf <- einf(A)

set.seed(1000 + rep_i)
lam <- rsm_reference(X1, X2, m, n, T = 100, seed = 1000 + rep_i)

rf <- if (rsm_stop == 'val') {
  timed(Matrix_factor_rsm_val(X1, X2, Y, length(X1), m, n, s = s, rho = rho,
        lambda_hat = lam, cap = rsm_cap, Mb_init = mb0))
} else {
  timed(Matrix_factor_rsm(X1, X2, Y, length(X1), m, n, s = s, rho = rho,
        lambda_hat = lam, branch = "ssa", init_iters = init_iters_rsm,
        Mb_init = mb0))
}
out$rsm_fac <- ef(rf$v); out$rsm_fac_sp <- esp(rf$v); out$rsm_fac_inf <- einf(rf$v)
out$t_rsm_fac <- rf$t
bk_f <- attr(rf$v, 'k_best')
out$rsm_fac_k <- if (is.null(bk_f)) NA_integer_ else as.integer(bk_f)

nuc_start <- if (rsm_stop == 'val') rf$v else mb0
nuc_iters <- if (rsm_stop == 'val') 0L else init_iters_rsm
bn <- NA_real_; bl <- NA_real_; tb <- NA_real_; bA <- NULL; bk <- NA_integer_
fac_failed <- rsm_stop == 'val' && (is.null(rf$v) || !is.finite(out$rsm_fac))
for (la in if (fac_failed) numeric(0) else 10^seq(-7, -2, by = 0.5)) {
  rt <- timed(Nuclear_opt_rsm(X1, X2, Y, length(X1), m, n, la, rho, s = s,
              lambda_hat = lam, branch = "ssa", init_iters = nuc_iters,
              svt_iters = 100, Mb_init = nuc_start))
  e <- ef(rt$v)
  if (!is.na(e) && (is.na(bn) || e < bn)) {
    bn <- e; bl <- la; tb <- rt$t; bA <- rt$v
    bk <- attr(rt$v, 'k_best')
    bk <- if (is.null(bk)) NA_integer_ else as.integer(bk)
  }
}
out$rsm_nuc_k <- if (rsm_stop == 'val') {
  kk <- attr(rf$v, 'k_best'); if (is.null(kk)) NA_integer_ else as.integer(kk)
} else bk
out$rsm_nuc <- bn; out$rsm_nuc_sp <- esp(bA); out$rsm_nuc_inf <- einf(bA)
out$rsm_nuc_lam <- bl; out$t_rsm_nuc <- tb

dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
write.csv(as.data.frame(out, stringsAsFactors = FALSE), fn, row.names = FALSE)
cat(sprintf("OK %s\n", fn))
