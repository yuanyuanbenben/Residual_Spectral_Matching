#!/usr/bin/env Rscript
# One real-data fit (Netflix or Amazon).
#
#   Rscript real_job.R <dataset> <case> <method> <param> [outdir]
#
#   dataset : netflix | amazon
#   case    : 1 | 2 | 3
#   method  : ref | base_fac | base_nuc | rsm_fac | rsm_nuc |
#             het_ref | hetW_fac | hetW_nuc
#   param   : s              for *_fac and het_ref
#             lambda         for *_nuc
#             s:lambda       for rsm_nuc and hetW_nuc
#
# Writes one row of metrics to <outdir>/<method>_case<case>_<key>.csv.

suppressPackageStartupMessages({
  library(stats)
  library(utils)
})

job_dir <- function() {
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub('^--file=', '', a[grep('^--file=', a)])
  if (length(f) == 1L) return(normalizePath(dirname(f)))
  normalizePath('.')
}
THIS <- job_dir()
source(file.path(THIS, '..', 'R', 'rsm.R'))
source(file.path(THIS, '..', 'R', 'baseline_methods.R'))

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 4L) {
  stop('usage: real_job.R <dataset> <case> <method> <param> [outdir]')
}
dataset <- args[[1]]
case    <- as.integer(args[[2]])
method  <- args[[3]]
param   <- args[[4]]
outdir  <- if (length(args) >= 5L) args[[5]] else
  file.path(THIS, 'output_real', dataset)

# ---------------------------------------------------------------------------
# Data
# ---------------------------------------------------------------------------
root <- Sys.getenv('REAL_DATA_ROOT')
if (!nzchar(root)) {
  stop('REAL_DATA_ROOT is not set: point it at the directory that holds ',
       'real_data_example/ and real_data_example2/')
}
data_dir <- file.path(root,
                      if (dataset == 'netflix') 'real_data_example' else 'real_data_example2')
if (!dir.exists(data_dir)) {
  stop('data directory not found: ', data_dir, ' (check REAL_DATA_ROOT)')
}
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

read_split <- function(split) {
  z <- read.csv(file.path(data_dir, 'data', sprintf('case%d_%sdata.csv', case, split)))
  list(X1 = z[[2]], X2 = z[[3]], Y = z[[4]])
}
tr <- read_split('train')
va <- read_split('validation')
te <- read_split('test')

m <- max(tr$X1); n <- max(tr$X2); K <- length(tr$Y)
rho <- K / (m * n)

M_train <- matrix(0, m, n)
M_train[cbind(tr$X1, tr$X2)] <- tr$Y

# ---------------------------------------------------------------------------
# Metrics
# ---------------------------------------------------------------------------
rms <- function(Y, Yhat) sqrt(mean((Yhat - Y)^2))
maxabs <- function(Y, Yhat) max(abs(Yhat - Y))
clip15 <- function(M) {
  M[M > 5] <- 5
  M[M < 1] <- 1
  M
}
metrics <- function(M) {
  vh <- M[cbind(va$X1, va$X2)]
  th <- M[cbind(te$X1, te$X2)]
  Mc <- clip15(M)
  vhc <- Mc[cbind(va$X1, va$X2)]
  thc <- Mc[cbind(te$X1, te$X2)]
  c(val_rmse = rms(va$Y, vh), val_max = maxabs(va$Y, vh),
    test_rmse = rms(te$Y, th), test_max = maxabs(te$Y, th),
    val_rmse_mod = rms(va$Y, vhc), val_max_mod = maxabs(va$Y, vhc),
    test_rmse_mod = rms(te$Y, thc), test_max_mod = maxabs(te$Y, thc))
}

rank_s_init <- function(s) {
  sv <- svd(M_train)
  s_eff <- min(s, sum(sv$d > 1e-10))
  sv$u[, seq_len(s_eff), drop = FALSE] %*%
    (sv$d[seq_len(s_eff)] * t(sv$v[, seq_len(s_eff), drop = FALSE])) / rho
}

auto_ssa_step <- function(lam, s, frac = AUTO_FRAC, mu0 = 1, K0 = 5, C_Q = 1,
                          gamma = 1e-8, W = NULL) {
  Om <- matrix(0, m, n); Om[cbind(tr$X1, tr$X2)] <- 1
  Ymat <- matrix(0, m, n); Ymat[cbind(tr$X1, tr$X2)] <- tr$Y
  svZ <- svd(Ymat / rho)
  s_eff <- min(s, sum(svZ$d > 1e-10))
  Zs <- svZ$u[, seq_len(s_eff), drop = FALSE] %*%
        (svZ$d[seq_len(s_eff)] * t(svZ$v[, seq_len(s_eff), drop = FALSE]))
  Bnd <- K0 * sqrt(mu0 * s) * svZ$d[1]
  Mb <- rsm_proj_B(Zs, Bnd, m, n)
  Ew <- if (is.null(W)) Ymat - Mb * Om else W * (Ymat - Mb * Om)
  svE <- svd(Ew)
  sig <- rsm_sigma_hat(svE$d, lam, m)
  b <- sqrt(m) * sig
  Q <- b * (svE$u %*% (lam * t(svE$v)))
  PQw <- if (is.null(W)) Q * Om else (W * Q) * Om
  nq <- norm(PQw, 'F')
  alpha <- if (nq <= 0) 1 else min(1, C_Q * sig * sqrt(m * rho) / nq)
  G <- (-Ew + alpha * PQw) / rho
  bal <- rsm_bal(Mb, s)
  dir <- rsm_direction(G, bal$L, bal$R, s, gamma)
  dM <- dir$DL %*% t(bal$R) + bal$L %*% t(dir$DR)
  frac * norm(Mb, 'F') / norm(dM, 'F')
}

choose_step <- function(lam, s, k_try = 10L, W = NULL) {
  base <- auto_ssa_step(lam, s, frac = 0.1, W = W)
  mult <- c(5, 2.5, 1.25, 0.625)
  vs <- max(1L, round(1 / 0.1))
  isv <- seq.int(vs, K, by = vs)
  fit <- setdiff(seq_len(K), isv)
  t5 <- as.integer(Sys.getenv('REAL_TREF_STEP', unset = '5'))
  lam_f <- rsm_reference(tr$X1[fit], tr$X2[fit], m, n, T = t5, seed = REF_SEED)
  v <- vapply(mult, function(mu) {
    con <- if (is.null(W)) {
      rsm_construct(tr$X1[fit], tr$X2[fit], tr$Y[fit], m, n, s,
                    length(fit) / (m * n), lam_f, branch = 'ssa',
                    init_iters = k_try, ssa_step = base * mu, patience = 0L,
                    val_ij = cbind(tr$X1[isv], tr$X2[isv]), val_Y = tr$Y[isv])
    } else {
      rsm_construct(tr$X1[fit], tr$X2[fit], tr$Y[fit], m, n, s,
                    length(fit) / (m * n), lam_f, W = W, branch = 'ssa',
                    init_iters = k_try, step0 = base * mu, patience = 0L,
                    val_ij = cbind(tr$X1[isv], tr$X2[isv]), val_Y = tr$Y[isv])
    }
    tail(con$val_hist, 1)
  }, numeric(1))
  i <- which.min(v)
  list(step = base * mult[i], mult = mult[i], base = base, vals = v)
}

base_step <- if (dataset == 'netflix') 20 else 2

TREF <- as.integer(Sys.getenv('REAL_TREF', unset = '100'))
CAP  <- as.integer(Sys.getenv('REAL_CAP',  unset = '1500'))
PAT  <- as.integer(Sys.getenv('REAL_PATIENCE', unset = '10'))
HET_PILOT_ITERS <- as.integer(Sys.getenv('HET_PILOT_ITERS', unset = '300'))
AUTO_STEP <- identical(Sys.getenv('REAL_SSA_STEP', unset = ''), 'auto')
AUTO_FRAC <- as.numeric(Sys.getenv('REAL_AUTO_FRAC', unset = '0.5'))
SSA_STEP <- local({
  z <- Sys.getenv('REAL_SSA_STEP', unset = '')
  if (nzchar(z) && z != 'auto') as.numeric(z) else NULL
})
USED_STEP <- SSA_STEP
REF_SEED <- 3000L + K

ref_cache <- file.path(THIS, 'cache', sprintf('%s_case%d_lambda_hat.rds', dataset, case))

s_val <- NA_integer_; lam_val <- NA_real_
if (method == 'rsm_nuc' || method == 'hetW_nuc') {
  sp <- strsplit(param, ':', fixed = TRUE)[[1]]
  s_val <- as.integer(sp[[1]]); lam_val <- as.numeric(sp[[2]])
} else if (method == 'het_ref') {
  s_val <- as.integer(param)
} else if (grepl('_fac$', method)) {
  s_val <- as.integer(param)
} else if (grepl('_nuc$', method)) {
  lam_val <- as.numeric(param)
}
het_cache <- file.path(THIS, 'cache',
                       sprintf('%s_case%d_het_s%s.rds', dataset, case,
                               if (is.na(s_val)) 'NA' else s_val))
key <- gsub('[^0-9A-Za-z.-]+', '_', param)
if (AUTO_STEP) key <- paste0(key, '_auto')
if (!is.null(SSA_STEP)) key <- paste0(key, '_st', SSA_STEP)
out_file <- file.path(outdir, sprintf('%s_case%d_%s.csv', method, case, key))

t0 <- Sys.time()
status <- 'OK'; err <- ''
k_best <- NA_integer_
het_sig <- NA_real_
M_hat <- NULL

res <- tryCatch({
  if (method == 'ref') {
    dir.create(dirname(ref_cache), showWarnings = FALSE, recursive = TRUE)
    lam <- rsm_reference(tr$X1, tr$X2, m, n, T = TREF)
    saveRDS(lam, ref_cache)
    M_hat <- NULL
    NULL
  } else if (method == 'base_fac') {
    set.seed(20000L + 97L * case + s_val)
    M_hat <- Matrix_factor_func(tr$X1, tr$X2, tr$Y, K, m, n,
                                lambda = 0, r = s_val, itertime = 30000, tor = 5e-4,
                                init = TRUE, M_input = rank_s_init(s_val),
                                step_size = base_step, penalty = 'None')
  } else if (method == 'rsm_fac') {
    if (!file.exists(ref_cache)) stop('missing reference cache; run the ref job first: ', ref_cache)
    lam <- readRDS(ref_cache)
    if (AUTO_STEP) USED_STEP <- choose_step(lam, s_val)$step
    M_hat <- Matrix_factor_rsm_val(tr$X1, tr$X2, tr$Y, K, m, n, s = s_val, rho = rho,
                                   lambda_hat = lam, T_ref = TREF, cap = CAP,
                                   patience = PAT, ref_seed = REF_SEED,
                                   ssa_step = USED_STEP)
  } else if (method == 'base_nuc') {
    set.seed(40000L + 97L * case + round(lam_val))
    M_hat <- Nuclear_opt_func(tr$X1, tr$X2, tr$Y, K, m, n,
                              lambda = lam_val, stepsize = 0.2,
                              itertime = 30000, tor = 5e-4,
                              init = TRUE, M_input = M_train / rho)
  } else if (method == 'het_ref') {
    if (!file.exists(ref_cache)) stop('missing reference cache; run the ref job first: ', ref_cache)
    lam <- readRDS(ref_cache)
    USED_STEP <- if (is.null(SSA_STEP)) choose_step(lam, s_val)$step else SSA_STEP
    pilot <- Matrix_factor_rsm(tr$X1, tr$X2, tr$Y, K, m, n, branch = 'ssa',
                               s = s_val, rho = rho, lambda_hat = lam,
                               init_iters = HET_PILOT_ITERS,
                               ssa_step = USED_STEP)
    if (!all(is.finite(pilot))) stop('non-finite pilot')
    res <- tr$Y - pilot[cbind(tr$X1, tr$X2)]
    sig <- sqrt(mean(res^2))
    het_sig <<- sig
    tau <- rsm_estimate_tau(tr$X1, tr$X2, res, m, n)
    dir.create(dirname(het_cache), showWarnings = FALSE, recursive = TRUE)
    saveRDS(list(s = s_val, step = USED_STEP, sigma = sig, tau = tau,
                 pilot_obs_rmse = sig), het_cache)
    M_hat <- NULL
  } else if (method == 'hetW_fac') {
    if (!file.exists(het_cache)) stop('missing whitened pilot cache; run the het_ref job first: ', het_cache)
    if (!file.exists(ref_cache)) stop('missing reference cache; run the ref job first: ', ref_cache)
    lam <- readRDS(ref_cache); hz <- readRDS(het_cache)
    W_hat <- 1 / hz$tau
    USED_STEP <- if (is.null(SSA_STEP)) choose_step(lam, s_val, W = W_hat)$step else SSA_STEP
    M_hat <- Matrix_factor_rsm_val(tr$X1, tr$X2, tr$Y, K, m, n, s = s_val, rho = rho,
                                   lambda_hat = lam, W = W_hat, T_ref = TREF,
                                   cap = CAP, patience = PAT, ref_seed = REF_SEED,
                                   step0 = USED_STEP)
  } else if (method == 'hetW_nuc') {
    if (!file.exists(het_cache)) stop('missing whitened pilot cache; run the het_ref job first: ', het_cache)
    if (!file.exists(ref_cache)) stop('missing reference cache; run the ref job first: ', ref_cache)
    lam <- readRDS(ref_cache); hz <- readRDS(het_cache)
    W_hat <- 1 / hz$tau
    USED_STEP <- if (is.null(SSA_STEP)) choose_step(lam, s_val, W = W_hat)$step else SSA_STEP
    M_hat <- Nuclear_opt_rsm_val(tr$X1, tr$X2, tr$Y, K, m, n, lambda = lam_val,
                                 rho = rho, s = s_val, lambda_hat = lam,
                                 W = W_hat, T_ref = TREF, cap = CAP, patience = PAT,
                                 ref_seed = REF_SEED, step0 = USED_STEP)
  } else if (method == 'rsm_nuc') {
    if (!file.exists(ref_cache)) stop('missing reference cache; run the ref job first: ', ref_cache)
    lam <- readRDS(ref_cache)
    if (AUTO_STEP) USED_STEP <- choose_step(lam, s_val)$step
    M_hat <- Nuclear_opt_rsm_val(tr$X1, tr$X2, tr$Y, K, m, n, lambda = lam_val,
                                 rho = rho, s = s_val, lambda_hat = lam,
                                 T_ref = TREF, cap = CAP, patience = PAT,
                                 ref_seed = REF_SEED, ssa_step = USED_STEP)
  } else {
    stop('unknown method: ', method)
  }
  if (!is.null(M_hat)) {
    if (!all(is.finite(M_hat))) stop('non-finite estimate')
    kb <- attr(M_hat, 'k_best')
    k_best <- if (is.null(kb) || length(kb) == 0L) NA_integer_ else as.integer(kb)
  }
  NULL
}, error = function(e) {
  status <<- 'ERROR'; err <<- conditionMessage(e); M_hat <<- NULL; NULL
})

elapsed <- as.numeric(difftime(Sys.time(), t0, units = 'secs'))
scale_test <- sqrt(mean(te$Y^2))

row <- data.frame(
  dataset = dataset, case = case, method = method, param = param,
  step = if (is.null(USED_STEP)) NA_real_ else USED_STEP,
  s = s_val, lambda = lam_val, k_best = k_best,
  val_rmse = NA_real_, val_max = NA_real_,
  test_rmse = NA_real_, test_max = NA_real_,
  val_rmse_mod = NA_real_, val_max_mod = NA_real_,
  test_rmse_mod = NA_real_, test_max_mod = NA_real_,
  scale_test = scale_test, rel_test = NA_real_, rel_test_mod = NA_real_,
  elapsed_s = elapsed, status = status, message = err,
  stringsAsFactors = FALSE)

if (status == 'OK' && !is.null(M_hat)) {
  mt <- metrics(M_hat)
  row[names(mt)] <- as.list(mt)
  row$rel_test     <- row$test_rmse / scale_test
  row$rel_test_mod <- row$test_rmse_mod / scale_test
} else if (status == 'OK' && method == 'het_ref') {
  row$val_rmse <- het_sig
}
cat(sprintf('%s %s case%d %s: %s  val=%.6f test=%.6f (%.1fs) %s\n',
            dataset, method, case, param, status,
            row$val_rmse, row$test_rmse, elapsed, err))

write.csv(row, out_file, row.names = FALSE)
if (status != 'OK') quit(status = 1L)
