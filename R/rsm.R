# Residual Spectral Matching (RSM): the estimators of Section 4.3.

# 0. Singular value decomposition with retry
svd_safe <- function(x, nu = min(dim(x)), nv = min(dim(x))) {
  r <- tryCatch(base::svd(x, nu = nu, nv = nv), error = function(e) e)
  if (!inherits(r, 'error')) return(r)
  r <- tryCatch(base::svd(x, nu = nu, nv = nv), error = function(e) e)
  if (!inherits(r, 'error')) return(r)
  ev <- eigen(crossprod(x), symmetric = TRUE)
  d  <- sqrt(pmax(ev$values, 0))
  V  <- ev$vectors
  keep <- d > max(d) * 1e-8
  u <- if (nu == 0) matrix(0, nrow(x), 0L) else {
    U <- matrix(0, nrow(x), length(d))
    U[, keep] <- x %*% V[, keep, drop = FALSE] %*% diag(1 / d[keep], sum(keep), sum(keep))
    U[, seq_len(min(nu, ncol(U))), drop = FALSE]
  }
  v <- if (nv == 0) matrix(0, ncol(x), 0L) else V[, seq_len(min(nv, ncol(V))), drop = FALSE]
  list(d = d, u = u, v = v)
}

# 1. Observation mask and same-mask Monte-Carlo reference spectrum

rsm_mask <- function(X1, X2, p, q) {
  Om <- matrix(FALSE, p, q)
  Om[cbind(X1, X2)] <- TRUE
  Om
}

rsm_reference <- function(X1, X2, p, q, T = 100, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  nmin <- min(p, q)
  ij <- cbind(X1, X2)
  D <- matrix(0, nmin, T)
  for (t in seq_len(T)) {
    G <- matrix(rnorm(p * q), p, q)
    Gm <- matrix(0, p, q)
    Gm[ij] <- G[ij]
    D[, t] <- svd_safe(Gm, nu = 0, nv = 0)$d[seq_len(nmin)]
  }
  rowMeans(D) / sqrt(p)
}

# Same-mask reference with entrywise noise scale index_r (times index_c).
rsm_reference_hetero <- function(X1, X2, p, q, index_r, index_c = NULL,
                                T = 100, seed = NULL, normalize = TRUE) {
  if (!is.null(seed)) set.seed(seed)
  nmin <- min(p, q)
  ij <- cbind(X1, X2)
  scl <- if (is.null(index_c)) index_r else index_r %o% index_c
  D <- matrix(0, nmin, T)
  for (t in seq_len(T)) {
    G <- matrix(rnorm(p * q), p, q) * scl
    Gm <- matrix(0, p, q)
    Gm[ij] <- G[ij]
    D[, t] <- svd_safe(Gm, nu = 0, nv = 0)$d[seq_len(nmin)]
  }
  rowMeans(D) / if (normalize) sqrt(p) else 1
}

rsm_I <- function(nmin) seq.int(ceiling(nmin / 3), floor(2 * nmin / 3))

rsm_sigma_hat <- function(d, lambda_hat, m) {
  I <- rsm_I(length(lambda_hat))
  sum(d[I]) / (sqrt(m) * sum(lambda_hat[I]))
}

# 1b. Entrywise noise scale from an observed residual.

rsm_estimate_tau <- function(X1, X2, res, p, q, iters = 5, floor_q = 0.25) {
  # Debiasing for log|residual|: if eps ~ N(0, tau^2) then
  #   E[log|eps|] = log(tau) - (log 2 + gamma) / 2,
  # so adding (log 2 + gamma) / 2 (~0.6352) makes log|res| an unbiased
  # estimate of log(tau), which the additive model below then fits.
  g <- 0.5772156649  # Euler-Mascheroni constant gamma = -digamma(1)
  ly <- log(pmax(abs(res), 1e-10)) + (log(2) + g) / 2
  a <- numeric(p); b <- numeric(q)
  for (it in seq_len(iters)) {
    tb <- tapply(ly, X2, median)
    b <- numeric(q); b[as.integer(names(tb))] <- tb
    b <- b - median(b)
    ta <- tapply(ly - b[X2], X1, median)
    a <- numeric(p); a[as.integer(names(ta))] <- ta
    a <- a - median(a)
  }
  mu <- median(ly - a[X1] - b[X2])
  tau <- exp(outer(a, b, "+") + mu)
  floor_v <- quantile(tau, floor_q, names = FALSE)
  pmax(tau, max(floor_v, 1e-12))
}


# 2. Building blocks of the Section 4.3 construction

rsm_proj_B <- function(A, Bnd, p, q) {
  rn <- sqrt(rowSums(A * A))
  cn <- sqrt(colSums(A * A))
  da <- pmin(1, Bnd / (sqrt(p) * rn)); da[rn <= 0] <- 1
  db <- pmin(1, Bnd / (sqrt(q) * cn)); db[cn <= 0] <- 1
  da[!is.finite(da)] <- 1; db[!is.finite(db)] <- 1
  A * tcrossprod(da, db)
}

rsm_bal <- function(M, s) {
  sv <- svd_safe(M)
  k <- min(s, sum(sv$d > 1e-12))
  L <- matrix(0, nrow(M), s)
  R <- matrix(0, ncol(M), s)
  if (k > 0) {
    dk <- sqrt(sv$d[seq_len(k)])
    L[, seq_len(k)] <- sv$u[, seq_len(k), drop = FALSE] %*% diag(dk, k)
    R[, seq_len(k)] <- sv$v[, seq_len(k), drop = FALSE] %*% diag(dk, k)
  }
  list(L = L, R = R)
}

rsm_direction <- function(G, L, R, s, gamma = 1e-8) {
  DL <- -G %*% R %*% chol2inv(chol(crossprod(R) + gamma * diag(s)))
  DR <- -t(G) %*% L %*% chol2inv(chol(crossprod(L) + gamma * diag(s)))
  list(DL = DL, DR = DR,
       ddir = -sum(G * (DL %*% t(R) + L %*% t(DR))))
}


# 3. Section 4.3 construction of M^(0),...,M^(k0)

rsm_construct <- function(X1, X2, Y, p, q, s, rho, lambda_hat,
                          W = NULL, branch = "ssa", mu0 = 1, K0 = 5, t0 = 0.1,
                          C_Q = 1, C_stop = 0, gamma = 1e-8, step0 = NULL,
                          ssa_step = NULL, init_iters = 100,
                          Mb_init = NULL, verbose = FALSE,
                          val_ij = NULL, val_Y = NULL, patience = 10L) {
  ij <- cbind(X1, X2)
  Ymat <- matrix(0, p, q); Ymat[ij] <- Y
  if (is.null(W)) W <- matrix(1, p, q)
  Om <- rsm_mask(X1, X2, p, q)

  sig0  <- rsm_sigma_hat(svd_safe(Ymat, nu = 0, nv = 0)$d, lambda_hat, p)
  h_raw <- sig0 * sqrt(p / rho)

  Z   <- Ymat / rho
  svZ <- svd_safe(Z)
  s_eff <- min(s, sum(svZ$d > 1e-10))
  Zs  <- svZ$u[, seq_len(s_eff), drop = FALSE] %*%
         (svZ$d[seq_len(s_eff)] * t(svZ$v[, seq_len(s_eff), drop = FALSE]))

  Bnd <- K0 * sqrt(mu0 * s) * svZ$d[1]
  Mb  <- rsm_proj_B(Zs, Bnd, p, q)
  if (!is.null(Mb_init)) Mb <- Mb_init

  if (is.null(step0)) {
    if (all(W == 1)) {
      step0 <- if (is.null(ssa_step)) 0.1 * rho else ssa_step
    } else {
      nEw <- norm(W * (Ymat - Mb * Om), "F")
      nE  <- norm(Ymat - Mb * Om, "F")
      step0 <- if (!is.finite(nEw) || nEw <= 0) 0.5 else min(0.5, 0.5 * nE / nEw)
    }
  }
  if (verbose) cat(sprintf("[rsm_construct] step0=%.4f\n", step0))

  use_ht <- switch(branch,
    ht   = TRUE,
    ssa  = FALSE,
    auto = sqrt(s) * h_raw >= t0 * svZ$d[s_eff])

  if (use_ht) {
    svM  <- svd_safe(Mb)
    keep <- which(svM$d >= svZ$d[s_eff] / 4)
    k    <- max(1L, min(length(keep), s_eff))
    M0   <- svM$u[, seq_len(k), drop = FALSE] %*%
            (svM$d[seq_len(k)] * t(svM$v[, seq_len(k), drop = FALSE]))
    k0 <- 0L
    br <- "ht"
  } else {
    M  <- Mb
    k0 <- 0L
    k_best <- 0L; v_best <- Inf; val_hist <- numeric(0)
    for (k in seq_len(init_iters)) {
      M_prev <- M
      Ew   <- W * (Ymat - M * Om)
      svE  <- svd_safe(Ew)
      sig  <- rsm_sigma_hat(svE$d, lambda_hat, p)
      b    <- sqrt(p) * sig
      Q    <- b * (svE$u %*% (lambda_hat * t(svE$v)))
      PQw  <- (W * Q) * Om
      nq   <- norm(PQw, "F")
      alpha <- if (nq <= 0) 1 else min(1, C_Q * sig * sqrt(p * rho) / nq)
      G    <- (-Ew + alpha * PQw) / rho
      bal <- rsm_bal(M, s)
      L <- bal$L; R <- bal$R
      dir <- rsm_direction(G, L, R, s, gamma)
      if (!is.finite(dir$ddir) || dir$ddir <= 0) break
      M <- (L + step0 * dir$DL) %*% t(R + step0 * dir$DR)
      bal <- rsm_bal(M, s); M <- bal$L %*% t(bal$R)
      M <- rsm_proj_B(M, Bnd, p, q)
      k0 <- k
      if (!is.null(val_ij)) {
        v <- sqrt(mean((val_Y - M[val_ij])^2))
        val_hist <- c(val_hist, v)
        if (is.finite(v) && v < v_best) { v_best <- v; k_best <- k }
        else if (patience > 0 && k - k_best >= patience) break
      }
      if (C_stop > 0 &&
          norm(M - M_prev, "F") <=
            C_stop * sqrt(s) * sig * sqrt(p / rho)) break
    }
    M0 <- M
    br <- "ssa"
  }
  if (use_ht) { k_best <- 0L; val_hist <- numeric(0) }

  sig_pil <- rsm_sigma_hat(svd_safe(W * (Ymat - M0 * Om), nu = 0, nv = 0)$d,
                          lambda_hat, p)
  if (verbose) {
    cat(sprintf("[rsm_construct] branch=%s k0=%d sigma_pil=%.4f\n",
                br, k0, sig_pil))
  }
  list(M0 = M0, k0 = k0, sigma_pil = sig_pil, branch = br,
      Bnd = Bnd, s_eff = s_eff, k_best = k_best, val_hist = val_hist)
}


# 4. RSM nonconvex factor estimator.

Matrix_factor_rsm <- function(X1, X2, Y, n, p, q, lambda2 = 1, s, rho,
                              lambda_hat = NULL, W = NULL, T_ref = 100,
                              branch = "ssa",
                              mu0 = 1, K0 = 5, t0 = 0.1, C_Q = 1, C_stop = 0,
                              gamma = 1e-8, init_iters = 100, step0 = NULL,
                              ssa_step = NULL,
                              alg1_iters = 0, tor = 1e-6, t_init = 1e6,
                              Mb_init = NULL, verbose = FALSE) {
  ij   <- cbind(X1, X2)
  Om   <- rsm_mask(X1, X2, p, q)
  Ymat <- matrix(0, p, q); Ymat[ij] <- Y
  nmin <- min(p, q)
  if (is.null(W)) W <- matrix(1, p, q)
  if (is.null(lambda_hat)) lambda_hat <- rsm_reference(X1, X2, p, q, T_ref)
  omega <- rep(1 / nmin, nmin)

  con <- rsm_construct(X1, X2, Y, p, q, s, rho, lambda_hat, W = W,
                      branch = branch,
                      mu0 = mu0, K0 = K0, t0 = t0, C_Q = C_Q,
                      C_stop = C_stop, gamma = gamma, step0 = step0,
                      ssa_step = ssa_step,
                      init_iters = init_iters, Mb_init = Mb_init, verbose = verbose)
  M <- con$M0
  if (alg1_iters <= 0) return(M)

  # ---- Algorithm 1 refinement ----
  bal <- rsm_bal(M, s); L <- bal$L; R <- bal$R; M <- L %*% t(R)
  b_fix <- sqrt(p) * con$sigma_pil
  loss_fun <- function(Mm) {
    d <- svd_safe(W * (Ymat - Mm * Om), nu = 0, nv = 0)$d[seq_len(nmin)]
    sum(omega * ((d - b_fix * lambda_hat) / b_fix)^2) / rho
  }
  obj_before <- Inf; tstep <- t_init
  for (iter in seq_len(alg1_iters)) {
    Ew  <- W * (Ymat - M * Om)
    svE <- svd_safe(Ew)
    a   <- (2 / (rho * b_fix)) * omega *
           (svE$d[seq_len(nmin)] - b_fix * lambda_hat) / b_fix
    G   <- -(svE$u[, seq_len(nmin), drop = FALSE] %*%
             (a * t(svE$v[, seq_len(nmin), drop = FALSE])))
    G   <- (W * G) * Om
    dir <- rsm_direction(G, L, R, s, gamma)
    if (!is.finite(dir$ddir) || dir$ddir <= 0) break

    Lc <- loss_fun(M); accepted <- FALSE
    for (bt in seq_len(200)) {
      Mt <- (L + tstep * dir$DL) %*% t(R + tstep * dir$DR)
      if (loss_fun(Mt) <= Lc - 1e-4 * tstep * dir$ddir) {
        accepted <- TRUE; break
      }
      tstep <- tstep / 2
    }
    if (!accepted) break
    M <- Mt
    bal <- rsm_bal(M, s); L <- bal$L; R <- bal$R; M <- L %*% t(R)
    tstep <- min(tstep * 2, 1e12)
    if (abs(Lc - obj_before) < tor) break
    obj_before <- Lc
  }
  M
}


# 4b. Two-stage estimator with held-out-validation stopping.

Matrix_factor_rsm_val <- function(X1, X2, Y, n, p, q, s, rho,
                                  lambda_hat = NULL, W = NULL, T_ref = 100,
                                  vfrac = 0.1, cap = 1000L, patience = 10L,
                                  ref_seed = NULL, lambda2 = 1,
                                  mu0 = 1, K0 = 5, t0 = 0.1, C_Q = 1,
                                  gamma = 1e-8, step0 = NULL, ssa_step = NULL,
                                  alg1_iters = 0, tor = 1e-6, t_init = 1e6,
                                  Mb_init = NULL) {
  K    <- length(X1)
  step <- max(1L, round(1 / vfrac))
  isv  <- seq.int(step, K, by = step)
  fit  <- setdiff(seq_len(K), isv)
  X1f <- X1[fit]; X2f <- X2[fit]; Yf <- Y[fit]
  X1v <- X1[isv]; X2v <- X2[isv]; Yv <- Y[isv]
  rho_f <- length(fit) / (p * q)

  if (is.null(lambda_hat)) lambda_hat <- rsm_reference(X1, X2, p, q, T_ref)
  if (is.null(ref_seed)) ref_seed <- 3000L + K
  old <- if (exists(".Random.seed", .GlobalEnv)) get(".Random.seed", .GlobalEnv)
  lam_f <- rsm_reference(X1f, X2f, p, q, T_ref, seed = ref_seed)
  if (!is.null(old)) assign(".Random.seed", old, .GlobalEnv)

  # ---- stage 1 ----
  con <- rsm_construct(X1f, X2f, Yf, p, q, s, rho_f, lam_f, W = W,
                      branch = "ssa", mu0 = mu0, K0 = K0, t0 = t0,
                      C_Q = C_Q, gamma = gamma, step0 = step0,
                      ssa_step = ssa_step, init_iters = cap,
                      Mb_init = Mb_init,
                      val_ij = cbind(X1v, X2v), val_Y = Yv,
                      patience = patience)
  k <- con$k_best
  if (!is.finite(k) || k < 1L) k <- 1L

  # ---- stage 2 ----
  M <- Matrix_factor_rsm(X1, X2, Y, n, p, q, lambda2 = lambda2, s = s,
                        rho = rho, lambda_hat = lambda_hat, W = W,
                        T_ref = T_ref, branch = "ssa",
                        mu0 = mu0, K0 = K0, t0 = t0, C_Q = C_Q,
                        gamma = gamma, init_iters = k, step0 = step0,
                        ssa_step = ssa_step, alg1_iters = alg1_iters,
                        tor = tor, t_init = t_init, Mb_init = Mb_init)
  attr(M, "k_best") <- k
  M
}


# 5. RSM nuclear-norm estimator.

Nuclear_opt_rsm <- function(X1, X2, Y, n_t, p, q, lambda, rho, s = NULL,
                            lambda_hat = NULL, W = NULL, T_ref = 100,
                            branch = "ssa",
                            mu0 = 1, K0 = 5, t0 = 0.1, C_Q = 1, C_stop = 0,
                            gamma = 1e-8, init_iters = 100, step0 = NULL,
                            ssa_step = NULL, svt_iters = 200,
                            tor = 1e-6, t_init = 1e6, Mb_init = NULL,
                            verbose = FALSE) {
  ij   <- cbind(X1, X2)
  Om   <- rsm_mask(X1, X2, p, q)
  Ymat <- matrix(0, p, q); Ymat[ij] <- Y
  nmin <- min(p, q)
  if (is.null(s)) s <- nmin
  if (is.null(W)) W <- matrix(1, p, q)
  if (is.null(lambda_hat)) lambda_hat <- rsm_reference(X1, X2, p, q, T_ref)
  omega <- rep(1 / nmin, nmin)

  con <- rsm_construct(X1, X2, Y, p, q, s, rho, lambda_hat, W = W,
                      branch = branch,
                      mu0 = mu0, K0 = K0, t0 = t0, C_Q = C_Q,
                      C_stop = C_stop, gamma = gamma, step0 = step0,
                      ssa_step = ssa_step,
                      init_iters = init_iters, Mb_init = Mb_init, verbose = verbose)
  M <- con$M0
  if (svt_iters <= 0) return(M)

  b_fix <- sqrt(p) * con$sigma_pil
  loss_fun <- function(Mm) {
    d <- svd_safe(W * (Ymat - Mm * Om), nu = 0, nv = 0)$d[seq_len(nmin)]
    sum(omega * ((d - b_fix * lambda_hat) / b_fix)^2) / rho
  }
  svt <- function(A, thr) {
    sv <- svd_safe(A)
    ds <- pmax(sv$d - thr, 0)
    sv$u %*% (ds * t(sv$v))
  }

  obj_before <- Inf; tstep <- t_init
  for (iter in seq_len(svt_iters)) {
    Ew  <- W * (Ymat - M * Om)
    svE <- svd_safe(Ew)
    a   <- (2 / (rho * b_fix)) * omega *
           (svE$d[seq_len(nmin)] - b_fix * lambda_hat) / b_fix
    g   <- -(svE$u[, seq_len(nmin), drop = FALSE] %*%
             (a * t(svE$v[, seq_len(nmin), drop = FALSE])))
    g   <- (W * g) * Om

    Lc <- loss_fun(M) + lambda * sum(svd_safe(M, nu = 0, nv = 0)$d)
    accepted <- FALSE
    for (bt in seq_len(200)) {
      Z   <- svt(M - tstep * g, tstep * lambda)
      lhs <- loss_fun(Z) + lambda * sum(svd_safe(Z, nu = 0, nv = 0)$d)
      if (is.finite(lhs) && lhs <= Lc - 1e-6 * tstep * sum(g^2)) {
        accepted <- TRUE; break
      }
      tstep <- tstep / 2
    }
    if (!accepted) break
    M <- Z
    tstep <- min(tstep * 2, 1e12)
    if (abs(Lc - obj_before) < tor) break
    obj_before <- Lc
  }
  M
}


# 4c. Held-out-validation stopping for the nuclear-norm arm.

Nuclear_opt_rsm_val <- function(X1, X2, Y, n_t, p, q, lambda, rho, s = NULL,
                                lambda_hat = NULL, W = NULL, T_ref = 100,
                                vfrac = 0.1, cap = 1000L, patience = 10L,
                                ref_seed = NULL,
                                mu0 = 1, K0 = 5, t0 = 0.1, C_Q = 1,
                                gamma = 1e-8, step0 = NULL, ssa_step = NULL,
                                svt_iters = 100, tor = 1e-6, t_init = 1e6,
                                Mb_init = NULL) {
  K    <- length(X1)
  step <- max(1L, round(1 / vfrac))
  isv  <- seq.int(step, K, by = step)
  fit  <- setdiff(seq_len(K), isv)
  X1f <- X1[fit]; X2f <- X2[fit]; Yf <- Y[fit]
  X1v <- X1[isv]; X2v <- X2[isv]; Yv <- Y[isv]
  rho_f <- length(fit) / (p * q)

  if (is.null(lambda_hat)) lambda_hat <- rsm_reference(X1, X2, p, q, T_ref)
  if (is.null(s)) s <- min(p, q)
  if (is.null(ref_seed)) ref_seed <- 3000L + K
  old <- if (exists(".Random.seed", .GlobalEnv)) get(".Random.seed", .GlobalEnv)
  lam_f <- rsm_reference(X1f, X2f, p, q, T_ref, seed = ref_seed)
  if (!is.null(old)) assign(".Random.seed", old, .GlobalEnv)

  con <- rsm_construct(X1f, X2f, Yf, p, q, s, rho_f, lam_f, W = W,
                      branch = "ssa", mu0 = mu0, K0 = K0, t0 = t0,
                      C_Q = C_Q, gamma = gamma, step0 = step0,
                      ssa_step = ssa_step, init_iters = cap,
                      Mb_init = Mb_init,
                      val_ij = cbind(X1v, X2v), val_Y = Yv,
                      patience = patience)
  k <- con$k_best
  if (!is.finite(k) || k < 1L) k <- 1L

  M <- Nuclear_opt_rsm(X1, X2, Y, n_t, p, q, lambda, rho, s = s,
                      lambda_hat = lambda_hat, W = W, T_ref = T_ref,
                      branch = "ssa", mu0 = mu0, K0 = K0, t0 = t0, C_Q = C_Q,
                      gamma = gamma, init_iters = k, step0 = step0,
                      ssa_step = ssa_step, svt_iters = svt_iters,
                      tor = tor, t_init = t_init, Mb_init = Mb_init)
  attr(M, "k_best") <- k
  M
}
