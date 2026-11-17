# Optional compiled core (Rcpp / RcppArmadillo)

`rsm_core.cpp` is a C++ implementation of the inner loops of the estimators in
[`../R/rsm.R`](../R/rsm.R). It is **optional**: every script in `simulation/`
and `real_data/` calls the base-R implementation, so none of the paper's tables
need a compiler. The compiled core exists for users who want the same fits
faster.

## Speed

About **3.2–3.3x end-to-end** relative to the base-R estimators for the
factorization arm, and **2.7–2.9x** for the nuclear-norm arm, homoscedastic or
whitened, on the simulation cell sizes. The speedup is not bought by loosening
the algorithm: the selected stopping point `k*` is identical. The factorization
fits agree with the base-R one to rounding (about `1e-10`); the nuclear fits
agree to about `1e-7`, the level at which the published SVT refinement itself
stops moving.

The savings are structural and exact in exact arithmetic:

1. the balancing SVD of the current estimate is taken from its factors
   (`M = A B'`, `A` is `p x s`, `B` is `q x s`, so the SVD is of an `s x s`
   core) instead of a dense `p x q` SVD, and it is computed once per iteration
   rather than twice;
2. the estimate is never materialised off `Omega`: the residual, the score term
   `Q` and the gradient `G` are only needed on the observed positions;
3. `Q = b * U diag(lambda_hat) V'` is evaluated on `Omega` only —
   `O(K * nmin)` instead of `O(p * q * nmin)`;
4. the directional derivative uses
   `sum(G * (DL R')) = sum(DL * (G R))`, which is `O(K * s)` instead of
   `O(p * q * s)`.

The only remaining `O(p * q * min(p, q))` step per iteration is the one
unavoidable singular value decomposition of the residual. A Monte-Carlo
reference spectrum is additionally parallelised over independent draws when
OpenMP is available.

## Requirements and loading

- R with a C++ toolchain;
- the `Rcpp` and `RcppArmadillo` packages.

```r
Rcpp::sourceCpp("cpp/rsm_core.cpp")
```

`sourceCpp` compiles the file and exposes the four kernels below. If OpenMP is
not available the file still compiles and runs, with a serial Monte-Carlo
reference.

## Exported kernels

These are the low-level pieces; the mask split, step-size calibration, penalty
handling and held-out stopping that turn them into the four estimators live in
[`../R/rsm.R`](../R/rsm.R). The signatures are those of the
`// [[Rcpp::export]]` declarations in `rsm_core.cpp`.

| Function | Role |
|----------|------|
| `rsm_ssa_loop_cpp(Xi, Xj, Y, p, q, s, rho, lambda_hat, W, Mb, Bnd, step0, C_Q, gamma, init_iters, val_i, val_j, val_y, patience, C_stop, exact_bal)` | one SSA loop on one mask (factorization estimator) |
| `rsm_svt_one_cpp(Xi, Xj, Y, p, q, lambda, rho, lambda_hat, W, M0, sigma_pil, svt_iters, tor, t_init, trace)` | one singular-value-thresholding pass (nuclear-norm estimator) |
| `rsm_alg1_cpp(Xi, Xj, Y, p, q, s, rho, lambda_hat, W, M_in, sigma_pil, alg1_iters, gamma, tor, t_init)` | optional Algorithm-1 factor refinement (off by default) |
| `rsm_ref_cpp(draws, Xi, Xj, p, q, nthreads)` | the same-mask Monte-Carlo reference spectrum |

`Xi`, `Xj`, `Y` are the observed coordinates and values (`1`-based), `W` is the
optional entrywise weight matrix (`NULL` for the homoscedastic case), and `Mb`
is the current `p x q` estimate.

## Scope

This folder does not contain an R package or a public estimator API. It is the
compiled core only. The standalone `rsm` package (kept separately from this
release) wraps the same kernels in `rsm_fac()`, `rsm_nuc()`, `rsm_fac_het()`
and `rsm_nuc_het()`; the base-R counterparts of those fits are the functions
documented in Section 1.1 of the top-level README.

## License

MIT, the same license as the rest of this release; see
[`../LICENSE`](../LICENSE).
