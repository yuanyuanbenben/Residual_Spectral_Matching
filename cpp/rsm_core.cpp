// ============================================================================
// Optional compiled core of the residual spectral matching estimators
// (Rcpp / RcppArmadillo).
//
// The base-R estimators in ../R/rsm.R are the reference: every routine below
// reproduces the same arithmetic, with four structural savings that are exact
// in exact arithmetic (see README.md in this folder for the structural savings
// and the measured speedup):
//
//   1. the balancing SVD of the current estimate is taken from its FACTORS
//      (M = A B' with A p x s, B q x s -> svd of an s x s core) instead of a
//      dense p x q SVD, and it is computed once per iteration rather than twice;
//   2. the estimate is never materialised off Omega; the residual, the score
//      term Q and the gradient G only ever need their values on the observed
//      positions;
//   3. Q = b * U diag(lambda_hat) V' is evaluated on Omega only
//      (O(K * nmin) instead of O(p * q * nmin));
//   4. the directional derivative is evaluated through the identity
//      sum(G * (DL R')) = sum(DL * (G R)), which is O(K * s) instead of O(p*q*s).
//
// The only remaining O(p*q*min(p,q)) step per iteration is the one unavoidable
// singular value decomposition of the residual.
// ============================================================================

#include <RcppArmadillo.h>
#include <vector>
#include <algorithm>
#include <cmath>

#ifdef _OPENMP
#include <omp.h>
#endif

// [[Rcpp::depends(RcppArmadillo)]]

using namespace Rcpp;

namespace {

inline double sq(double x) { return x * x; }

// A^{-1} for a symmetric positive definite A, via Cholesky -- the same route
// R's chol2inv(chol(A)) takes.
inline arma::mat spd_inv(const arma::mat& A) {
  arma::mat U;
  if (!arma::chol(U, A)) return arma::pinv(A);
  arma::mat X = arma::solve(arma::trimatu(U), arma::eye(A.n_rows, A.n_cols));
  return X * X.t();
}

// ---------------------------------------------------------------------------
// Observation mask: coordinates, values, weights, and the row/column CSR
// structures used to sweep Omega without touching the zero entries.
// ---------------------------------------------------------------------------
struct Mask {
  int p = 0, q = 0, K = 0;
  std::vector<int> oi, oj;
  std::vector<double> y, w;
  std::vector<int> rp, ro, cp, co;
};

// NULL-tolerant view of the optional p x q weight matrix.  Constructing a
// NumericMatrix from the underlying SEXP wraps it, it does not copy it.
struct Weights {
  bool has = false;
  NumericMatrix m;
};

inline Weights get_weights(const Nullable<NumericMatrix>& W) {
  Weights out;
  if (!W.isNull()) { out.m = NumericMatrix(W.get()); out.has = true; }
  return out;
}

Mask make_mask(const IntegerVector& Xi, const IntegerVector& Xj,
               const NumericVector& Y, const Weights& W, int p, int q) {
  Mask m; m.p = p; m.q = q; m.K = Xi.size();
  m.oi.resize(m.K); m.oj.resize(m.K); m.y.resize(m.K); m.w.resize(m.K);
  std::vector<int> cr(p, 0), cc(q, 0);
  for (int k = 0; k < m.K; ++k) {
    int i = Xi[k] - 1, j = Xj[k] - 1;
    m.oi[k] = i; m.oj[k] = j; m.y[k] = Y[k];
    m.w[k] = W.has ? W.m(i, j) : 1.0;
    ++cr[i]; ++cc[j];
  }
  m.rp.assign(p + 1, 0);
  for (int i = 0; i < p; ++i) m.rp[i + 1] = m.rp[i] + cr[i];
  m.cp.assign(q + 1, 0);
  for (int j = 0; j < q; ++j) m.cp[j + 1] = m.cp[j] + cc[j];
  m.ro.resize(m.K); m.co.resize(m.K);
  std::vector<int> fr(m.rp.begin(), m.rp.end() - 1), fc(m.cp.begin(), m.cp.end() - 1);
  for (int k = 0; k < m.K; ++k) { m.ro[fr[m.oi[k]]++] = k; m.co[fc[m.oj[k]]++] = k; }
  return m;
}

// M_ij for i in [i0, i1) rows of the CSR sweep (used by the two gradient passes).
inline double entry(const double* Lp, const double* Rp, int p, int q, int s,
                    int i, int j) {
  const double* li = Lp + i;
  const double* rj = Rp + j;
  double v = 0;
  for (int t = 0; t < s; ++t) v += li[(size_t)t * p] * rj[(size_t)t * q];
  return v;
}

// ---------------------------------------------------------------------------
// Balancing: M = L R' with L p x s, R q x s, both carrying sqrt(d).
//   bal_dense : rsm_bal() applied to a dense matrix (full SVD, truncated).
//   bal_fact  : the same for M = A B' given its factors -- svd of an s x s core.
// ---------------------------------------------------------------------------
void bal_dense(const arma::mat& M, int s, arma::mat& L, arma::mat& R) {
  arma::mat U, V; arma::vec d;
  arma::svd_econ(U, d, V, M);
  const int nmin = std::min((int)M.n_rows, (int)M.n_cols);
  int k = 0;
  while (k < s && k < nmin && d[k] > 1e-12) ++k;
  L.zeros(M.n_rows, s); R.zeros(M.n_cols, s);
  for (int t = 0; t < k; ++t) {
    const double sd = std::sqrt(d[t]);
    L.col(t) = sd * U.col(t);
    R.col(t) = sd * V.col(t);
  }
}

void bal_fact(const arma::mat& A, const arma::mat& B, int s,
              arma::mat& L, arma::mat& R) {
  arma::mat Q1, R1, Q2, R2;
  arma::qr_econ(Q1, R1, A);
  arma::qr_econ(Q2, R2, B);
  arma::mat C = R1 * R2.t();                       // s x s core of A B'
  arma::mat Uc, Vc; arma::vec d;
  arma::svd_econ(Uc, d, Vc, C);
  int k = 0;
  while (k < s && d[k] > 1e-12) ++k;
  arma::mat U = Q1 * Uc, V = Q2 * Vc;
  L.zeros(A.n_rows, s); R.zeros(B.n_rows, s);
  for (int t = 0; t < k; ++t) {
    const double sd = std::sqrt(d[t]);
    L.col(t) = sd * U.col(t);
    R.col(t) = sd * V.col(t);
  }
}

// P_B applied to the FACTORS: it is a row/column scaling of M, so
// (D_a M D_b) = (D_a L)(D_b R)' and the factorization survives exactly.
//
// Both scaling vectors are read off the UNSCALED factors, as rsm_proj_B does:
//   rowSq_i = ||(L R')_{i,.}||^2 = L_i (R'R) L_i',      H = R'R
//   colSq_j = ||(L R')_{.,j}||^2 = R_j (L'L) R_j',      G = L'L
// Reading G off a row-scaled L would give the column norms of the SCALED
// matrix, i.e. a different map -- that is a real deviation from rsm_proj_B, so
// the scaling is accumulated into two vectors and applied only at the end.
void proj_B_factors(arma::mat& L, arma::mat& R, double Bnd, int p, int q, int s) {
  arma::mat H = R.t() * R;                          // s x s
  arma::mat G = L.t() * L;                          // s x s
  arma::mat Lg = L * H;                             // p x s
  arma::mat Rg = R * G;                             // q x s
  double* lp = L.memptr(); double* lgp = Lg.memptr();
  double* rp_ = R.memptr(); double* rgp = Rg.memptr();
  const double span = std::sqrt((double)p);
  const double snap = std::sqrt((double)q);
  // da/db are kept in two small vectors so that neither factor is touched
  // before both norms have been read.
  std::vector<double> da(p, 1.0), db(q, 1.0);
  for (int i = 0; i < p; ++i) {
    double rs = 0;
    for (int t = 0; t < s; ++t) rs += lp[i + (size_t)t * p] * lgp[i + (size_t)t * p];
    if (rs > 0) { da[i] = std::min(1.0, Bnd / (span * std::sqrt(rs))); }
    if (!std::isfinite(da[i])) da[i] = 1.0;
  }
  for (int j = 0; j < q; ++j) {
    double cs = 0;
    for (int t = 0; t < s; ++t) cs += rp_[j + (size_t)t * q] * rgp[j + (size_t)t * q];
    if (cs > 0) { db[j] = std::min(1.0, Bnd / (snap * std::sqrt(cs))); }
    if (!std::isfinite(db[j])) db[j] = 1.0;
  }
  for (int i = 0; i < p; ++i) {
    if (da[i] == 1.0) continue;
    for (int t = 0; t < s; ++t) lp[i + (size_t)t * p] *= da[i];
  }
  for (int j = 0; j < q; ++j) {
    if (db[j] == 1.0) continue;
    for (int t = 0; t < s; ++t) rp_[j + (size_t)t * q] *= db[j];
  }
}

// ||A B' - C D'||_F^2 from the factors alone.
double frob_diff_factors(const arma::mat& A, const arma::mat& B,
                         const arma::mat& C, const arma::mat& D, int s) {
  arma::mat P(A.n_rows, 2 * s), Q(B.n_rows, 2 * s);
  P.cols(0, s - 1) = A; P.cols(s, 2 * s - 1) = -C;
  Q.cols(0, s - 1) = B; Q.cols(s, 2 * s - 1) = D;
  // ||P Q'||_F^2 = tr(P'P Q'Q) = sum_ij (P'P)_ij (Q'Q)_ij
  return arma::accu((P.t() * P) % (Q.t() * Q));
}

}  // namespace

// ===========================================================================
// 1. Same-mask Monte-Carlo reference spectrum
// ===========================================================================
// `draws` holds one Monte-Carlo draw per COLUMN, each already flattened
// column-major, so that the caller can generate them through R's own RNG
// stream.  Only the singular-value work is parallelised.
// [[Rcpp::export]]
NumericVector rsm_ref_cpp(const NumericMatrix& draws, const IntegerVector& Xi,
                          const IntegerVector& Xj, int p, int q, int nthreads) {
  const int T = draws.ncol();
  const int K = Xi.size();
  const int nmin = std::min(p, q);
  const double* dp = draws.begin();

  std::vector<int> oi(K), oj(K);
  for (int k = 0; k < K; ++k) { oi[k] = Xi[k] - 1; oj[k] = Xj[k] - 1; }

  std::vector<double> acc((size_t)nmin * T, 0.0);
#if defined(_OPENMP)
#pragma omp parallel for schedule(dynamic) num_threads(nthreads > 0 ? nthreads : 1)
#endif
  for (int t = 0; t < T; ++t) {
    arma::mat G(p, q, arma::fill::zeros);
    const double* col = dp + (size_t)t * (size_t)p * (size_t)q;
    for (int k = 0; k < K; ++k)
      G(oi[k], oj[k]) = col[(size_t)oj[k] * p + oi[k]];
    arma::vec sv;
    arma::svd(sv, G);
    for (int r = 0; r < nmin; ++r) acc[(size_t)t * nmin + r] = sv[r];
  }

  NumericVector out(nmin);
  for (int r = 0; r < nmin; ++r) {
    double a = 0;
    for (int t = 0; t < T; ++t) a += acc[(size_t)t * nmin + r];
    out[r] = a / T;
  }
  return out;
}

// ===========================================================================
// 2. One SSA loop on ONE mask
// ===========================================================================
// Direct counterpart of .rsm_ssa_loop_R: same starting point, same step, same
// monitor.  Returns the dense estimate, the iteration count, and (when a
// held-out set is supplied) the argmin index and the held-out curve.
// [[Rcpp::export]]
List rsm_ssa_loop_cpp(IntegerVector Xi, IntegerVector Xj, NumericVector Y,
                      int p, int q, int s, double rho,
                      NumericVector lambda_hat,
                      Nullable<NumericMatrix> W,
                      NumericMatrix Mb,
                      double Bnd, double step0, double C_Q, double gamma,
                      int init_iters,
                      Nullable<IntegerVector> val_i = R_NilValue,
                      Nullable<IntegerVector> val_j = R_NilValue,
                      Nullable<NumericVector> val_y = R_NilValue,
                      int patience = 10, double C_stop = 0.0,
                      bool exact_bal = false) {
  const Weights Wv = get_weights(W);
  Mask m = make_mask(Xi, Xj, Y, Wv, p, q);
  const int K = m.K, nmin = std::min(p, q);

  const bool has_val = !val_i.isNull() && !val_j.isNull() && !val_y.isNull();
  std::vector<int> vi, vj; std::vector<double> vy;
  if (has_val) {
    IntegerVector a = val_i, b = val_j; NumericVector c = val_y;
    vi.assign(a.begin(), a.end()); vj.assign(b.begin(), b.end()); vy.assign(c.begin(), c.end());
  }

  arma::vec lam(lambda_hat.begin(), lambda_hat.size());
  // middle-third index set, as rsm_I()
  const int I0 = (int)std::ceil(nmin / 3.0) - 1;
  const int I1 = (int)std::floor(2.0 * nmin / 3.0) - 1;
  double lam_sum_I = 0;
  for (int t = I0; t <= I1; ++t) lam_sum_I += lam[t];

  arma::mat Md(Mb.begin(), p, q, false, true);
  // The iterate is carried as a pair of factors (L, R), M = L R', so that the
  // rebalancing at the end of each step is an SVD of an s x s core rather than
  // a fresh svd(M).  The INIT is the one iterate that need not be rank s -- the
  // oracle start M_0 + 5*N is full rank -- and the reference loop reads its
  // first residual off that full-rank matrix, before balancing it for the
  // direction.  So `Md` stays available and it == 0 takes the residual from it
  // directly; from it == 1 on, M is rank s by construction and (L, R) is exact.
  arma::mat L, R;                                   // the iterate, as factors
  arma::mat Ew(p, q, arma::fill::zeros);
  arma::mat U, V, GR(p, s, arma::fill::zeros), GL(q, s, arma::fill::zeros);
  arma::mat DL(p, s), DR(q, s), A(p, s), B(q, s), Lb, Rb;
  arma::vec d;
  std::vector<double> mv(K), ev(K), gv(K), pq(K), qk_acc(K);

  int k0 = 0, k_best = 0;
  double v_best = R_PosInf;
  std::vector<double> val_hist;

  for (int it = 0; it < init_iters; ++it) {
    const double* Lp;
    const double* Rp;
    std::fill(mv.begin(), mv.end(), 0.0);
    if (it == 0) {
      bal_dense(Md, s, Lb, Rb);
      Lp = Lb.memptr(); Rp = Rb.memptr();
      for (int k = 0; k < K; ++k)
        mv[k] = Md(m.oi[k], m.oj[k]);
    } else {
      if (exact_bal) { arma::mat Mfull = L * R.t(); bal_dense(Mfull, s, Lb, Rb); }
      else           { bal_fact(L, R, s, Lb, Rb); }
      Lp = Lb.memptr(); Rp = Rb.memptr();
      // Sweep one factor column at a time: the columns of Lb/Rb are contiguous
      // and short (p and q doubles), so the gathered accesses stay in L1, and
      // the accumulation order per entry is still increasing t -- the same
      // order the reference implementation uses.
      for (int t = 0; t < s; ++t) {
        const double* lt = Lp + (size_t)t * p;
        const double* rt = Rp + (size_t)t * q;
        for (int k = 0; k < K; ++k) mv[k] += lt[m.oi[k]] * rt[m.oj[k]];
      }
    }
    std::fill(Ew.memptr(), Ew.memptr() + (size_t)p * q, 0.0);
    double* ep = Ew.memptr();
    for (int k = 0; k < K; ++k) {
      ev[k] = m.w[k] * (m.y[k] - mv[k]);
      ep[(size_t)m.oj[k] * p + m.oi[k]] = ev[k];
    }

    arma::svd_econ(U, d, V, Ew);
    double dsum = 0;
    for (int t = I0; t <= I1; ++t) dsum += d[t];
    const double sig = dsum / (std::sqrt((double)p) * lam_sum_I);
    const double b = std::sqrt((double)p) * sig;

    // ---- score term on Omega -----------------------------------------------
    // Q = b * U diag(lambda_hat) V'; evaluated only where it is consumed.
    const double* Up = U.memptr();
    const double* Vp = V.memptr();
    std::fill(qk_acc.begin(), qk_acc.end(), 0.0);
    for (int t = 0; t < nmin; ++t) {
      const double wt = lam[t] * b;
      const double* ut = Up + (size_t)t * p;
      const double* vt = Vp + (size_t)t * q;
      for (int k = 0; k < K; ++k) qk_acc[k] += wt * ut[m.oi[k]] * vt[m.oj[k]];
    }
    double nq2 = 0;
    for (int k = 0; k < K; ++k) {
      const double pqv = m.w[k] * qk_acc[k];
      pq[k] = pqv;
      nq2 += pqv * pqv;
    }
    const double nq = std::sqrt(nq2);
    const double alpha = (nq <= 0) ? 1.0
                                   : std::min(1.0, C_Q * sig * std::sqrt((double)p * rho) / nq);
    for (int k = 0; k < K; ++k) gv[k] = (-ev[k] + alpha * pq[k]) / rho;

    // ---- gradient products, restricted to Omega ----------------------------
    GR.zeros(); GL.zeros();
    for (int r = 0; r < p; ++r) {
      double* gr = GR.memptr() + r;
      for (int idx = m.rp[r]; idx < m.rp[r + 1]; ++idx) {
        const int k = m.ro[idx];
        const double g = gv[k];
        const double* rj = Rp + m.oj[k];
        for (int t = 0; t < s; ++t) gr[(size_t)t * p] += g * rj[(size_t)t * q];
      }
    }
    for (int c = 0; c < q; ++c) {
      double* gl = GL.memptr() + c;
      for (int idx = m.cp[c]; idx < m.cp[c + 1]; ++idx) {
        const int k = m.co[idx];
        const double g = gv[k];
        const double* li = Lp + m.oi[k];
        for (int t = 0; t < s; ++t) gl[(size_t)t * q] += g * li[(size_t)t * p];
      }
    }

    arma::mat S = Rb.t() * Rb; S.diag() += gamma;
    DL = -GR * spd_inv(S);
    S = Lb.t() * Lb; S.diag() += gamma;
    DR = -GL * spd_inv(S);

    double ddir = 0;
    { const double* a1 = DL.memptr(); const double* a2 = GR.memptr();
      for (size_t z = 0, n = (size_t)p * s; z < n; ++z) ddir -= a1[z] * a2[z]; }
    { const double* a1 = DR.memptr(); const double* a2 = GL.memptr();
      for (size_t z = 0, n = (size_t)q * s; z < n; ++z) ddir -= a1[z] * a2[z]; }
    if (!std::isfinite(ddir) || ddir <= 0) break;

    A = Lb + step0 * DL;
    B = Rb + step0 * DR;
    bal_fact(A, B, s, L, R);
    proj_B_factors(L, R, Bnd, p, q, s);
    ++k0;

    // ---- held-out monitor --------------------------------------------------
    if (has_val) {
      double acc = 0;
      const double* Lq = L.memptr(); const double* Rq = R.memptr();
      for (size_t z = 0; z < vi.size(); ++z) {
        const double pr = entry(Lq, Rq, p, q, s, vi[z] - 1, vj[z] - 1);
        acc += sq(vy[z] - pr);
      }
      const double v = std::sqrt(acc / (double)vi.size());
      val_hist.push_back(v);
      if (std::isfinite(v) && v < v_best) { v_best = v; k_best = it + 1; }
      else if (patience > 0 && (it + 1) - k_best >= patience) break;
    }
    if (C_stop > 0) {
      const double nrm = std::sqrt(std::max(0.0, frob_diff_factors(L, R, Lb, Rb, s)));
      if (nrm <= C_stop * std::sqrt((double)s) * sig * std::sqrt((double)p / rho)) break;
    }
  }

  // No step completed -> the iterate is still the (possibly full-rank) init.
  arma::mat Mout = (k0 == 0) ? arma::mat(Md) : arma::mat(L * R.t());
  return List::create(_["M"] = wrap(Mout),
                      _["k0"] = k0,
                      _["k_best"] = k_best,
                      _["val_hist"] = wrap(val_hist));
}

// ===========================================================================
// 3. SVT refinement for ONE lambda
// ===========================================================================
// Ports Nuclear_opt_rsm's loop: minimise  loss(M) + lambda * ||M||_*
// by singular-value thresholding with backtracking.  Two exact savings over
// the reference: loss() re-uses the residual SVD already computed for the
// gradient instead of redoing the identical factorization, and ||M||_* is
// carried along in the thresholded singular values instead of being recomputed
// from a fresh svd(M).  Selection over the lambda grid is left to the caller.
//
// `trace` is a debugging hook, not part of the estimator: the R side never sets
// it, and a positive value makes each iteration print its step size, the number
// of backtracking halvings, and the change in the objective.  It is what showed
// that this refinement does not converge at the default tor = 1e-6 -- the R and
// C++ engines agree on those three quantities at every iteration, so the two
// implementations follow identical control flow and differ only through the
// rounding that the wandering objective then amplifies.
// [[Rcpp::export]]
NumericMatrix rsm_svt_one_cpp(IntegerVector Xi, IntegerVector Xj, NumericVector Y,
                              int p, int q, double lambda, double rho,
                              NumericVector lambda_hat, Nullable<NumericMatrix> W,
                              NumericMatrix M0, double sigma_pil,
                              int svt_iters, double tor, double t_init,
                              int trace = 0) {
  const Weights Wv = get_weights(W);
  Mask m = make_mask(Xi, Xj, Y, Wv, p, q);
  const int K = m.K, nmin = std::min(p, q);

  arma::vec lam(lambda_hat.begin(), lambda_hat.size());
  const double b_fix = std::sqrt((double)p) * sigma_pil;
  const double inv_b = 1.0 / b_fix;
  const double inv_nmin_rho = 1.0 / ((double)nmin * rho);

  arma::mat M(M0.begin(), p, q, false, true);
  arma::mat Ew(p, q, arma::fill::zeros), Ew2(p, q, arma::fill::zeros);
  arma::mat Ue, Ve, Ua, Va;
  arma::vec de, da;
  std::vector<double> gvv(K), accv(K), zv(K);

  // ||M0||_*  (singular values only)
  arma::vec dM; arma::svd(dM, M);
  double nuc_c = arma::accu(dM);

  double tstep = t_init, obj_before = R_PosInf;
  for (int iter = 1; iter <= svt_iters; ++iter) {
    // residual on Omega, zero-filled elsewhere, and its SVD
    std::fill(Ew.memptr(), Ew.memptr() + (size_t)p * q, 0.0);
    for (int k = 0; k < K; ++k)
      Ew.at(m.oi[k], m.oj[k]) = m.w[k] * (m.y[k] - M.at(m.oi[k], m.oj[k]));
    arma::svd_econ(Ue, de, Ve, Ew);

    // loss at the current point: the same weighted spectrum mismatch
    double loss_c = 0;
    for (int t = 0; t < nmin; ++t) {
      const double z = (de[t] - b_fix * lam[t]) * inv_b;
      loss_c += z * z;
    }
    loss_c *= inv_nmin_rho;
    const double Lc = loss_c + lambda * nuc_c;

    // gradient on Omega: g = -(W .* (U diag(a) V')), a_t as in the reference.
    // One factor column at a time keeps the gathered accesses in L1.
    const double* Up = Ue.memptr(); const double* Vp = Ve.memptr();
    std::fill(accv.begin(), accv.end(), 0.0);
    for (int t = 0; t < nmin; ++t) {
      const double av = (2.0 / (rho * b_fix)) * (1.0 / (double)nmin) *
                        (de[t] - b_fix * lam[t]) * inv_b;
      const double* ut = Up + (size_t)t * p;
      const double* vt = Vp + (size_t)t * q;
      for (int k = 0; k < K; ++k) accv[k] += av * ut[m.oi[k]] * vt[m.oj[k]];
    }
    for (int k = 0; k < K; ++k) gvv[k] = -m.w[k] * accv[k];
    double gg = 0;
    for (int k = 0; k < K; ++k) gg += gvv[k] * gvv[k];

    // backtracking on the step
    bool accepted = false;
    int bt_used = -1;
    for (int bt = 0; bt < 200; ++bt) {
      const double thr = tstep * lambda;
      arma::mat Amat = M;
      for (int k = 0; k < K; ++k) Amat.at(m.oi[k], m.oj[k]) -= tstep * gvv[k];
      arma::svd_econ(Ua, da, Va, Amat);

      arma::vec ds(nmin);
      double nuc_z = 0;
      for (int t = 0; t < nmin; ++t) {
        ds[t] = std::max(da[t] - thr, 0.0);
        nuc_z += ds[t];
      }
      // residual at Z = Ua diag(ds) Va', and its singular values
      std::fill(zv.begin(), zv.end(), 0.0);
      for (int t = 0; t < nmin; ++t) {
        const double ds_t = ds[t];
        if (ds_t == 0.0) continue;
        const double* ut = Ua.memptr() + (size_t)t * p;
        const double* vt = Va.memptr() + (size_t)t * q;
        for (int k = 0; k < K; ++k) zv[k] += ds_t * ut[m.oi[k]] * vt[m.oj[k]];
      }
      std::fill(Ew2.memptr(), Ew2.memptr() + (size_t)p * q, 0.0);
      double* e2p = Ew2.memptr();
      for (int k = 0; k < K; ++k)
        e2p[(size_t)m.oj[k] * p + m.oi[k]] = m.w[k] * (m.y[k] - zv[k]);
      arma::vec dz; arma::svd(dz, Ew2);
      double loss_z = 0;
      for (int t = 0; t < nmin; ++t) {
        const double zz = (dz[t] - b_fix * lam[t]) * inv_b;
        loss_z += zz * zz;
      }
      loss_z *= inv_nmin_rho;
      const double lhs = loss_z + lambda * nuc_z;
      if (std::isfinite(lhs) && lhs <= Lc - 1e-6 * tstep * gg) {
        M = Ua * arma::diagmat(ds) * Va.t();
        nuc_c = nuc_z;
        accepted = true;
        bt_used = bt;
        break;
      }
      tstep /= 2;
    }
    if (!accepted) break;
    tstep = std::min(tstep * 2, 1e12);
    if (trace > 0)
      Rcpp::Rcout << "cpp it=" << iter << " bt=" << bt_used
                  << " tstep=" << tstep << " Lc=" << Lc
                  << " d=" << std::abs(Lc - obj_before) << "\n";
    if (std::abs(Lc - obj_before) < tor) break;
    obj_before = Lc;
  }
  return wrap(M);
}

// ===========================================================================
// 4. Optional Algorithm-1 factor refinement (off by default)
// ===========================================================================
// Kept deliberately close to the reference implementation: this stage is a
// single handful of steps and is never on the default path, so it trades speed
// for an obvious correspondence with .rsm_alg1_R.
// [[Rcpp::export]]
NumericMatrix rsm_alg1_cpp(IntegerVector Xi, IntegerVector Xj, NumericVector Y,
                           int p, int q, int s, double rho,
                           NumericVector lambda_hat, Nullable<NumericMatrix> W,
                           NumericMatrix M_in, double sigma_pil,
                           int alg1_iters, double gamma, double tor, double t_init) {
  if (alg1_iters <= 0) return M_in;
  const Weights Wv = get_weights(W);
  Mask m = make_mask(Xi, Xj, Y, Wv, p, q);
  const int K = m.K, nmin = std::min(p, q);
  arma::vec lam(lambda_hat.begin(), lambda_hat.size());
  const double b_fix = std::sqrt((double)p) * sigma_pil;

  arma::mat M(M_in.begin(), p, q, false, true);
  arma::mat L, R, Lb, Rb;
  bal_dense(M, s, L, R);
  M = L * R.t();

  arma::mat Ew(p, q, arma::fill::zeros), Ue, Ve;
  arma::vec de;
  auto resid_svd = [&](const arma::mat& Mm) {
    std::fill(Ew.memptr(), Ew.memptr() + (size_t)p * q, 0.0);
    for (int k = 0; k < K; ++k)
      Ew.at(m.oi[k], m.oj[k]) = m.w[k] * (m.y[k] - Mm.at(m.oi[k], m.oj[k]));
    arma::svd_econ(Ue, de, Ve, Ew);
  };
  auto loss_of = [&]() {
    double acc = 0;
    for (int t = 0; t < nmin; ++t) {
      const double z = (de[t] - b_fix * lam[t]) / b_fix;
      acc += z * z;
    }
    return acc / (double)nmin / rho;
  };

  double obj_before = R_PosInf, tstep = t_init;
  for (int iter = 0; iter < alg1_iters; ++iter) {
    resid_svd(M);
    arma::vec a(nmin);
    for (int t = 0; t < nmin; ++t)
      a[t] = (2.0 / (rho * b_fix)) * (1.0 / (double)nmin) *
             (de[t] - b_fix * lam[t]) / b_fix;
    arma::mat G = -(Ue * arma::diagmat(a) * Ve.t());
    arma::mat Gs(p, q, arma::fill::zeros);
    for (int k = 0; k < K; ++k) Gs.at(m.oi[k], m.oj[k]) = m.w[k] * G.at(m.oi[k], m.oj[k]);

    arma::mat S = R.t() * R; S.diag() += gamma;
    arma::mat DL = -Gs * R * spd_inv(S);
    S = L.t() * L; S.diag() += gamma;
    arma::mat DR = -Gs.t() * L * spd_inv(S);
    const double ddir = -arma::accu(Gs % (DL * R.t() + L * DR.t()));
    if (!std::isfinite(ddir) || ddir <= 0) break;

    const double Lc = loss_of();
    bool accepted = false;
    arma::mat Mt;
    for (int bt = 0; bt < 200; ++bt) {
      Mt = (L + tstep * DL) * (R + tstep * DR).t();
      resid_svd(Mt);
      if (loss_of() <= Lc - 1e-4 * tstep * ddir) { accepted = true; break; }
      tstep /= 2;
    }
    if (!accepted) break;
    M = Mt;
    bal_dense(M, s, Lb, Rb); L = Lb; R = Rb; M = L * R.t();
    tstep = std::min(tstep * 2, 1e12);
    if (std::abs(Lc - obj_before) < tor) break;
    obj_before = Lc;
  }
  return wrap(M);
}
