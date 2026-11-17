# ============================================================================
# Report generator for the six homoscedastic simulation families -- one
# section per paper table, three metrics each:
#   E_F   (relative Frobenius) : sqrt(mean((M0-A)^2) / mean(M0^2))
#   E_sp  (relative spectral)  : svd(M0-A)$d[1]  / svd(M0)$d[1]
#   E_inf (relative max)       : max|M0-A|       / max|M0|
#
# Reads output_homo_<fam><DIRV>/*.csv (see homo_job.R), averages over the reps of
# each cell, and prints one cell x method table per metric.  Columns:
#   base_fac / base_nuc    the two baselines
#   rsm_fac  / rsm_nuc     the RSM estimators, under the held-out stopping rule
#
# Usage: Rscript aggregate_paper.R [report.txt] [dir-suffix]
#
# This is the generator that STARTS the report; aggregate_hetero_paper.R and
# aggregate_misrank.R append their own sections to the same file afterwards.
# ============================================================================
REP  <- if (length(commandArgs(TRUE)) >= 1) commandArgs(TRUE)[1] else 'output_paper_report.txt'
DIRV <- if (length(commandArgs(TRUE)) >= 2) commandArgs(TRUE)[2] else ''  # dir suffix
FAMS <- c('size', 'cond', 'noise', 'supp', 'robust', 'dep')
BY <- list(size   = c('m', 'r'),
           cond   = c('kappa', 'r'),
           noise  = c('r', 'sigma'),
           supp   = c('sigma'),
           robust = c('m', 'r', 'hetero_size'),
           dep    = c('r', 'dep_mode', 'dep_ratio'))
FAC <- c('base_fac', 'rsm_fac')
NUC <- c('base_nuc', 'rsm_nuc')

rd <- function(dir, pat) {
  f <- list.files(dir, pattern = pat, full.names = TRUE)
  if (!length(f)) return(NULL)
  do.call(rbind, lapply(f, read.csv))
}

all <- list()
for (fam in FAMS) {
  d <- rd(paste0('output_homo_', fam, DIRV), '^homo_.*csv$')
  if (is.null(d)) { cat(sprintf('%-7s (no files in output_homo_%s%s)\n', fam, fam, DIRV)); next }
  cat(sprintf('%-7s rows=%4d\n', fam, nrow(d)))
  all[[fam]] <- d
}
D <- do.call(rbind, all)
if (is.null(D) || !nrow(D)) stop('no result files found -- run the families first')

out <- function(...) cat(..., '\n', sep = '', file = REP, append = TRUE)
cat('', file = REP, append = FALSE)
out(sprintf('# Simulation report -- the six homoscedastic families'))
out(sprintf('# input dirs: output_homo_<fam>%s   rows=%d', DIRV, nrow(D)))
out(sprintf('# metrics: E_F (fro) | E_sp (spectral) | E_inf (max); rows = cells, cols = methods'))
out(sprintf('#'))
out(sprintf('# ---- legend: method columns of the E_F / E_sp / E_inf tables ----'))
out(sprintf('#   base_fac      : BASELINE, nonconvex factorization (MF gradient descent)'))
out(sprintf('#   base_nuc      : BASELINE, nuclear-norm proximal'))
out(sprintf('#   rsm_fac       : RSM factorization, step0 = 0.1*rho, with the held-out'))
out(sprintf('#                   validation stopping rule (RSM_STOP=val, RSM_CAP=1500):'))
out(sprintf('#                   stage 1 runs the SSA loop on 90%% of the observed entries and'))
out(sprintf('#                   picks the argmin of the held-out RMSE; stage 2 reruns it on the'))
out(sprintf('#                   FULL mask for exactly that many steps, so no data is discarded.'))
out(sprintf('#   rsm_nuc       : RSM nuclear-norm; the penalty is swept over'))
out(sprintf('#                   10^seq(-7,-2,0.5) and the sweep member with the smallest E_F is'))
out(sprintf('#                   reported; the SSA phase is shared with rsm_fac, which is'))
out(sprintf('#                   lambda-free, so the two differ only in the SVT refinement.'))
out(sprintf('#'))
out(sprintf('# ---- legend: the 2 columns of each "E_F improvement" block ----'))
out(sprintf('#   value = 100*(reference - RSM arm)/reference ; POSITIVE = RSM has LOWER E_F'))
out(sprintf('#   rsmFac/base = rsm_fac vs base_fac        rsmNuc/base = rsm_nuc vs base_nuc'))
out(sprintf('#   MEAN row = (mean reference - mean RSM) / mean reference -- a ratio of'))
out(sprintf('#              MEANS, not the mean of the per-cell percentages (more stable)'))
out(sprintf('#   a trailing `*` = the reference cell is at machine level (E_F < 1e-3), so the'))
out(sprintf('#              percentage is a ratio of two ~zero errors and MEANS NOTHING.'))
out(sprintf('#              Read the absolute E_F / E_sp / E_inf columns in those rows.'))
out(sprintf('#   the improvement block is E_F only; E_sp / E_inf are absolute tables above'))
out(sprintf('#'))
out(sprintf('# ---- CAVEATS you must carry into the paper tables -------------------------'))
out(sprintf('#  (0) WHAT IS AND IS NOT TRUTH-FREE -- read before quoting any number:'))
out(sprintf('#      DATA-ONLY (touches no M_0): the SSA/RSM iteration itself, the step'))
out(sprintf('#      0.1*rho and rho, `lambda_hat = rsm_reference(...)`, and the WHOLE'))
out(sprintf('#      held-out stopping rule (stage 1 fits 90%% of the OBSERVED entries and'))
out(sprintf('#      takes the argmin of the held-out OBSERVED RMSE) -> the headline'))
out(sprintf('#      `rsm_fac` and every `*_k` are clean.  The working rank `s = r` and the'))
out(sprintf('#      protocol-A init are SHARED with the baselines (not an edge).'))
out(sprintf('#  (1) ORACLE-SELECTED PENALTY -- EVERY `*_nuc` ARM: the penalty sweep takes'))
out(sprintf('#      the argmin of the TRUE error (`e <- ef(rt$v)`).  Those arms are tuned'))
out(sprintf('#      ON THE TRUTH -- an advantage the baseline\'s FIXED-lambda nuc arm'))
out(sprintf('#      (0.5*lambda_plus) does NOT have.  So a `*_nuc` win, supp\'s +70%% above'))
out(sprintf('#      all, is part oracle-lambda + part better SSA start, NOT a pure'))
out(sprintf('#      algorithmic win; and the `*_nuc` numbers are a LOWER BOUND (no honest'))
out(sprintf('#      lambda rule can beat a truth-tuned one).  `rsm_fac` is NOT affected --'))
out(sprintf('#      its only truth contact is the init, which under protocol A the'))
out(sprintf('#      baselines get too.'))
out(sprintf('#  (2) NUCLEAR-NORM SMALL-sigma STALL: the baseline nuc arm uses that FIXED'))
out(sprintf('#      lambda, which at small sigma (noise/supp s<=0.05) overshrinks and STALLS'))
out(sprintf('#      at E_F ~ 0.13, nearly flat in s -- so that comparison is against a'))
out(sprintf('#      STALLED reference as well (a baseline tuning defect, separate from (1)).'))
out(sprintf('#  (3) PROTOCOL A = ORACLE INIT (`M_input = M_0 + 5N`) for EVERY method, ours'))
out(sprintf('#      and the baselines alike (the paper\'s own protocol).  Under a data-only'))
out(sprintf('#      init the arms largely TIE -- the RSM estimator\'s edge there is'))
out(sprintf('#      speed/reliability, not accuracy.  Do NOT present these tables as a'))
out(sprintf('#      truth-free comparison.'))
out(sprintf('#  (4) CAP-LIMITED CELLS: the stopping rule exits on RSM_CAP=1500 as well as on'))
out(sprintf('#      the held-out criterion.  Cells flagged by the CAVEAT cap-stop / near-cap'))
out(sprintf('#      lines below are limited by the CAP, not the rule -- they would improve'))
out(sprintf('#      further with a larger cap (all sigma=0 cells, and small sigma generally).'))
out(sprintf('#      At sigma=0 the BASELINE is numerically EXACT (E_F 0.0000, clean data +'))
out(sprintf('#      oracle init + 30000 steps), so no RSM arm can win there, only tie;'))
out(sprintf('#      worst case noise r=20 s=0: rsm_fac 0.0042 / rsm_nuc 0.0039 vs 0.0000 --'))
out(sprintf('#      the small step0 (0.1*rho) has not converged even at 1500 steps, so that'))
out(sprintf('#      row is a CAP+step0 artefact.'))
out(sprintf('#  (5) PARTIAL-NA CELLS: flagged per family; the cell mean then averages over the'))
out(sprintf('#      surviving reps only.'))
out(sprintf('#'))

collapse <- function(d, by, metric) {
  sfx <- if (metric == 'f') '' else paste0('_', metric)
  key <- interaction(d[, by, drop = FALSE], drop = TRUE, lex.order = TRUE)
  parts <- split(seq_len(nrow(d)), key)
  rows <- lapply(parts, function(ix) {
    e <- d[ix, , drop = FALSE]
    row <- as.data.frame(e[1, by, drop = FALSE], stringsAsFactors = FALSE)
    for (g in c(FAC, NUC)) {
      cc <- paste0(g, sfx)
      row[[g]] <- if (cc %in% names(e)) mean(e[[cc]], na.rm = TRUE) else NA_real_
    }
    row$n       <- nrow(e)
    row$na_fac  <- if ('rsm_fac' %in% names(e)) sum(!is.finite(e$rsm_fac)) else NA_integer_
    row$na_nuc  <- if ('rsm_nuc' %in% names(e)) sum(!is.finite(e$rsm_nuc)) else NA_integer_
    row$.lbl    <- if ('.lbl' %in% names(e)) e$.lbl[1] else
                     do.call(paste, c(e[1, by, drop = FALSE], sep = '_'))
    row
  })
  m <- do.call(rbind, rows)
  m <- m[do.call(order, m[by]), , drop = FALSE]
  m
}

fmtpct <- function(a, base) {
  if (is.na(a) || is.na(base) || base == 0) return(sprintf('%+7.2f%%', NA_real_))
  sprintf('%+7.2f%%%s', 100 * (base - a) / base, if (base < 1e-3) '*' else '')
}

for (fam in FAMS) {
  d <- all[[fam]]; if (is.null(d)) next
  by <- BY[[fam]]
  if (fam == 'size')   d$.lbl <- sprintf('r=%s m=%s',  d$r, d$m)
  if (fam == 'cond')   d$.lbl <- sprintf('k=%s r=%s',  d$kappa, d$r)
  if (fam == 'noise')  d$.lbl <- sprintf('r=%s s=%s',  d$r, d$sigma)
  if (fam == 'supp')   d$.lbl <- sprintf('s=%s',        d$sigma)
  if (fam == 'robust') d$.lbl <- sprintf('m=%s r=%s h=%s', d$m, d$r, d$hetero_size)
  if (fam == 'dep')    d$.lbl <- sprintf('r=%s d=%s rt=%s', d$r, d$dep_mode, d$dep_ratio)

  out(sprintf('\n================================================================================'))
  out(sprintf('FAMILY %s   (%d reps/cell, %d rows)', fam, max(1L, round(nrow(d) / max(1, length(unique(interaction(d[, by, drop = FALSE], drop = TRUE)))))), nrow(d)))

  for (metric in c('f', 'sp', 'inf')) {
    m <- collapse(d, by, metric)
    present <- c(intersect(FAC, names(m)), intersect(NUC, names(m)))
    if (!length(present)) next
    out(sprintf('\n-- %s --', c(f = 'E_F (relative Frobenius)', sp = 'E_sp (relative spectral norm)',
                                 inf = 'E_inf (relative max norm)')[metric]))
    hdr <- sprintf('%-22s %s', 'cell', paste(sprintf('%14s', present), collapse = ''))
    out(hdr)
    for (i in seq_len(nrow(m))) {
      vals <- sapply(present, function(g) {
        v <- m[[g]][i]; if (!is.finite(v)) 'NA' else sprintf('%.4f', v)
      })
      out(sprintf('%-22s %s', m$.lbl[i], paste(sprintf('%14s', vals), collapse = '')))
    }
  }

  mf <- collapse(d, by, 'f')
  out(sprintf('\n-- E_F improvement vs baseline (positive = better) --'))
  out(sprintf('%-22s %9s | %9s', 'cell', 'rsmFac/base', 'rsmNuc/base'))
  for (i in seq_len(nrow(mf))) {
    out(sprintf('%-22s %9s | %9s', mf$.lbl[i],
        fmtpct(mf$rsm_fac[i], mf$base_fac[i]),
        fmtpct(mf$rsm_nuc[i], mf$base_nuc[i])))
  }
  out(sprintf('%-22s %9s | %9s', 'MEAN',
              fmtpct(mean(mf$rsm_fac, na.rm = TRUE), mean(mf$base_fac, na.rm = TRUE)),
              fmtpct(mean(mf$rsm_nuc, na.rm = TRUE), mean(mf$base_nuc, na.rm = TRUE))))
  out(sprintf('failures (NA) of RSM arms: rsm_fac %d  rsm_nuc %d  of %d fits',
              sum(mf$na_fac), sum(mf$na_nuc), sum(mf$n)))

  key <- interaction(d[, by, drop = FALSE], drop = TRUE, lex.order = TRUE)
  lab <- tapply(d$.lbl, key, function(v) v[1])
  if ('rsm_fac_k' %in% names(d)) {
    cap <- tapply(d$rsm_fac_k, key, function(v) {
      v <- v[is.finite(v)]; length(v) > 0 && all(v >= 1500) })
    ic <- which(cap)
    out(sprintf('# CAVEAT cap-stop: %d/%d cells ran to RSM_CAP=1500 (every rep) -- CAP-LIMITED%s',
                length(ic), length(cap),
                if (length(ic)) paste0(': ', paste(lab[ic], collapse = ', ')) else ''))
    pf <- tapply(d$rsm_fac_k, key, function(v) { v <- v[is.finite(v)]
                                                 if (!length(v)) 0 else mean(v >= 1500) })
    ip <- which(pf > 0 & pf < 1)
    if (length(ip))
      out(sprintf('# CAVEAT near-cap: %d cell(s) where only some reps hit the cap: %s',
                  length(ip), paste(sprintf('%s %d%%', lab[ip], round(100 * pf[ip])), collapse = ', ')))
  }
  for (arm in c('rsm_fac', 'rsm_nuc')) {
    if (!(arm %in% names(d))) next
    nna <- tapply(!is.finite(d[[arm]]), key, sum)
    nto <- tapply(!is.finite(d[[arm]]), key, length)
    ib <- which(nna > 0)
    if (length(ib))
      out(sprintf('# CAVEAT partial-NA %s: %s', arm,
                  paste(sprintf('%s %d/%d reps', lab[ib], nna[ib], nto[ib]), collapse = '; ')))
  }
}

out('\n# ==== per-family MEAN E_F improvement (RSM vs baseline) ====')
out('# rsmFac = the RSM factorization (step0 = 0.1*rho, nothing tuned per cell)')
for (fam in FAMS) {
  d <- all[[fam]]; if (is.null(d)) next
  mf <- collapse(d, BY[[fam]], 'f')
  g <- function(a, base) {
    if (!(a %in% names(mf)) || !(base %in% names(mf))) return(' NA ')
    if (all(is.na(mf[[a]])) || all(is.na(mf[[base]]))) return(' NA ')
    sprintf('%+6.2f%%', 100 * (mean(mf[[base]], na.rm = TRUE) - mean(mf[[a]], na.rm = TRUE)) / mean(mf[[base]], na.rm = TRUE))
  }
  out(sprintf('%-7s  rsmFac vs base %s   |   nuc vs base %s',
              fam, g('rsm_fac', 'base_fac'), g('rsm_nuc', 'base_nuc')))
}
cat(sprintf('wrote %s\n', REP))
