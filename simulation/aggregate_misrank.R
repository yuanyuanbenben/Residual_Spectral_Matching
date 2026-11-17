# ============================================================================
# Report generator for the misspecified-rank tables of the supplement
# (tab:supp:misrank_fnorm / _specnorm / _maxnorm).
#
#   m=500, n=250, rho=0.2, sigma=1; true rank r, working rank s = r..r+6;
#   100 reps.  Rows (r, estimator); columns s -- s is GIVEN, never chosen.
#
# Reads output_misrank/*.csv (see run_misrank.sh).  Averages over the reps of
# each (r, s) cell and prints one table per metric.
#
# Usage: Rscript aggregate_misrank.R [report.txt] [dir]
#   APPENDS to report.txt, like aggregate_hetero_paper.R; run aggregate_paper.R
#   first, since that is the one that starts the file.
# ============================================================================
DIR <- Sys.getenv('MISRANK_DIR', 'output_misrank')
out <- if (length(commandArgs(TRUE)) >= 1) commandArgs(TRUE)[1] else 'output_paper_report.txt'
if (length(commandArgs(TRUE)) >= 2) DIR <- commandArgs(TRUE)[2]
sink(out, append = TRUE)

f <- list.files(DIR, pattern = '^homo_misrank_.*[.]csv$', full.names = TRUE)
if (!length(f)) stop('no result files yet in ', DIR)
dt <- do.call(rbind, lapply(f, read.csv))

rs  <- sort(unique(dt$r))
ARMS <- c('base_fac', 'rsm_fac')
MET  <- c('E_F' = '', 'E_sp' = '_sp', 'E_inf' = '_inf')

NC   <- length(unique(paste(dt$r, dt$s)))   # cells; arms are COLUMNS, not rows
NREP <- max(table(paste(dt$r, dt$s)))       # the full rep count, from the fullest cell
cat('\n================================================================================\n')
cat('MISSPECIFIED RANK (tab:supp:misrank_fnorm / _specnorm / _maxnorm)\n')
cat(sprintf('# input %s: %d files, %d rows, %d cells x %d arms x %g reps/cell\n',
            DIR, length(f), nrow(dt), NC, length(ARMS), nrow(dt) / NC))
cat(sprintf('# setting: m=%d n=%d rho=%g sigma=%g, true r in {%s}, used s = r..r+6\n',
            dt$m[1], dt$n[1], dt$rho[1], dt$sigma[1], paste(rs, collapse = ',')))
cat('# E_F / E_sp / E_inf = relative Frobenius / spectral / maximal norm of M_hat - M_0\n')
cat('# the working rank s is GIVEN and never chosen: this section shows how the\n')
cat('# estimators degrade as s grows past the true rank r.\n')
cat('# means over reps; cells with any non-finite arm are averaged over the finite reps.\n\n')

LONG <- c('E_F' = 'E_F (relative Frobenius)', 'E_sp' = 'E_sp (relative spectral norm)',
          'E_inf' = 'E_inf (relative max norm)')
for (k in seq_along(MET)) {
  cat(sprintf('\n-- %s --\n', LONG[names(MET)[k]]))
  cat(sprintf('%-14s', ''))
  for (i in 0:6) cat(sprintf('%9s', sprintf('s=r+%d', i)))
  cat('\n')
  for (r in rs) {
    for (a in ARMS) {
      cat(sprintf('r=%-2d %-9s', r, a))
      for (i in 0:6) {
        v <- dt[[paste0(a, MET[[k]])]][dt$r == r & dt$s == r + i]
        v <- v[is.finite(v)]
        cat(sprintf('%9s', if (!length(v)) 'NA' else sprintf('%.4f', mean(v))))
      }
      cat('\n')
    }
    cat('\n')
  }
}

cat('\n---- relative degradation E_F(s=max) / E_F(s=r)  [lower = more robust] ----\n')
cat(sprintf('%-12s%10s', 'r', 's_max'))
for (a in ARMS) cat(sprintf('%12s', a))
cat('\n')
for (r in rs) {
  sm <- max(dt$s[dt$r == r], na.rm = TRUE)
  cat(sprintf('%-12s%10d', sprintf('r=%d', r), sm))
  for (a in ARMS) {
    lo <- dt[[a]][dt$r == r & dt$s == r];   lo <- lo[is.finite(lo)]
    hi <- dt[[a]][dt$r == r & dt$s == sm];  hi <- hi[is.finite(hi)]
    cat(sprintf('%12s', if (!length(lo) || !length(hi) || mean(lo) == 0) 'NA'
                        else sprintf('%.3fx', mean(hi) / mean(lo))))
  }
  cat('\n')
}

cat('\n---- per-cell diagnostics (files / count | non-finite values) ----\n')
cat(sprintf('%-8s%8s%10s%12s%12s%12s\n', 'r', 's', 'files', 'base_fac', 'rsm_fac', 'k bad'))
for (r in rs) {
  for (i in 0:6) {
    d <- dt[dt$r == r & dt$s == r + i, ]
    nf <- function(a) sum(!is.finite(d[[a]]))
    kb <- sum(!is.finite(d$rsm_fac_k))
    cat(sprintf('%-8d%8d%10d%12d%12d%12d%s\n', r, r + i, nrow(d),
                nf('base_fac'), nf('rsm_fac'), kb,
                if (nrow(d) < NREP || kb) '   <== CHECK' else ''))
  }
}

cat('\n---- median rsm_fac_k by (r, s): the stopping point the rule chose ----\n')
cat(sprintf('%-10s', ''))
for (i in 0:6) cat(sprintf('%9s', sprintf('s=r+%d', i)))
cat('\n')
for (r in rs) {
  cat(sprintf('%-10s', sprintf('r=%d', r)))
  for (i in 0:6) {
    v <- dt$rsm_fac_k[dt$r == r & dt$s == r + i]
    v <- v[is.finite(v)]
    cat(sprintf('%9s', if (!length(v)) 'NA' else sprintf('%g', median(v))))
  }
  cat('\n')
}
cat('\n')
sink()
cat(sprintf('written: %s\n', out))
