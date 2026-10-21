# ============================================================================
# Report generator for the heteroscedastic experiment.
#
#   base_*     : the two baselines
#   rsm_*      : RSM, homoscedastic (W = NULL)
#   hetW_*     : RSM whitened with the estimated noise profile
#   hetWtrue_* : RSM whitened with the true noise law
#
# Reads output_hetero/*.csv (see hetero_job.R).  Reports E_F, E_sp, E_inf per
# mode and folded over the 4 modes.
#
# Usage: Rscript aggregate_hetero_paper.R [report.txt] [dir]
#   appends to report.txt (run aggregate_paper.R first: it truncates the file)
# ============================================================================
args <- commandArgs(TRUE)
REP <- if (length(args) >= 1) args[1] else 'output_paper_report.txt'
DIR <- if (length(args) >= 2) args[2] else 'output_hetero'

rd <- function(dir, pat) {
  f <- list.files(dir, pattern = pat, full.names = TRUE)
  if (!length(f)) stop('no csv in ', dir)
  do.call(rbind, lapply(f, read.csv))
}
D <- rd(DIR, '^hetero_m.*\\.csv$')

out <- function(...) cat(..., '\n', sep = '', file = REP, append = TRUE)

out(sprintf('\n================================================================================'))
out('# HETEROSCEDASTIC experiment (baselines vs RSM: homo / whitened estimated / whitened true)')
out(sprintf('# input dir: %s   rows: %d', DIR, nrow(D)))
out(sprintf('# *** CAVEAT -- EVERY `*_nuc` ARM HERE IS ORACLE-LAMBDA-TUNED: `hetero_job.R`'))
out(sprintf('#     sweeps the penalty and keeps the argmin of the TRUE E_F (`e <- ef(rt$v)`).'))
out(sprintf('#     The baseline nuc arm uses a FIXED 0.5*lambda_plus, so part of every nuc'))
out(sprintf('#     win is an oracle advantage, NOT algorithmic.  See aggregate_paper.R\'s'))
out(sprintf('#     caveat (1).  The whitened fac arms (hetW_fac / hetWtrue_fac) and the'))
out(sprintf('#     `*_k` columns are NOT truth-tuned: hetW estimates W from a data pilot,'))
out(sprintf('#     `hetWtrue_*` uses the TRUE noise LAW (declared, the "known law" arm), and'))
out(sprintf('#     k* comes from the held-out OBSERVED RMSE.'))
out(sprintf('#     Protocol A everywhere: the init M_0 + 5N is the paper\'s own, shared with'))
out(sprintf('#     the baselines; a data-only init largely TIES the arms.'))

modes <- sort(unique(D$mode))
sigs  <- sort(unique(D$sigma), decreasing = TRUE)
mdlab <- c('1 row-Bernoulli', '2 row-Gaussian', '3 rowcol-Bernoulli', '4 rowcol-Gaussian')

FAC <- c('base_fac', 'rsm_fac', 'hetW_fac', 'hetWtrue_fac')
NUC <- c('base_nuc', 'rsm_nuc', 'hetW_nuc', 'hetWtrue_nuc')
sfx <- c(f = '', sp = '_sp', inf = '_inf')

blk <- function(arms, metric, md) {
  out(sprintf('%-16s %s', 'method', paste(sprintf('%10g', sigs), collapse = '')))
  for (mm in arms) {
    cc <- paste0(mm, sfx[metric])
    if (!(cc %in% names(D))) { out(sprintf('%-16s (missing %s)', mm, cc)); next }
    vals <- sapply(sigs, function(s) {
      v <- D[[cc]][D$sigma == s & D$mode == md]; v <- v[is.finite(v)]
      if (!length(v)) 'NA' else sprintf('%.4f', mean(v))
    })
    out(sprintf('%-16s %s', mm, paste(sprintf('%10s', vals), collapse = '')))
  }
}

mnames <- c(f = 'E_F (relative Frobenius)', sp = 'E_sp (relative spectral)',
            inf = 'E_inf (relative max)')
for (metric in c('f', 'sp', 'inf')) {
  out(sprintf('\n================ %s -- per mode (fac) ================', mnames[metric]))
  for (md in modes) { out(sprintf('\n--- mode %s ---', mdlab[md])); blk(FAC, metric, md) }
  out(sprintf('\n================ %s -- per mode (nuc) ================', mnames[metric]))
  for (md in modes) { out(sprintf('\n--- mode %s ---', mdlab[md])); blk(NUC, metric, md) }
  out(sprintf('\n================ %s -- folded over the 4 modes ================', mnames[metric]))
  out(sprintf('%-16s %s', 'method', paste(sprintf('%10g', sigs), collapse = '')))
  for (mm in c(FAC, '|', NUC)) {
    if (mm == '|') { out('---'); next }
    cc <- paste0(mm, sfx[metric])
    if (!(cc %in% names(D))) next
    vals <- sapply(sigs, function(s) {
      v <- D[[cc]][D$sigma == s]; v <- v[is.finite(v)]
      if (!length(v)) 'NA' else sprintf('%.4f', mean(v))
    })
    out(sprintf('%-16s %s', mm, paste(sprintf('%10s', vals), collapse = '')))
  }
}

out('\n================ improvement vs baseline (positive = RSM is better) ================')
rel <- function(a, b, metric) {
  ca <- paste0(a, sfx[metric]); cb <- paste0(b, sfx[metric])
  if (!(ca %in% names(D)) || !(cb %in% names(D))) return('   NA')
  aa <- D[[ca]]; bb <- D[[cb]]; ok <- is.finite(aa) & is.finite(bb)
  if (!any(ok)) return('   NA')
  sprintf('%+6.1f%%', 100 * (mean(bb[ok]) - mean(aa[ok])) / mean(bb[ok]))
}
for (metric in c('f', 'sp', 'inf')) {
  out(sprintf('\n-- metric %s --', mnames[metric]))
  for (a in c('rsm_fac','hetW_fac','hetWtrue_fac','rsm_nuc','hetW_nuc','hetWtrue_nuc')) {
    b <- if (grepl('fac', a)) 'base_fac' else 'base_nuc'
    out(sprintf('%-16s vs %-9s %s', a, b, rel(a, b, metric)))
  }
}

out('\n================ stopping point k* (folded over the 4 modes) ================')
out(sprintf('%-16s %s', 'arm \\ sigma', paste(sprintf('%8g', sigs), collapse = '')))
for (mm in c('rsm_fac', 'hetW_fac', 'hetWtrue_fac')) {
  kc <- paste0(mm, '_k')
  if (!(kc %in% names(D))) { out(sprintf('%-16s (no %s column)', mm, kc)); next }
  mk <- sapply(sigs, function(s) { v <- D[[kc]][D$sigma == s]; v <- v[is.finite(v)]
                                   if (!length(v)) NA_real_ else mean(v) })
  pc <- sapply(sigs, function(s) { v <- D[[kc]][D$sigma == s]; v <- v[is.finite(v)]
                                   if (!length(v)) NA_real_ else 100 * mean(v >= 1500) })
  out(sprintf('%-16s %s', mm, paste(sprintf('%8.1f', mk), collapse = '')))
  out(sprintf('%-16s %s', '  [% reps at cap]', paste(sprintf('%8.1f', pc), collapse = '')))
  cells <- split(D[[kc]], list(mlab = D$mode, sg = D$sigma), drop = TRUE)
  cap <- vapply(cells, function(v) { v <- v[is.finite(v)]
                                     length(v) > 0 && all(v >= 1500) }, logical(1))
  if (any(cap))
    out(sprintf('# CAVEAT cap-stop %s: %d/%d cells at RSM_CAP=1500 (every rep): %s',
                mm, sum(cap), length(cap), paste(names(cap)[cap], collapse = ' ')))
}

out('\n================ failures (NA) ================')
nf <- 0L
for (mm in c(FAC, NUC)) {
  for (sf in c('', '_sp', '_inf')) {
    cc <- paste0(mm, sf)
    if (!(cc %in% names(D))) next
    n <- sum(!is.finite(D[[cc]]))
    if (n > 0) { out(sprintf('%-16s %5d / %d   (%s)', mm, n, nrow(D), cc)); nf <- nf + n }
  }
}
out(if (nf == 0L) sprintf('none: every arm of every metric is finite in all %d rows', nrow(D))
    else sprintf('total non-finite entries in the reported columns: %d', nf))
cat(sprintf('appended to %s\n', REP))
