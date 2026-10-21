#!/usr/bin/env Rscript
# Aggregate the real-data fits.
#
#   Rscript aggregate_real.R --select              -> cache/selected_s.csv
#   Rscript aggregate_real.R [report.txt]          -> output_real_report.txt

THIS <- local({
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub('^--file=', '', a[grep('^--file=', a)])
  if (length(f) == 1L) normalizePath(dirname(f)) else normalizePath('.')
})
setwd(THIS)

args <- commandArgs(trailingOnly = TRUE)
select_mode <- '--select' %in% args
args <- setdiff(args, '--select')
report_file <- if (length(args) >= 1L) args[[1]] else 'output_real_report.txt'

rd <- function(f) {
  z <- tryCatch(read.csv(f, stringsAsFactors = FALSE), error = function(e) NULL)
  if (is.null(z) || nrow(z) == 0L) return(NULL)
  z
}

all_files <- list.files('output_real', pattern = '\\.csv$', recursive = TRUE,
                        full.names = TRUE)
all_files <- all_files[!grepl('/cache/', all_files)]
if (length(all_files) == 0L) stop('no result CSVs under output_real/')

fill <- function(z, col, val) {
  if (is.null(z)) return(NULL)
  if (is.null(z[[col]])) z[[col]] <- val
  z
}
res <- lapply(all_files, function(f) fill(rd(f), 'step', NA_real_))
res <- do.call(rbind, res)
res <- res[res$status == 'OK' & is.finite(res$val_rmse), , drop = FALSE]
if (nrow(res) == 0L) stop('no successful fits under output_real/')

# ---- selection -------------------------------------------------------------
key <- paste(res$dataset, res$case, res$method, sep = '\r')
idx <- tapply(seq_len(nrow(res)), key, function(i) i[which.min(res$val_rmse[i])])
best <- res[unlist(idx), , drop = FALSE]

if (select_mode) {
  dir.create('cache', showWarnings = FALSE)
  sel <- best[best$method == 'rsm_fac', c('dataset', 'case', 's')]
  sel <- sel[order(sel$dataset, sel$case), , drop = FALSE]
  write.csv(sel, 'cache/selected_s.csv', row.names = FALSE, quote = FALSE, na = '')
  cat(sprintf('wrote cache/selected_s.csv (%d rows)\n', nrow(sel)))
  print(sel, row.names = FALSE)
  quit(status = 0L)
}

# ---- helpers ---------------------------------------------------------------
getrow <- function(ds, case, method) {
  z <- best[best$dataset == ds & best$case == case & best$method == method, , drop = FALSE]
  if (nrow(z) == 0L) return(NULL)
  z[1, , drop = FALSE]
}

arm_value <- function(ds, case, method) {
  z <- getrow(ds, case, method)
  if (is.null(z)) return(list(rel = NA_real_, rel_mod = NA_real_,
                              abs = NA_real_, abs_mod = NA_real_,
                              s = NA_integer_, lambda = NA_real_,
                              step = NA_real_))
  list(rel = z$rel_test, rel_mod = z$rel_test_mod,
       abs = z$test_rmse, abs_mod = z$test_rmse_mod,
       s = z$s, lambda = z$lambda, step = z$step)
}

fmt <- function(x) ifelse(is.na(x), '   --  ', sprintf('%7.4f', x))

# Scenario order of the paper's tables.
scenario_map <- list(netflix = c(1L, 2L, 3L),
                     amazon  = c(1L, 2L, 3L))

obs_rate <- function(ds, case) {
  root <- Sys.getenv('REAL_DATA_ROOT')
  if (!nzchar(root)) return(NA_real_)      # densities are optional in the report
  z <- tryCatch(read.csv(sprintf(
    '%s/data/case%d_traindata.csv',
    file.path(root,
              if (ds == 'netflix') 'real_data_example' else 'real_data_example2'),
    case)), error = function(e) NULL)
  if (is.null(z)) return(NA_real_)
  nrow(z) / (max(z[[2]]) * max(z[[3]]))
}

titles <- c(netflix = 'NETFLIX RATING DATA',
            amazon  = 'AMAZON REVIEW DATA')

COLS <- c('BaseFac', "BaseFac'", 'RsmFac', "RsmFac'",
          'BaseNuc', "BaseNuc'", 'RsmNuc', "RsmNuc'",
          'HetFac', "HetFac'", 'HetNuc', "HetNuc'")

sink(report_file)

cat('=============================================================================\n')
cat('REAL-DATA EXPERIMENTS\n')
cat(sprintf('generated: %s\n', format(Sys.time(), '%Y-%m-%d %H:%M:%S')))
cat('=============================================================================\n')
cat('\nMetric: relative RMSE on the test split = RMSE(test) / sqrt(mean(Y_test^2)).\n')
cat('Base = the two baselines; Rsm = RSM; Het = whitened RSM.\n')
cat('Primed columns clip the predictions to [1, 5]; the table at the end gives\n')
cat('the same twelve columns as ABSOLUTE RMSE.\n\n')

for (ds in c('netflix', 'amazon')) {
  cat('-----------------------------------------------------------------------------\n')
  cat(titles[[ds]], '\n')
  cat('-----------------------------------------------------------------------------\n')
  cat(sprintf('%-8s %-5s %-7s %s\n', 'Scenario', 'case', 'obs',
              paste(sprintf('%8s', COLS), collapse = ' ')))
  for (sc in seq_along(scenario_map[[ds]])) {
    case <- scenario_map[[ds]][sc]
    bf <- arm_value(ds, case, 'base_fac')
    bn <- arm_value(ds, case, 'base_nuc')
    e  <- arm_value(ds, case, 'rsm_fac')
    f  <- arm_value(ds, case, 'rsm_nuc')
    g  <- arm_value(ds, case, 'hetW_fac')
    h  <- arm_value(ds, case, 'hetW_nuc')
    row <- c(bf$rel, bf$rel_mod, e$rel, e$rel_mod,
             bn$rel, bn$rel_mod, f$rel, f$rel_mod,
             g$rel, g$rel_mod, h$rel, h$rel_mod)
    cat(sprintf('%-8d %-5d %-7.4f %s\n', sc, case, obs_rate(ds, case),
                paste(fmt(row), collapse = ' ')))
  }
  cat('\nSelected tuning parameters (validation RMSE)\n')
  cat(sprintf('%-8s %-5s %-22s %-34s %-34s\n', 'Scenario', 'case',
              'BaseFac / BaseNuc',
              'RsmFac / RsmNuc (s,lambda; step)',
              'HetFac / HetNuc (step; lambda)'))
  for (sc in seq_along(scenario_map[[ds]])) {
    case <- scenario_map[[ds]][sc]
    bf <- arm_value(ds, case, 'base_fac')
    bn <- arm_value(ds, case, 'base_nuc')
    e  <- arm_value(ds, case, 'rsm_fac')
    f  <- arm_value(ds, case, 'rsm_nuc')
    g  <- arm_value(ds, case, 'hetW_fac')
    h  <- arm_value(ds, case, 'hetW_nuc')
    num <- function(x) if (is.null(x) || is.na(x)) '--' else format(x)
    st <- if (is.null(e$step) || is.na(e$step)) 'default' else format(e$step)
    cat(sprintf('%-8d %-5d %-22s %-34s %-34s\n', sc, case,
                sprintf('%s / %s', num(bf$s), num(bn$lambda)),
                sprintf('%s / %s; %s', num(e$s),
                        if (is.na(f$s)) '--' else sprintf('%s,%s', num(f$s), num(f$lambda)),
                        st),
                sprintf('%s / %s; %s', num(g$step), num(h$step), num(h$lambda))))
  }
  cat('\nWhitened (heteroscedastic) arms\n')
  cat(sprintf('%-8s %-5s %-6s %-12s %-12s %-12s %-12s\n', 'Scenario', 'case', 's',
              'pilot RMSE', 'pilot step', 'step (fac)', 'step (nuc)'))
  for (sc in seq_along(scenario_map[[ds]])) {
    case <- scenario_map[[ds]][sc]
    hz <- getrow(ds, case, 'het_ref')
    g <- arm_value(ds, case, 'hetW_fac')
    h <- arm_value(ds, case, 'hetW_nuc')
    num2 <- function(x) if (is.null(x) || is.na(x)) '--' else sprintf('%.4g', x)
    cat(sprintf('%-8d %-5d %-6s %-12s %-12s %-12s %-12s\n', sc, case,
                if (is.null(hz) || is.na(hz$s)) '--' else format(hz$s),
                if (is.null(hz) || is.na(hz$val_rmse)) '--' else sprintf('%.4f', hz$val_rmse),
                if (is.null(hz) || is.na(hz$step)) '--' else sprintf('%.4g', hz$step),
                num2(g$step), num2(h$step)))
  }
  cat(sprintf('  hetW_nuc selected lambda: %s\n', paste(vapply(
    seq_along(scenario_map[[ds]]), function(sc) {
      h <- arm_value(ds, scenario_map[[ds]][sc], 'hetW_nuc')
      if (is.null(h$lambda) || is.na(h$lambda)) '--' else format(h$lambda)
    }, character(1)), collapse = '  ')))
  cat('\nABSOLUTE test RMSE, same twelve columns (primed = CLIPPED to [1,5])\n')
  cat(sprintf('%-8s %-5s %s\n', 'Scenario', 'case',
              paste(sprintf('%8s', COLS), collapse = ' ')))
  for (sc in seq_along(scenario_map[[ds]])) {
    case <- scenario_map[[ds]][sc]
    bf <- arm_value(ds, case, 'base_fac')
    bn <- arm_value(ds, case, 'base_nuc')
    e  <- arm_value(ds, case, 'rsm_fac')
    f  <- arm_value(ds, case, 'rsm_nuc')
    g  <- arm_value(ds, case, 'hetW_fac')
    h  <- arm_value(ds, case, 'hetW_nuc')
    row <- c(bf$abs, bf$abs_mod, e$abs, e$abs_mod,
             bn$abs, bn$abs_mod, f$abs, f$abs_mod,
             g$abs, g$abs_mod, h$abs, h$abs_mod)
    cat(sprintf('%-8d %-5d %s\n', sc, case, paste(fmt(row), collapse = ' ')))
  }
  cat('\n')
}

cat('-----------------------------------------------------------------------------\n')
cat('NOTE\n')
cat('relative RMSE is normalized per scenario by sqrt(mean(Y_test^2)).  The\n')
cat('paper\'s real-data table divides each dataset block by one constant\n')
cat('(Netflix scenario 1: 3.8202, scenarios 2-4: 3.7259; Amazon: 4.445), so\n')
cat('compare rows within this report rather than against that table as printed.\n')
cat('=============================================================================\n')
sink()
