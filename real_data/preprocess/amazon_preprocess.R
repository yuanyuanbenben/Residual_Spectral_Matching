#!/usr/bin/env Rscript
# ============================================================================
# Rebuild the Amazon rating CSVs used by the real-data experiments.
#
# Usage:
#   Rscript amazon_preprocess.R <raw_dir> [out_root]
#
# <raw_dir> contains the decompressed Amazon Reviews'23 files
#   Movies_and_TV.jsonl, Books.jsonl, Electronics.jsonl
# (https://amazon-reviews-2023.github.io/).
#
# out_root defaults to $REAL_DATA_ROOT, or to matrix_test/ next to the release
# tree when run directly.  The script writes
#   <out_root>/real_data_example2/data/
#     case{1,2,3}_{train,validation,test}data.csv
# with the columns X1_<split>, X2_<split>, Y_<split>: row index, column index
# and rating.
#
# The JSONL files are read in chunks by jsonlite::stream_in, so a 20 GB
# category is never loaded whole.
# ============================================================================

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1L) {
  stop('usage: amazon_preprocess.R <raw_dir> [out_root]')
}
raw_dir <- args[[1]]
THIS <- local({
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub('^--file=', '', a[grep('^--file=', a)])
  if (length(f) == 1L) normalizePath(dirname(f)) else normalizePath('.')
})
out_root <- if (length(args) >= 2L) args[[2]] else Sys.getenv('REAL_DATA_ROOT')
if (!nzchar(out_root)) {
  cand <- c(file.path(getwd(), 'matrix_test'),
            file.path(THIS, '..', '..', '..', 'matrix_test'),
            file.path(THIS, '..', '..', '..', '..', 'matrix_test'))
  has <- dir.exists(file.path(cand, 'real_data_example')) |
         dir.exists(file.path(cand, 'real_data_example2'))
  out_root <- if (any(has)) cand[which(has)[1]] else cand[2]
}
if (!dir.exists(raw_dir)) stop('not found: ', raw_dir)
if (!requireNamespace('jsonlite', quietly = TRUE)) {
  stop('the Amazon preprocessing needs the jsonlite package')
}

out_dir <- file.path(out_root, 'real_data_example2', 'data')
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

PAGESIZE <- 20000L

stream_chunks <- function(path, handler) {
  con <- file(path, open = 'r')
  on.exit(close(con))
  jsonlite::stream_in(con, pagesize = PAGESIZE, verbose = FALSE, handler = handler)
}

# One streaming pass: number of reviews of every parent_asin.
count_items <- function(path) {
  cnt <- new.env(hash = TRUE, parent = emptyenv())
  stream_chunks(path, function(df) {
    tb <- table(df$parent_asin)
    for (nm in names(tb)) {
      old <- cnt[[nm]]
      cnt[[nm]] <- if (is.null(old)) as.integer(tb[[nm]]) else
        old + as.integer(tb[[nm]])
    }
    NULL
  })
  items <- ls(cnt)
  freq <- vapply(items, function(nm) cnt[[nm]], integer(1))
  names(freq) <- items
  freq[order(names(freq))]          # the ordering table() uses
}

# Second streaming pass: keep the four columns of the selected items only.
collect_items <- function(path, keep) {
  out <- list()
  stream_chunks(path, function(df) {
    z <- df[df$parent_asin %in% keep,
            c('parent_asin', 'user_id', 'rating'), drop = FALSE]
    if (nrow(z)) out[[length(out) + 1L]] <<- z
    NULL
  })
  if (!length(out)) return(NULL)
  do.call(rbind, out)
}

# case | review file | minimum ratings per user (among the top 1000 items)
categories <- data.frame(
  case        = c(1L, 2L, 3L),
  file        = c('Movies_and_TV.jsonl', 'Books.jsonl', 'Electronics.jsonl'),
  min_ratings = c(14L, 12L, 12L),
  stringsAsFactors = FALSE
)

for (k in seq_len(nrow(categories))) {
  path <- file.path(raw_dir, categories$file[k])
  if (!file.exists(path)) stop('not found: ', path)

  freq <- count_items(path)
  keep <- names(freq)[order(freq, decreasing = TRUE)]
  keep <- keep[seq_len(min(1000L, length(keep)))]

  d <- collect_items(path, keep)
  if (is.null(d) || !nrow(d)) stop('no reviews with a selected item in ', path)

  users <- table(d$user_id)
  used_id <- names(users)[users > categories$min_ratings[k]]
  if (length(used_id) < 3000L) {
    stop(sprintf('%s: only %d users pass the threshold, need 3000',
                 categories$file[k], length(used_id)))
  }
  set.seed(20240504)
  used_id <- sample(used_id, 3000L, replace = FALSE)

  d <- d[d$user_id %in% used_id, ]
  X1_total <- match(d$user_id, used_id)
  X2_total <- match(d$parent_asin, keep)
  Y_total  <- as.numeric(d$rating)

  n <- length(Y_total)
  train_index <- sample.int(n, round(n * 4 / 6), replace = FALSE)
  rest <- setdiff(seq_len(n), train_index)
  validation_pos <- sample.int(length(rest), round(length(rest) / 2),
                               replace = FALSE)
  validation_index <- rest[validation_pos]
  test_index <- rest[-validation_pos]

  write_split <- function(idx, split) {
    z <- data.frame(X1_total[idx], X2_total[idx], Y_total[idx])
    names(z) <- c(paste0('X1_', split), paste0('X2_', split), paste0('Y_', split))
    write.csv(z, file.path(out_dir,
                           sprintf('case%d_%sdata.csv', categories$case[k], split)))
  }
  write_split(train_index, 'train')
  write_split(validation_index, 'validation')
  write_split(test_index, 'test')

  cat(sprintf('case%d (%s): m=%d n=%d ratings=%d\n',
              categories$case[k], categories$file[k],
              length(used_id), length(keep), n))
}
