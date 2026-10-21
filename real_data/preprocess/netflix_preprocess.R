#!/usr/bin/env Rscript
# ============================================================================
# Rebuild the Netflix rating CSVs used by the real-data experiments.
#
# Usage:
#   Rscript netflix_preprocess.R <netflix_data.csv> [out_root]
#
# <netflix_data.csv> has the header movies,id,score,time: movie id, user id,
# rating, date.  The README shows how to build it from the Netflix Prize
# combined_data_*.txt files.
#
# out_root defaults to $REAL_DATA_ROOT, or to matrix_test/ next to the release
# tree when run directly.  The script writes
#   <out_root>/real_data_example/data/
#     case{1,2,3}_{train,validation,test}data.csv
# with the columns X1_<split>, X2_<split>, Y_<split>: row index, column index
# and rating.
# ============================================================================

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1L) {
  stop('usage: netflix_preprocess.R <netflix_data.csv> [out_root]')
}
raw_file <- args[[1]]
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
if (!file.exists(raw_file)) stop('not found: ', raw_file)

out_dir <- file.path(out_root, 'real_data_example', 'data')
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

data_total <- read.csv(raw_file, stringsAsFactors = FALSE)
names(data_total) <- c('movies', 'id', 'score', 'time')

# Items: movies with more than 25,000 ratings in the full data.
movies <- as.data.frame(table(data_total$movies))
used_movies <- as.numeric(levels(droplevels(movies[movies$Freq > 25000, 1])))
data <- data_total[data_total$movies %in% used_movies, ]
ids <- as.data.frame(table(data$id))

# case | minimum ratings per user | users kept (NA = every user above it)
cases <- data.frame(
  case        = c(1L, 2L, 3L),
  min_ratings = c(700L, 200L, 75L),
  n_users     = c(NA_integer_, 3000L, 3000L)
)

for (k in seq_len(nrow(cases))) {
  # Every case starts from the same seed, as in the paper.
  set.seed(20240504)
  used_id <- as.numeric(levels(droplevels(ids[ids$Freq > cases$min_ratings[k], 1])))
  if (!is.na(cases$n_users[k])) {
    used_id <- sample(used_id, cases$n_users[k], replace = FALSE)
  }

  d <- data[data$id %in% used_id, ]
  X1_total <- match(d$id, used_id)
  X2_total <- match(d$movies, used_movies)
  Y_total  <- d$score

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
                           sprintf('case%d_%sdata.csv', cases$case[k], split)))
  }
  write_split(train_index, 'train')
  write_split(validation_index, 'validation')
  write_split(test_index, 'test')

  cat(sprintf('case%d: m=%d n=%d ratings=%d\n',
              cases$case[k], length(used_id), length(used_movies), n))
}
