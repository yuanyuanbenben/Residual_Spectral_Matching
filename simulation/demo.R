# Demo: Compare proposed method to baseline
# This is a simplified demo script with fixed parameters for quick testing
# Parameters: m=500, n=250, r=10, sigma=1, rho=0.5 (50% observation)
# Runs a single simulation and prints detailed results to console

library(foreach)
library(doParallel)

source('../R/baseline_methods.R')
source('../R/proposed_method.R')

# Fixed parameters (from simulation.sh for m=500, n=250)
m <- 500        # rows
n <- 250        # columns
r <- 10         # true rank
s <- 10         # factorization rank
rho <- 0.2      # observation fraction (20%)
sigma <- 1      # noise level
stepsize1 <- 100  # learning rate for proposed method
stepsize2 <- 10   # learning rate for baseline method

p <- n / m
lambda_plus <- (m * n)^0.5 * (1 + p^0.5)

cat("=== Residual Spectral Matching Demo ===\n\n")
cat("Parameters:\n")
cat(sprintf("  Matrix dimensions: %d x %d\n", m, n))
cat(sprintf("  True rank (r): %d\n", r))
cat(sprintf("  Factorization rank (s): %d\n", s))
cat(sprintf("  Observation fraction (rho): %.0f%%\n", rho * 100))
cat(sprintf("  Noise level (sigma): %.1f\n", sigma))
cat(sprintf("  Stepsize (proposed): %d\n", stepsize1))
cat(sprintf("  Stepsize (baseline): %d\n", stepsize2))
cat(sprintf("  lambda_plus: %.4f\n\n", lambda_plus))

# Define error measure function
error_measure_func <- function(M_1, M_2, m, n, r) {
  f_norm <- sqrt(mean((M_1 - M_2)^2) / mean(M_1^2))
  spectral_norm <- svd(M_1 - M_2)$d[1] / svd(M_1)$d[1]
  maximal_norm <- max(abs(M_1 - M_2)) / max(abs(M_1))

  svd_1 <- svd(M_1)
  svd_2 <- svd(M_2)
  subspace_loss <- (sqrt(sum((svd_1$u[1:m, 1:r] - svd_1$u[1:m, 1:r] %*%
                               t(svd_1$u[1:m, 1:r]) %*% svd_2$u[1:m, 1:r])^2) / r) +
                    sqrt(sum((svd_1$v[1:n, 1:r] - svd_1$v[1:n, 1:r] %*%
                               t(svd_1$v[1:n, 1:r]) %*% svd_2$v[1:n, 1:r])^2) / r))
  return(c(f_norm, spectral_norm, maximal_norm, subspace_loss))
}

# Set seed for reproducibility
set.seed(42)

cat("Generating synthetic data...\n")

# Generate H matrix (noise component)
H <- matrix(rnorm(m * n), m, n) * 0.5^0.5 +
     (matrix(rbinom(m * n, 1, 0.5), m, n) * 2 - 1) * 0.5^0.5

# Generate low-rank component S
S <- matrix(rnorm(m * n), m, n)
svd_S <- svd(S)
u <- svd_S$u[1:m, 1:r]
v <- svd_S$v[1:n, 1:r]

# Generate singular values for rank 10
singular_values <- c(1, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9)
M_0 <- u %*% diag(singular_values) %*% t(v) * lambda_plus

# Combine to get true matrix M
M <- H * sigma + M_0

cat(sprintf("  True matrix M generated (dimensions: %d x %d)\n", m, n))
cat(sprintf("  Rank of M_0: %d\n", r))
cat(sprintf("  Top 5 singular values of M_0: %.4f, %.4f, %.4f, %.4f, %.4f\n\n",
            singular_values[1] * lambda_plus, singular_values[2] * lambda_plus,
            singular_values[3] * lambda_plus, singular_values[4] * lambda_plus,
            singular_values[5] * lambda_plus))

# Generate observed entries
observed_index <- sample.int(m * n, round(m * n * rho), replace = FALSE)
X1_index <- (observed_index - 1) %/% n + 1
X2_index <- observed_index %% n + 1
sample_size <- length(X1_index)

Y_obs <- rep(0, sample_size)
M_obs <- matrix(0, m, n)
for (i in 1:sample_size) {
  Y_obs[i] <- M[X1_index[i], X2_index[i]]
  M_obs[X1_index[i], X2_index[i]] <- M[X1_index[i], X2_index[i]]
}

cat(sprintf("  Observed entries: %d (%.1f%% of total)\n\n",
            sample_size, sample_size / (m * n) * 100))

# Initialize M with perturbation
M_init <- M_0 + matrix(rnorm(m * n), m, n) * 5

cat("Running proposed method...\n")
M_hat_our <- Matrix_factor_new_func(X1_index, X2_index, Y_obs, sample_size, m, n, 0,
                                     stepsize1, s, init = TRUE, M_input = M_init,
                                     step_size = 1, itertime = 30000,
                                     penalty = 'None', tor = 1e-4 * (sigma^2 + 1e-10))
loss_our <- error_measure_func(M_0, M_hat_our, m, n, r)

cat("Running baseline method...\n")
M_hat_baseline <- Matrix_factor_func(X1_index, X2_index, Y_obs, sample_size, m, n, 0, s,
                                      init = TRUE, M_input = M_init,
                                      step_size = stepsize2, itertime = 30000,
                                      penalty = 'None', tor = 1e-4 * (sigma^2 + 1e-10))
loss_baseline <- error_measure_func(M_0, M_hat_baseline, m, n, r)

cat("\n")
cat("=== Results ===\n\n")

cat("Proposed Method:\n")
cat(sprintf("  Frobenius norm error:    %.6f\n", loss_our[1]))
cat(sprintf("  Spectral norm error:     %.6f\n", loss_our[2]))
cat(sprintf("  Maximal norm error:      %.6f\n", loss_our[3]))
cat(sprintf("  Subspace loss:           %.6f\n\n", loss_our[4]))

cat("Baseline Method:\n")
cat(sprintf("  Frobenius norm error:    %.6f\n", loss_baseline[1]))
cat(sprintf("  Spectral norm error:     %.6f\n", loss_baseline[2]))
cat(sprintf("  Maximal norm error:      %.6f\n", loss_baseline[3]))
cat(sprintf("  Subspace loss:           %.6f\n\n", loss_baseline[4]))

cat("=== Comparison ===\n")
cat(sprintf("Frobenius improvement:     %.2fx\n", loss_baseline[1] / loss_our[1]))
cat(sprintf("Spectral improvement:      %.2fx\n", loss_baseline[2] / loss_our[2]))
cat(sprintf("Maximal improvement:       %.2fx\n", loss_baseline[3] / loss_our[3]))
cat(sprintf("Subspace improvement:      %.2fx\n", loss_baseline[4] / loss_our[4]))

cat("\n=== Demo completed successfully ===\n")
