# Compare different heteroscedastic noise patterns for convex matrix completion
# This script performs Monte Carlo simulations to compare two convex optimization
# methods under four types of heteroscedastic noise patterns:
#   mode 1: row-wise Bernoulli scaling
#   mode 2: row-wise Gaussian scaling
#   mode 3: row-column separable Bernoulli scaling
#   mode 4: row-column separable Gaussian scaling
#
# Input: command line arguments:
#   args[1] = sigma (noise level)
#   args[2] = r (rank)
#   args[3] = mode (noise type, 1-4)
# Output: CSV files with loss metrics for both methods

library(foreach)
library(doParallel)

source('../R/baseline_methods.R')
source('../R/hetero_proposed_method.R')

args = commandArgs(trailingOnly = TRUE)
m = 500
n = 250
p = n/m
rho = 0.2
r = as.integer(args[2])
sigma = as.numeric(args[1])
lambda_plus = (m*n)**(0.5) * (1 + p**0.5)
mode = as.integer(args[3])
lambda_tuning1 = 0.5
lambda_tuning2 = 0.5

error_measure_func <- function(M_1,M_2,m,n,r){
  f_norm = sqrt(mean((M_1-M_2)**2)/mean(M_1**2))
  spectral_norm = svd(M_1 - M_2)$d[1]/svd(M_1)$d[1]
  maximal_norm = max(abs(M_1 - M_2))/max(abs(M_1))
  svd_1 = svd(M_1)
  svd_2 = svd(M_2)
  subspace_loss = (sqrt(sum((svd_1$u[1:m,1:r] - svd_1$u[1:m,1:r] %*% t(svd_1$u[1:m,1:r]) %*% svd_2$u[1:m,1:r])^2)/r)
                   + sqrt(sum((svd_1$v[1:n,1:r] - svd_1$v[1:n,1:r] %*% t(svd_1$v[1:n,1:r]) %*% svd_2$v[1:n,1:r])^2)/r))
  return(c(f_norm,spectral_norm,maximal_norm,subspace_loss))
}

cl.cores = detectCores(logical = F)
cl <- makeCluster(102)
registerDoParallel(cl)

loss_total <- foreach(seed=1:100,.verbose=TRUE,.combine = rbind) %dopar% {
  set.seed(seed)

  H = matrix(rnorm(m*n),m,n) * 0.5**0.5 + (matrix(rbinom(m*n,1,0.5),m,n)*2 - 1) * 0.5 ** 0.5
  if (mode == 1){
    noise_index = rbinom(m,1,0.2) + 0.2
    H = H*noise_index/0.32**0.5
  }
  if (mode == 2){
    noise_index = rnorm(m)
    H = H*noise_index
  }
  if (mode == 3){
    noise_index1 = (rbinom(m,1,0.2) + 0.2)/0.32**0.5
    noise_index2 = (rbinom(n,1,0.2) + 0.2)/0.32**0.5
    H = H*(matrix(noise_index1,m,1)%*%noise_index2)
  }
  if (mode == 4){
    noise_index1 = rnorm(m)
    noise_index2 = rnorm(n)
    H = H*(matrix(noise_index1,m,1)%*%noise_index2)
  }

  S = matrix(rnorm(m*n),m,n)
  svd_S = svd(S)
  u = svd_S$u[1:m,1:r]
  v = svd_S$v[1:n,1:r]
  if (r == 5){
    M_0 = u %*% diag(c(1,1.2,1.4,1.6,1.8)) %*% t(v) * lambda_plus
  }
  if (r == 10){
    M_0 = u %*% diag(c(1,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9)) %*% t(v) * lambda_plus
  }
  if (r == 20){
    M_0 = u %*% diag(c(1,1.05,1.1,1.15,1.2,1.25,1.3,1.35,1.4,1.45,1.5,1.55,1.6,1.65,1.7,1.75,1.8,1.85,1.9,1.95)) %*% t(v) * lambda_plus
  }

  M = H * sigma + M_0

  observed_index = sample.int(m*n,round(m*n*rho),replace = FALSE)
  X1_index = (observed_index-1)%/%n + 1
  X2_index = observed_index%%n + 1
  sample_size = length(X1_index)
  Y_obs = rep(0,sample_size)
  M_obs = matrix(0,m,n)
  for (i in 1:sample_size){
    Y_obs[i] = M[X1_index[i],X2_index[i]]
    M_obs[X1_index[i],X2_index[i]] = M[X1_index[i],X2_index[i]]
  }

  diag_H_mat = matrix(0,n,100)
  if (mode == 1){
    for (j in 1:100){
      temp_H1 = matrix(rnorm(m*n),m,n)
      temp_H = matrix(0,m,n)
      noise_index = rbinom(m,1,0.2) + 0.2
      for (i in 1:sample_size){
        temp_H[X1_index[i],X2_index[i]] = temp_H1[X1_index[i],X2_index[i]]
      }
      temp_H = temp_H*noise_index/0.32**0.5
      diag_H_mat[,j] = svd(temp_H)$d
    }
  }
  if (mode == 2){
    for (j in 1:100){
      temp_H1 = matrix(rnorm(m*n),m,n)
      temp_H = matrix(0,m,n)
      noise_index = rnorm(m)
      for (i in 1:sample_size){
        temp_H[X1_index[i],X2_index[i]] = temp_H1[X1_index[i],X2_index[i]]
      }
      temp_H = temp_H*noise_index
      diag_H_mat[,j] = svd(temp_H)$d
    }
  }
  if (mode == 3){
    for (j in 1:100){
      temp_H1 = matrix(rnorm(m*n),m,n)
      temp_H = matrix(0,m,n)
      noise_index1 = (rbinom(m,1,0.2) + 0.2)/0.32**0.5
      noise_index2 = (rbinom(n,1,0.2) + 0.2)/0.32**0.5
      for (i in 1:sample_size){
        temp_H[X1_index[i],X2_index[i]] = temp_H1[X1_index[i],X2_index[i]]
      }
      temp_H = temp_H*(matrix(noise_index1,m,1)%*%noise_index2)
      diag_H_mat[,j] = svd(temp_H)$d
    }
  }
  if (mode == 4){
    for (j in 1:100){
      temp_H1 = matrix(rnorm(m*n),m,n)
      temp_H = matrix(0,m,n)
      noise_index1 = rnorm(m)
      noise_index2 = rnorm(n)
      for (i in 1:sample_size){
        temp_H[X1_index[i],X2_index[i]] = temp_H1[X1_index[i],X2_index[i]]
      }
      temp_H = temp_H*(matrix(noise_index1,m,1)%*%noise_index2)
      diag_H_mat[,j] = svd(temp_H)$d
    }
  }
  diag_H = rowMeans(diag_H_mat)

  M_init = M_0 + matrix(rnorm(m*n),m,n) * 5

  tuning = lambda_tuning2 * lambda_plus
  stepsize = 0.2
  M_hat_baseline1 = Nuclear_opt_func(X1_index,X2_index,Y_obs,sample_size,m,n,tuning,stepsize,init=TRUE,M_input = M_init,tor=1e-4*(sigma**2+1e-10))
  loss_baseline1 = error_measure_func(M_0,M_hat_baseline1,m,n,r)
  M_hat_obs = rep(0,sample_size)
  for (i in 1:sample_size){
    M_hat_obs[i] = M_hat_baseline1[X1_index[i],X2_index[i]]
  }
  sigma_hat = (mean((M_hat_obs-Y_obs)**2))**0.5

  tuning = lambda_tuning1 * lambda_plus
  gamma = 1
  M_hat_our = Nuclear_opt_new_func_hetero(X1_index,X2_index,Y_obs,sample_size,m,n,tuning,gamma,spectrum_reference_init=TRUE,
                                          spectrum_reference = diag_H,sigma = sigma_hat,init=TRUE,M_input = M_init,tor=1e-4*(sigma**2+1e-10))
  loss_our = error_measure_func(M_0,M_hat_our,m,n,r)

  loss_return = rep(0,8)
  loss_return[1:4] = loss_our
  loss_return[5:8] = loss_baseline1
  loss_return
}

loss_our = data.frame(loss_total[,1:4])
write.csv(loss_our,paste("output/hetero_convex_loss_our1",mode,m,n,rho,r,sigma,".csv",sep = "_"))
loss_baseline1 = data.frame(loss_total[,5:8])
write.csv(loss_baseline1,paste("output/hetero_convex_loss_baseline1",mode,m,n,rho,r,sigma,".csv",sep = "_"))

stopCluster(cl)
