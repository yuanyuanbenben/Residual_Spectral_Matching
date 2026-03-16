# Compare different condition numbers for non-convex optimization
# This script performs Monte Carlo simulations to compare the performance of
# the proposed matrix factorization method against a baseline method
# under varying condition numbers of the underlying low-rank matrix.

library(foreach)
library(doParallel)

source('../R/baseline_methods.R')
source('../R/proposed_method.R')

args = commandArgs(trailingOnly = TRUE)
m = 500
n = 250
p = n/m
rho = 0.05
r = as.integer(args[2])
sigma = 1
kappa = as.numeric(args[1])
kappa_ = kappa - 1
lambda_plus = (m*n)**(0.5) * (1 + p**0.5)
s = r
stepsize1 = 100/kappa
stepsize2 = 10/kappa

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

  S = matrix(rnorm(m*n),m,n)
  svd_S = svd(S)
  u = svd_S$u[1:m,1:r]
  v = svd_S$v[1:n,1:r]
  if (r == 5){
    M_0 = u %*% diag(c(kappa_ + 1,0.75*kappa_ + 1,0.5*kappa_ + 1,0.25*kappa_ + 1,1)) %*% t(v) * lambda_plus
  }
  if (r == 10){
    M_0 = u %*% diag(c(kappa_ + 1,8/9*kappa_ + 1,7/9*kappa_ + 1,6/9*kappa_ + 1,5/9*kappa_ + 1,4/9*kappa_ + 1,
                       3/9*kappa_ + 1,2/9*kappa_ + 1,1/9*kappa_ + 1,1)) %*% t(v) * lambda_plus
  }
  if (r == 20){
    M_0 = u %*% diag(c(kappa,18/19*kappa_ + 1, 17/19*kappa_ + 1,16/19*kappa_ + 1,15/19*kappa_ + 1,14/19*kappa_ + 1,13/19*kappa_ + 1,12/19*kappa_ + 1,
                       11/19*kappa_ + 1,10/19*kappa_ + 1,9/19*kappa_ + 1,8/19*kappa_ + 1,7/19*kappa_ + 1,6/19*kappa_ + 1,5/19*kappa_ + 1,
                       4/19*kappa_ + 1,3/19*kappa_ + 1,2/19*kappa_ + 1,1/19*kappa_ + 1,1)) %*% t(v) * lambda_plus
  }

  M = H*sigma + M_0

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

  M_init = M_0 + matrix(rnorm(m*n),m,n) * 5

  M_hat_baseline1 = Matrix_factor_func(X1_index,X2_index,Y_obs,sample_size,m,n,0,s,
                                       init=TRUE,M_input=M_init,step_size = stepsize2,itertime=30000,penalty='None',tor=1e-4*(sigma**2+1e-10))
  loss_baseline1 = error_measure_func(M_0,M_hat_baseline1,m,n,r)

  M_hat_our = Matrix_factor_new_func(X1_index,X2_index,Y_obs,sample_size,m,n,0,stepsize1,s,
                                     init=TRUE,M_input=M_init,step_size = 1,itertime=30000,penalty='None',tor=1e-4*(sigma**2+1e-10))
  loss_our = error_measure_func(M_0,M_hat_our,m,n,r)

  loss_return = rep(0,8)
  loss_return[1:4] = loss_our
  loss_return[5:8] = loss_baseline1
  loss_return
}

loss_our = data.frame(loss_total[,1:4])
write.csv(loss_our,paste("output/nonconvex_loss_our1",m,n,rho,s,r,sigma,kappa,".csv",sep = "_"))
loss_baseline1 = data.frame(loss_total[,5:8])
write.csv(loss_baseline1,paste("output/nonconvex_loss_baseline1",m,n,rho,s,r,sigma,kappa,".csv",sep = "_"))

stopCluster(cl)
