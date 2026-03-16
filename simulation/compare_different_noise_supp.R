# Compare different noise levels for non-convex optimization (supplementary)
# This script performs Monte Carlo simulations to compare the performance of
# the proposed matrix factorization method against a baseline method
# under varying noise levels (sigma). The ground-truth matrix is generated
# via a factor model (L * R^T) instead of SVD.

library(foreach)
library(doParallel)

source('../R/baseline_methods.R')
source('../R/proposed_method.R')

args = commandArgs(trailingOnly = TRUE)
m = 500
n = 250
p = n/m
rho = 0.2
r = as.integer(args[2])
sigma = as.numeric(args[1])
lambda_plus = (m*n)**(0.5) * (1 + p**0.5)
s = r
stepsize1 = 100
stepsize2 = 10

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
cl <- makeCluster(52)
registerDoParallel(cl)

loss_total <- foreach(seed=1:100,.verbose=TRUE,.combine = rbind) %dopar% {
  set.seed(seed)

  H = matrix(rnorm(m*n),m,n) * 0.5**0.5 + (matrix(rbinom(m*n,1,0.5),m,n)*2 - 1) * 0.5 ** 0.5

  L = matrix(rnorm(m*r),m,r)
  R = matrix(rnorm(n*r),n,r)
  M_0 = L%*%t(R)

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
write.csv(loss_our,paste("output/supp_nonconvex_loss_our1",m,n,rho,s,r,sigma,".csv",sep = "_"))
loss_baseline1 = data.frame(loss_total[,5:8])
write.csv(loss_baseline1,paste("output/supp_nonconvex_loss_baseline1",m,n,rho,s,r,sigma,".csv",sep = "_"))

stopCluster(cl)
