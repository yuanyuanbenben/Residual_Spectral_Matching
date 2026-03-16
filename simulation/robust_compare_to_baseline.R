library(foreach)
library(doParallel)

source('../R/baseline_methods.R')
source('../R/proposed_method.R')

args = commandArgs(trailingOnly = TRUE)
# setting
m = as.integer(args[1])
n = as.integer(args[2])
p = n/m
rho = as.integer(args[3])/100
r = as.integer(args[6])
sigma = 1
lambda_plus = (m*n)**(0.5) * (1 + p**0.5)
# lambda_ = m**0.5 * (1 - p**0.5)
# setting rank
s = as.integer(args[5])
stepsize1 = as.integer(args[7])
stepsize2 = as.integer(args[8])
hetero_size = as.numeric(args[9])

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
cl <- makeCluster(as.integer(args[4]))
registerDoParallel(cl)
set.seed(123456)
noise_index = matrix(runif(m*n,1-hetero_size,1+hetero_size),m,n)
# baseline compare
loss_total <- foreach(seed=1:100,.verbose=TRUE,.combine = rbind) %dopar% {
  set.seed(seed)

  # noise setting
  # H = matrix(rnorm(m*n),m,n)
  # H = matrix(rbinom(m*n,1,0.5),m,n) * 2 - 1
  H = matrix(rnorm(m*n),m,n) * 0.5**0.5 + (matrix(rbinom(m*n,1,0.5),m,n)*2 - 1) * 0.5 ** 0.5
  H = H*noise_index


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


  # M(0)
  # M_init = svd(M_obs)$u[1:m,1:s] %*% diag(svd(M_obs)$d[1:s]) %*% t(svd(M_obs)$v[1:n,1:s]) / rho
  M_init = M_0 + matrix(rnorm(m*n),m,n) * 5

  # baseline
  M_hat_baseline1 = Matrix_factor_func(X1_index,X2_index,Y_obs,sample_size,m,n,0,s,
                                       init=TRUE,M_input=M_init,step_size = stepsize2,itertime=30000,penalty='None',tor=1e-4*(sigma**2+1e-10))
  loss_baseline1 = error_measure_func(M_0,M_hat_baseline1,m,n,r)


  # our method (robust)
  M_hat_our_robust = Matrix_factor_new_func(X1_index,X2_index,Y_obs,sample_size,m,n,0,stepsize1,s,
                                            init=TRUE,M_input=M_init,step_size = 1,itertime=30000,penalty='None',tor=1e-4*(sigma**2+1e-10))
  loss_our_robust = error_measure_func(M_0,M_hat_our_robust,m,n,r)
  print(loss_baseline1)
  print(loss_our_robust)


  loss_return = rep(0,8)
  loss_return[1:4] = loss_our_robust
  loss_return[5:8] = loss_baseline1
  # loss_return[9:12] = loss_our2
  # loss_return[13:16] = loss_baseline2
  # loss_return[17:20] = loss_our3
  # loss_return[21:24] = loss_baseline3
  loss_return
}

loss_our_robust = data.frame(loss_total[,1:4])
write.csv(loss_our_robust,paste("output/robust_nonconvex_loss_our1",m,n,rho,s,r,hetero_size,".csv",sep = "_"))
loss_baseline1 = data.frame(loss_total[,5:8])
write.csv(loss_baseline1,paste("output/robust_nonconvex_loss_baseline1",m,n,rho,s,r,hetero_size,".csv",sep = "_"))

stopCluster(cl)
