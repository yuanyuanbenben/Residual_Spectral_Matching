library(foreach)
library(doParallel)

setwd('/home/yuanyuanbenben/project_dmc/matrix_test/mc_spectral_method')
source('baseline_methods.R')
source('proposed_method.R')

args = commandArgs(trailingOnly = TRUE)
# setting
m = 500
n = 250
p = n/m
rho = 0.2
r = 10
sigma = 1
lambda_plus = m**(0.5) * sigma * (1 + p**0.5) 
lambda_ = m**0.5 * sigma * (1 - p**0.5) 
# setting rank
s = 10
stepsize1 = 100
stepsize2 = 10

set.seed(20240508)
  
# noise setting 
#H = matrix(rnorm(m*n),m,n)
#H = matrix(rbinom(m*n,1,0.5),m,n) * 2 - 1
H = matrix(rnorm(m*n),m,n) * 0.5**0.5 + (matrix(rbinom(m*n,1,0.5),m,n)*2 - 1) * 0.5 ** 0.5

S = matrix(rnorm(m*n),m,n)
svd_S = svd(S)
u = svd_S$u[1:m,1:r]
v = svd_S$v[1:n,1:r]
if (r == 5){
  M_0 = u %*% diag(c(1,1.1,1.2,1.3,1.4)) %*% t(v) * lambda_plus * 10 / 3
}
if (r == 10){
  M_0 = u %*% diag(c(1,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9)) %*% t(v) * lambda_plus * 10 / 3
}
if (r == 20){
  M_0 = u %*% diag(c(1,1.05,1.1,1.15,1.2,1.25,1.3,1.35,1.4,1.45,1.5,1.55,1.6,1.65,1.7,1.75,1.8,1.85,1.9,1.95)) %*% t(v) * lambda_plus * 10 / 3
}

M = H + M_0

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


residual_func <- function(M_1,M_2){
  M_3 = matrix(0,m,n)
  for (i in 1:sample_size){
    M_3[X1_index[i],X2_index[i]] = M_2[X1_index[i],X2_index[i]]
  }
  return(svd(M_1 - M_3)$d)
}

# M(0)
M_init = svd(M_obs)$u[1:m,1:s] %*% diag(svd(M_obs)$d[1:s]) %*% t(svd(M_obs)$v[1:n,1:s]) / rho

# our method
M_hat_our = Matrix_factor_new_func(X1_index,X2_index,Y_obs,sample_size,m,n,0,stepsize1,s,
                                   init=TRUE,M_input=M_init,step_size = 1,itertime=30000,penalty='None',tor=1e-4)
residual_our = residual_func(M_obs,M_hat_our)



# baseline1: F norm based on matrix factorization
M_hat_baseline1 = Matrix_factor_func(X1_index,X2_index,Y_obs,sample_size,m,n,0,s,
                                     init=TRUE,M_input=M_init,step_size = stepsize2,itertime=30000,penalty='None',tor=1e-4)
residual_baseline1 = residual_func(M_obs,M_hat_baseline1)


M_init = M_obs / rho

# our method

tuning = 60 * (n/100)**0.5
gamma = 1
M_hat_our_convex = Nuclear_opt_new_func(X1_index,X2_index,Y_obs,sample_size,m,n,tuning,gamma,init=TRUE,M_input = M_obs/rho,tor=1e-4)
residual_our_convex = residual_func(M_obs,M_hat_our_convex)


# baseline1: trancated svd
tuning = 60 * (n/100)**0.5
stepsize = 0.2
M_hat_baseline1_convex = Nuclear_opt_func(X1_index,X2_index,Y_obs,sample_size,m,n,tuning,stepsize,init=TRUE,M_input = M_obs/rho,tor=1e-4)
residual_baseline1_convex = residual_func(M_obs,M_hat_baseline1_convex)


loss_our = data.frame(residual_our,residual_our_convex)
write.csv(loss_our,paste("output/nonconvex_residual_our1",m,n,rho,s,r,".csv",sep = "_"))
loss_baseline1 = data.frame(residual_baseline1,residual_baseline1_convex)
write.csv(loss_baseline1,paste("output/nonconvex_residual_baseline1",m,n,rho,s,r,".csv",sep = "_"))



