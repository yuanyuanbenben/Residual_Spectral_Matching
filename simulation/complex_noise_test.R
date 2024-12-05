setwd('/home/yuanyuanbenben/project_dmc/matrix_test/mc_spectral_method')
source('baseline_methods.R')
source('proposed_method.R')
source('complex_noise_proposed_method.R')
library(RVCompare)
library(ICtest)


m = 500
n = round(m/2)
r = 5


M_comp = matrix(rnorm(m*n),m,n)
svd_M_comp = svd(M_comp)
U0 = svd_M_comp$u[1:m,1:r]
V0 = svd_M_comp$v[1:n,1:r]
# lambda_min = 0.5
# lambda_plus = 2.5
# f_func <- function(x){
#   return(x+log(x)+1)
# }
# x_value = (1:10000)*(lambda_plus - lambda_min)/10000 + lambda_min
# den <- function(x){
#   #return((f_func(x))*((x-lambda_min)*(lambda_plus-x))**0.5/2/pi)
#   return(1)
# }
# C_d = mean(den(x_value))
# den_nomal <- function(x){
#   return(den/C_d)
# }
# H_D = (sampleFromDensity(den,n,c(lambda_min,lambda_plus)))**0.5
# U = rorth(m)[1:m,1:n]
# V = rorth(n)
# H = U%*%diag(H_D)%*%V
# H = H / (mean(H**2)**0.5)
# rmse = rep(0,1000)
# for (i in 1:1000) {
rho = 0.2
observed_index = sample.int(m*n,round(m*n*rho),replace = FALSE)
X1_index = (observed_index-1)%/%n + 1
X2_index = observed_index%%n + 1
sample_size = length(X1_index)
Y_obs = rep(0,sample_size)
M_obs = matrix(0,m,n)
H_obs = matrix(0,m,n)
Sigma = c(1,1.1,1.2,1.3,1.4)*2*m**(0.5) * (1 + 0.5**0.5)
M = U0%*%diag(Sigma)%*%t(V0)
# 
H = matrix(rnorm(m*n),m,n)* (matrix((1:m),m,1)%*%matrix((1:n),1,n))
H = H / (mean(H**2))**0.5

Y =  M + H #matrix(rnorm(m*n),m,n)
for (j in 1:sample_size){
  Y_obs[j] = Y[X1_index[j],X2_index[j]]
  M_obs[X1_index[j],X2_index[j]] = Y[X1_index[j],X2_index[j]]
  H_obs[X1_index[j],X2_index[j]] = H[X1_index[j],X2_index[j]]
}
  
#   a = svd(M_obs)$d[20:230]
#   a = a/mean(a)
#   b = svd(H_obs)$d[20:230]
#   b = b/mean(b)
#   
#   rmse[i]=sum((a-b)**2)/sum(a**2)
# }
# 
# pdf(file="myplot2.pdf")
# probability = c(21:1020)**2/1020**2
# plot(probability[100:1000],rmse[100:1000],type="l")
# dev.off()

stepsize1 = 200
stepsize2 = 20

error_measure_func <- function(M_1,M_2,m,n,r){
  f_norm = sqrt(sum((M_1-M_2)^2)/m/n)
  spectral_norm = svd(M_1 - M_2)$d[1]/sqrt(m)
  maximal_norm = max(abs(M_1 - M_2))
  svd_1 = svd(M_1)
  svd_2 = svd(M_2)
  subspace_loss = (sqrt(sum((svd_1$u[1:m,1:r] - svd_1$u[1:m,1:r] %*% t(svd_1$u[1:m,1:r]) %*% svd_2$u[1:m,1:r])^2)/r) 
                   + sqrt(sum((svd_1$v[1:n,1:r] - svd_1$v[1:n,1:r] %*% t(svd_1$v[1:n,1:r]) %*% svd_2$v[1:n,1:r])^2)/r))
  return(c(f_norm,spectral_norm,maximal_norm,subspace_loss))
}

s = r
M_init = svd(M_obs)$u[1:m,1:s] %*% diag(svd(M_obs)$d[1:s]) %*% t(svd(M_obs)$v[1:n,1:s]) / rho

# our method
M_hat_our = Matrix_factor_new_complex_func(X1_index,X2_index,Y_obs,sample_size,m,n,0,stepsize1,s,
                                   init=TRUE,M_input=M_init,step_size = 1,itertime=30000,penalty='None',tor=1e-4)
loss_our = error_measure_func(M,M_hat_our,m,n,r)



# baseline1: F norm based on matrix factorization
M_hat_baseline1 = Matrix_factor_func(X1_index,X2_index,Y_obs,sample_size,m,n,0,s,
                                     init=TRUE,M_input=M_init,step_size = stepsize2,itertime=30000,penalty='None',tor=1e-4)
loss_baseline1 = error_measure_func(M,M_hat_baseline1,m,n,r)


