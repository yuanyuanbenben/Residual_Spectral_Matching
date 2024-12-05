setwd('/home/yuanyuanbenben/project_dmc/matrix_test')
source('baseline_methods.R')
source('proposed_method.R')
set.seed(20231230)

#set.seed(20231220)
# setting
m = 200
n = 100
p = n/m
rho = 0.2
r = 5
sigma = 1
lambda_plus = m**(0.5) * sigma * (1 + p**0.5) 
lambda_ = m**0.5 * sigma * (1 - p**0.5) 

#H = matrix(rnorm(m*n),m,n)

#H = matrix(rbinom(m*n,1,0.5),m,n) * 2 - 1

H = matrix(rnorm(m*n),m,n) * 0.5**0.5 + (matrix(rbinom(m*n,1,0.5),m,n)*2 - 1) * 0.5 ** 0.5

S = matrix(rnorm(m*n),m,n)
svd_S = svd(S)
u = svd_S$u[1:m,1:r]
v = svd_S$v[1:n,1:r]



M_0 = u %*% diag(c(1,1.1,1.2,1.3,1.4)) %*% t(v) * lambda_plus * 5

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


MSE_func <- function(M1,M2,m,n){
  return(sum((M1-M2)^2)/m/n)
}
M_init = svd(M_obs)$u[1:m,1:r] %*% diag(svd(M_obs)$d[1:r]) %*% t(svd(M_obs)$v[1:n,1:r]) / rho
print(MSE_func(M_init,M_0,m,n))

# baseline 1
# source('nuclear_norm_opt.R')
tuning = 60 * (n/100)**0.5
stepsize = 0.3
M_hat_1 = Nuclear_opt_func(X1_index,X2_index,Y_obs,sample_size,m,n,tuning,stepsize,tor=1e-4,init=TRUE,M_input = M_obs/rho)
print(MSE_func(M_hat_1,M_0,m,n))


residual_1 = matrix(0,m,n)
for (i in 1:sample_size){
  residual_1[X1_index[i],X2_index[i]] = Y_obs[i] - M_hat_1[X1_index[i],X2_index[i]]
}
sigma_hat = mean(residual_1^2)^(0.5)
temp_H = matrix(rnorm(m*n,sd = sigma_hat),m,n)


print(t(svd(M_hat_1_fix)$v[1:n,1:s]) %*% svd(residual_1_fix)$v)
print(svd(residual_1)$d)
print(svd(temp_H)$d)
print(sum(svd(residual_1)$d))
print(sum(svd(temp_H)$d))
print(m^(0.5)*sigma_hat *(1+p^0.5))
print(m^(0.5)*sigma_hat *(1-p^0.5))
error_u_1 = 1 - abs(colSums(svd(M_hat_1)$u[1:m,1:r] * svd(M_0)$u[1:m,1:r]))
error_v_1 = 1 - abs(colSums(svd(M_hat_1)$v[1:n,1:r] * svd(M_0)$v[1:n,1:r]))
error_d_1 = abs(svd(M_hat_1)$d[1:r] - svd(M_0)$d[1:r])
print(error_u_1)
print(error_v_1)
print(error_d_1)
######################################

tuning = 10
gamma = 5
M_hat_1_fix = Nuclear_opt_modified_func(X1_index,X2_index,Y_obs,sample_size,m,n,tuning,gamma,tor=1e-1)
print(MSE_func(M_hat_1_fix,M_0,m,n))


residual_1_fix = matrix(0,m,n)
for (i in 1:sample_size){
  residual_1_fix[X1_index[i],X2_index[i]] = Y_obs[i] - M_hat_1_fix[X1_index[i],X2_index[i]]
}
sigma_hat = mean(residual_1_fix^2)^(0.5)
temp_H = matrix(rnorm(m*n,sd = sigma_hat),m,n)


print(t(svd(M_hat_1_fix)$v[1:n,1:s]) %*% svd(residual_1_fix)$v)
print(svd(residual_1_fix)$d)
print(svd(temp_H)$d)
print(sum(svd(residual_1_fix)$d))
print(sum(svd(temp_H)$d))
print(m^(0.5)*sigma_hat *(1+p^0.5))
print(m^(0.5)*sigma_hat *(1-p^0.5))
error_u_1_fix = 1 - abs(colSums(svd(M_hat_1_fix)$u[1:m,1:r] * svd(M_0)$u[1:m,1:r]))
error_v_1_fix = 1 - abs(colSums(svd(M_hat_1_fix)$v[1:n,1:r] * svd(M_0)$v[1:n,1:r]))
error_d_1_fix = abs(svd(M_hat_1_fix)$d[1:r] - svd(M_0)$d[1:r])
print(error_u_1_fix)
print(error_v_1_fix)
print(error_d_1_fix)
##################################################

tuning = 60* (n/100)**0.5
gamma = 1
M_hat_1_new = Nuclear_opt_new_func(X1_index,X2_index,Y_obs,sample_size,m,n,tuning,gamma,tor=1e-4,init=TRUE,M_input = M_obs/rho)
print(MSE_func(M_hat_1_new,M_0,m,n))


residual_1_new = matrix(0,m,n)
for (i in 1:sample_size){
  residual_1_new[X1_index[i],X2_index[i]] = Y_obs[i] - M_hat_1_new[X1_index[i],X2_index[i]]
}
sigma_hat = mean(residual_1_new^2)^(0.5)
temp_H = matrix(rnorm(m*n,sd = sigma_hat),m,n)


print(t(svd(M_hat_1_new)$v[1:n,1:s]) %*% svd(residual_1_new)$v)
print(svd(residual_1_new)$d)
print(svd(temp_H)$d)
print(sum(svd(residual_1_new)$d))
print(sum(svd(temp_H)$d))
print(m^(0.5)*sigma_hat *(1+p^0.5))
print(m^(0.5)*sigma_hat *(1-p^0.5))
error_u_1_new = 1 - abs(colSums(svd(M_hat_1_new)$u[1:m,1:r] * svd(M_0)$u[1:m,1:r]))
error_v_1_new = 1 - abs(colSums(svd(M_hat_1_new)$v[1:n,1:r] * svd(M_0)$v[1:n,1:r]))
error_d_1_new = svd(M_hat_1_new)$d[1:r] - svd(M_0)$d[1:r]
print(error_u_1_new)
print(error_v_1_new)
print(error_d_1_new)







M_temp = svd(M_hat_1)$u[1:m,1:r] %*% diag(svd(M_hat_1_fix)$d[1:r]) %*% t(svd(M_hat_1)$v[1:n,1:r])

print(MSE_func(M_temp,M_0,m,n))
# baseline 2 

s = 5
# source('matrix_factorization_opt.R')
tuning = 0
M_hat_2 = Matrix_factor_func(X1_index,X2_index,Y_obs,sample_size,m,n,tuning,s,step_size = 10,tor=1e-4)
print(MSE_func(M_hat_2,M_0,m,n))

residual_2 = matrix(0,m,n)
for (i in 1:sample_size){
  residual_2[X1_index[i],X2_index[i]] = Y_obs[i] - M_hat_2[X1_index[i],X2_index[i]]
}
sigma_hat = mean(residual_2^2)^(0.5)
temp_H = matrix(rnorm(m*n,sd = sigma_hat),m,n)

print(t(svd(M_hat_2)$v[1:n,1]) %*% svd(residual_2)$v)
print(svd(residual_2)$d)
print(svd(temp_H)$d)
print(sum(svd(residual_2)$d))
print(sum(svd(temp_H)$d))
print(m^(0.5)*sigma_hat *(1+p^0.5))
print(m^(0.5)*sigma_hat *(1-p^0.5))
error_u_2 = 1 - abs(colSums(svd(M_hat_2)$u[1:m,1:s] * svd(M_0)$u[1:m,1:s]))
error_v_2 = 1 - abs(colSums(svd(M_hat_2)$v[1:n,1:s] * svd(M_0)$v[1:n,1:s]))
error_d_2 = svd(M_hat_2)$d[1:s] - svd(M_0)$d[1:s]
print(error_u_2)
print(error_v_2)
print(error_d_2)
print(mean((svd(M_hat_2)$u[1:m,1:s] %*% diag((svd(M_hat_2)$d[1:s])**0.5) - svd(M_0)$u[1:m,1:s] %*% diag((svd(M_0)$d[1:s])**0.5))**2))
print(mean((svd(M_hat_2)$v[1:n,1:s] %*% diag((svd(M_hat_2)$d[1:s])**0.5) - svd(M_0)$v[1:n,1:s] %*% diag((svd(M_0)$d[1:s])**0.5))**2))
###############################################

tuning1 = 0
tuning2 = 30
M_hat_2_fix = Matrix_factor_modified_func(X1_index,X2_index,Y_obs,sample_size,m,n,tuning1,tuning2,s,step_size = 1)
print(MSE_func(M_hat_2_fix,M_0,m,n))

residual_2_fix = matrix(0,m,n)
for (i in 1:sample_size){
  residual_2_fix[X1_index[i],X2_index[i]] = Y_obs[i] - M_hat_2_fix[X1_index[i],X2_index[i]]
}
sigma_hat = mean(residual_2_fix^2)^(0.5)
temp_H = matrix(rnorm(m*n,sd = sigma_hat),m,n)


print(t(svd(M_hat_2_fix)$v[1:n,1:s]) %*% svd(residual_2_fix)$v)
print(svd(residual_2_fix)$d)
print(svd(temp_H)$d)
print(sum(svd(residual_2_fix)$d))
print(sum(svd(temp_H)$d))
print(m^(0.5)*sigma_hat *(1+p^0.5))
print(m^(0.5)*sigma_hat *(1-p^0.5))
error_u_2_fix = 1 - abs(colSums(svd(M_hat_2_fix)$u[1:m,1:s] * svd(M_0)$u[1:m,1:s]))
error_v_2_fix = 1 - abs(colSums(svd(M_hat_2_fix)$v[1:n,1:s] * svd(M_0)$v[1:n,1:s]))
error_d_2_fix = svd(M_hat_2_fix)$d[1:s] - svd(M_0)$d[1:s]
print(error_u_2_fix)
print(error_v_2_fix)
print(error_d_2_fix)
print(mean((svd(M_hat_2_fix)$u[1:m,1:s] %*% diag((svd(M_hat_2_fix)$d[1:s])**0.5) - svd(M_0)$u[1:m,1:s] %*% diag((svd(M_0)$d[1:s])**0.5))**2))
print(mean((svd(M_hat_2_fix)$v[1:n,1:s] %*% diag((svd(M_hat_2)$d[1:s])**0.5) - svd(M_0)$v[1:n,1:s] %*% diag((svd(M_0)$d[1:s])**0.5))**2))

M_temp = svd(M_hat_2)$u[1:m,1:s] %*% diag(svd(M_hat_2_fix)$d[1:s]) %*% t(svd(M_hat_2)$v[1:n,1:s])
print(MSE_func(M_temp,M_0,m,n))


#######################
tuning1 = 0
tuning2 = 5
M_hat_2_new = Matrix_factor_new_func(X1_index,X2_index,Y_obs,sample_size,m,n,tuning1,tuning2,s,init=TRUE,M_input=M_init,step_size = 1,itertime=30000,tor=1e-4)
print(MSE_func(M_hat_2_new,M_0,m,n))
residual_2_new = matrix(0,m,n)
for (i in 1:sample_size){
  residual_2_new[X1_index[i],X2_index[i]] = Y_obs[i] - M_hat_2_new[X1_index[i],X2_index[i]]
}
sigma_hat = mean(residual_2_new^2)^(0.5)
temp_H = matrix(rnorm(m*n,sd = sigma_hat),m,n)

print(t(svd(M_hat_2_new)$v[1:n,1:s]) %*% svd(residual_2_new)$v)
print(svd(residual_2_new)$d)
print(svd(temp_H)$d)
print(sum(svd(residual_2_new)$d))
print(sum(svd(temp_H)$d))
print(m^(0.5)*sigma_hat *(1+p^0.5))
print(m^(0.5)*sigma_hat *(1-p^0.5))
error_u_2_new = 1 - abs(colSums(svd(M_hat_2_new)$u[1:m,1:s] * svd(M_0)$u[1:m,1:s]))
error_v_2_new = 1 - abs(colSums(svd(M_hat_2_new)$v[1:n,1:s] * svd(M_0)$v[1:n,1:s]))
error_d_2_new = svd(M_hat_2_new)$d[1:s] - svd(M_0)$d[1:s]
print(error_u_2_new)
print(error_v_2_new)
print(error_d_2_new)
print(mean((svd(M_hat_2_new)$u[1:m,1:s] %*% diag((svd(M_hat_2_new)$d[1:s])**0.5) - svd(M_0)$u[1:m,1:s] %*% diag((svd(M_0)$d[1:s])**0.5))**2))
print(mean((svd(M_hat_2_new)$v[1:n,1:s] %*% diag((svd(M_hat_2_new)$d[1:s])**0.5) - svd(M_0)$v[1:n,1:s] %*% diag((svd(M_0)$d[1:s])**0.5))**2))

M_temp = svd(M_hat_2)$u[1:m,1:s] %*% diag(svd(M_hat_2_new)$d[1:s]) %*% t(svd(M_hat_2)$v[1:n,1:s])
print(MSE_func(M_temp,M_0,m,n))

M_obs_hat = matrix(0,m,n)
H_obs = matrix(0,m,n)
for (i in 1:sample_size){
  M_obs_hat[X1_index[i],X2_index[i]] =  M_0[X1_index[i],X2_index[i]] - M_hat_2_new[X1_index[i],X2_index[i]]
  H_obs[X1_index[i],X2_index[i]] =  H[X1_index[i],X2_index[i]]
}
svd(M_obs_hat)$d
svd(H_obs)$d
svd(M_obs_hat+H_obs)$d




