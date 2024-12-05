setwd('/home/yuanyuanbenben/project_dmc/matrix_test/real_data_example')
source('baseline_methods.R')
source('proposed_method.R')

train_data = read.csv('data/case3_traindata.csv')
X1_train = train_data$X1_train
X2_train = train_data$X2_train
Y_train = train_data$Y_train
validation_data = read.csv('data/case3_validationdata.csv')
X1_validation = validation_data$X1_validation
X2_validation = validation_data$X2_validation
Y_validation = validation_data$Y_validation
test_data = read.csv('data/case3_testdata.csv')
X1_test = test_data$X1_test
X2_test = test_data$X2_test
Y_test = test_data$Y_test



# setting
m = max(X1_train)
n = max(X2_train)
p = n/m
sample_size = length(X1_train)
rho = sample_size/m/n
M_train = matrix(0,m,n)
for (i in 1:sample_size) {
  M_train[X1_train[i],X2_train[i]] = Y_train[i]
}

stepsize1 = 100
stepsize2 = 20

residual_func <- function(M_1,X1,X2,Y,m,n){
  res_mat = matrix(0,m,n)
  sample_size = length(X1)
  for (i in 1:sample_size) {
    res_mat[X1[i],X2[i]] = M_1[X1[i],X2[i]] - Y[i]
  }
  return(svd(res_mat)$d)
}


diag_H_mat = matrix(0,n,100)
for (j in 1:100){
  print(j)
  temp_H1 = matrix(rnorm(m*n),m,n)
  temp_H = matrix(0,m,n)
  for (i in 1:n){
    temp_H[X1_train[i],X2_train[i]] =temp_H1[X1_train[i],X2_train[i]]
  }
  diag_H_mat[,j] = svd(temp_H)$d
}
diag_H = rowMeans(diag_H_mat)

s = 14

# M(0)
if (s == 1){
  M_init = svd(M_train)$u[1:m,1:s] %*% t(svd(M_train)$v[1:n,1:s]) * svd(M_train)$d[1] / rho
}else{
  M_init = svd(M_train)$u[1:m,1:s] %*% diag(svd(M_train)$d[1:s]) %*% t(svd(M_train)$v[1:n,1:s]) / rho
}


# our method
M_hat_our = Matrix_factor_new_func(X1_train,X2_train,Y_train,sample_size,m,n,0,stepsize1,s,diag_H,
                                   init=TRUE,M_input=M_init,step_size = 1,itertime=30000,penalty='None',tor=5e-4)
test_residual_our = residual_func(M_hat_our,X1_test,X2_test,Y_test,m,n)
validation_residual_our = residual_func(M_hat_our,X1_validation,X2_validation,Y_validation,m,n)
train_residual_our = residual_func(M_hat_our,X1_train,X2_train,Y_train,m,n)
index1 = (M_hat_our > 5)
M_hat_our[index1] = 5
index2 = (M_hat_our < 1)
M_hat_our[index2] =1
test_residual_our_modified = residual_func(M_hat_our,X1_test,X2_test,Y_test,m,n)
validation_residual_our_modified = residual_func(M_hat_our,X1_validation,X2_validation,Y_validation,m,n)
train_residual_our_modified = residual_func(M_hat_our,X1_train,X2_train,Y_train,m,n)


s = 15

# M(0)
if (s == 1){
  M_init = svd(M_train)$u[1:m,1:s] %*% t(svd(M_train)$v[1:n,1:s]) * svd(M_train)$d[1] / rho
}else{
  M_init = svd(M_train)$u[1:m,1:s] %*% diag(svd(M_train)$d[1:s]) %*% t(svd(M_train)$v[1:n,1:s]) / rho
}


# baseline1: F norm based on matrix factorization
M_hat_baseline1 = Matrix_factor_func(X1_train,X2_train,Y_train,sample_size,m,n,0,s,
                                     init=TRUE,M_input=M_init,step_size = stepsize2,itertime=30000,penalty='None',tor=5e-4)
validation_residual_baseline1 = residual_func(M_hat_baseline1,X1_validation,X2_validation,Y_validation,m,n)
test_residual_baseline1 = residual_func(M_hat_baseline1,X1_test,X2_test,Y_test,m,n)
train_residual_baseline1 = residual_func(M_hat_baseline1,X1_train,X2_train,Y_train,m,n)
index1 = (M_hat_baseline1 > 5)
M_hat_baseline1[index1] = 5
index2 = (M_hat_baseline1 < 1)
M_hat_baseline1[index2] =1
validation_residual_baseline1_modified = residual_func(M_hat_baseline1,X1_validation,X2_validation,Y_validation,m,n)
test_residual_baseline1_modified = residual_func(M_hat_baseline1,X1_test,X2_test,Y_test,m,n)
train_residual_baseline1_modified = residual_func(M_hat_baseline1,X1_train,X2_train,Y_train,m,n)

residual_1 = data.frame(test_residual_our,validation_residual_our,train_residual_our,test_residual_our_modified,validation_residual_our_modified,train_residual_our_modified)
write.csv(residual_1,"output/case3_nonconvex_residual_our.csv")

residual_2 = data.frame(test_residual_baseline1,validation_residual_baseline1,train_residual_baseline1,test_residual_baseline1_modified,validation_residual_baseline1_modified,train_residual_baseline1_modified)
write.csv(residual_2,"output/case3_nonconvex_residual_baseline1.csv")
 

