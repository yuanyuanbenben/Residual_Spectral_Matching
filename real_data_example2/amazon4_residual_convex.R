setwd('/home/yuanyuanbenben/project_dmc/matrix_test/real_data_example2')
source('baseline_methods.R')
source('proposed_method.R')

train_data = read.csv('data/case4_traindata.csv')
X1_train = train_data$X1_train
X2_train = train_data$X2_train
Y_train = train_data$Y_train
validation_data = read.csv('data/case4_validationdata.csv')
X1_validation = validation_data$X1_validation
X2_validation = validation_data$X2_validation
Y_validation = validation_data$Y_validation
test_data = read.csv('data/case4_testdata.csv')
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


# M(0)
M_init = M_train / rho

# our method
gamma = 1
tuning = 430
M_hat_our = Nuclear_opt_new_func(X1_train,X2_train,Y_train,sample_size,m,n,tuning,gamma,diag_H,init=TRUE,M_input = M_init,tor=5e-4)
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

# baseline2
stepsize = 0.1
tuning = 2300
M_hat_baseline1 = Nuclear_opt_func(X1_train,X2_train,Y_train,sample_size,m,n,tuning,stepsize,init=TRUE,M_input = M_init,tor=5e-4)
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
write.csv(residual_1,"output/case4_convex_residual_our.csv")

residual_2 = data.frame(test_residual_baseline1,validation_residual_baseline1,train_residual_baseline1,test_residual_baseline1_modified,validation_residual_baseline1_modified,train_residual_baseline1_modified)
write.csv(residual_2,"output/case4_convex_residual_baseline1.csv")


