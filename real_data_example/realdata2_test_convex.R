library(foreach)
library(doParallel)

setwd('/home/yuanyuanbenben/project_dmc/matrix_test/real_data_example')
source('baseline_methods.R')
source('proposed_method.R')

train_data = read.csv('data/case2_traindata.csv')
X1_train = train_data$X1_train
X2_train = train_data$X2_train
Y_train = train_data$Y_train
validation_data = read.csv('data/case2_validationdata.csv')
X1_validation = validation_data$X1_validation
X2_validation = validation_data$X2_validation
Y_validation = validation_data$Y_validation
test_data = read.csv('data/case2_testdata.csv')
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


error_measure_func <- function(M_1,X1,X2,Y,m,n){
  Y_hat = rep(0,length(Y))
  for (i in 1:length(Y)) {
    Y_hat[i] = M_1[X1[i],X2[i]]
  }
  f_norm = sqrt(mean((Y_hat - Y)**2))
  maximal_norm = max(abs(Y_hat - Y))
  return(c(f_norm,maximal_norm))
}


cl.cores = detectCores(logical = F)
cl <- makeCluster(10)
registerDoParallel(cl)

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

# baseline compare
loss_total <- foreach(tuning = c(100,130,160,200,230,260,300,330,360,400),.verbose=TRUE,.combine = rbind) %dopar% {
  
  # M(0)
  M_init = M_train / rho
  
  # our method
  gamma = 1
  M_hat_our = Nuclear_opt_new_func(X1_train,X2_train,Y_train,sample_size,m,n,tuning,gamma,diag_H,init=TRUE,M_input = M_init,tor=5e-4)
  validation_loss_our = error_measure_func(M_hat_our,X1_validation,X2_validation,Y_validation,m,n)
  test_loss_our = error_measure_func(M_hat_our,X1_test,X2_test,Y_test,m,n)
  index1 = (M_hat_our > 5)
  M_hat_our[index1] = 5
  index2 = (M_hat_our < 1)
  M_hat_our[index2] =1
  validation_loss_our_modified = error_measure_func(M_hat_our,X1_validation,X2_validation,Y_validation,m,n)
  test_loss_our_modified = error_measure_func(M_hat_our,X1_test,X2_test,Y_test,m,n)
  
  # baseline1: F norm based on matrix factorization
  stepsize = 0.2
  M_hat_baseline1 = Nuclear_opt_func(X1_train,X2_train,Y_train,sample_size,m,n,tuning,stepsize,init=TRUE,M_input = M_init,tor=5e-4)
  validation_loss_baseline1 = error_measure_func(M_hat_baseline1,X1_validation,X2_validation,Y_validation,m,n)
  test_loss_baseline1 = error_measure_func(M_hat_baseline1,X1_test,X2_test,Y_test,m,n)
  index1 = (M_hat_baseline1 > 5)
  M_hat_baseline1[index1] = 5
  index2 = (M_hat_baseline1 < 1)
  M_hat_baseline1[index2] =1
  validation_loss_baseline1_modified = error_measure_func(M_hat_baseline1,X1_validation,X2_validation,Y_validation,m,n)
  test_loss_baseline1_modified = error_measure_func(M_hat_baseline1,X1_test,X2_test,Y_test,m,n)
  
  
  loss_return = rep(0,16)
  loss_return[1:2] = validation_loss_our
  loss_return[3:4] = test_loss_our
  loss_return[5:6] = validation_loss_our_modified
  loss_return[7:8] = test_loss_our_modified
  loss_return[9:10] = validation_loss_baseline1
  loss_return[11:12] = test_loss_baseline1
  loss_return[13:14] = validation_loss_baseline1_modified
  loss_return[15:16] = test_loss_baseline1_modified
  # loss_return[9:12] = loss_our2
  # loss_return[13:16] = loss_baseline2
  # loss_return[17:20] = loss_our3
  # loss_return[21:24] = loss_baseline3
  loss_return
}

loss_our = data.frame(loss_total[,1:8])
write.csv(loss_our,"output/case2_convex_loss_our1.csv")
loss_baseline1 = data.frame(loss_total[,9:16])
write.csv(loss_baseline1,"output/case2_convex_loss_baseline1.csv")
# loss_our2 = data.frame(loss_total[,9:12])
# write.csv(loss_our2,paste("output/nonconvex_loss_our2",m,n,rho,s,".csv",sep = "_"))
# loss_baseline2 = data.frame(loss_total[,13:16])
# write.csv(loss_baseline2,paste("output/nonconvex_loss_baseline2",m,n,rho,s,".csv",sep = "_"))
# loss_our3 = data.frame(loss_total[,17:20])
# write.csv(loss_our3,paste("output/nonconvex_loss_our3",m,n,rho,s,".csv",sep = "_"))
# loss_baseline3 = data.frame(loss_total[,21:24])
# write.csv(loss_baseline3,paste("output/nonconvex_loss_baseline3",m,n,rho,s,".csv",sep = "_"))

stopCluster(cl) 

