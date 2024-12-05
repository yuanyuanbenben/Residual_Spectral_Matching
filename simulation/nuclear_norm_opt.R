# using FISTA algorithm
library(irlba)
# compute Lipschitz constant
Lipschitz_base_func <- function(X1,X2,n_t,p,q){
  L_mat <- matrix(0,p,q)
  for (i in 1:n_t) {
    L_mat[X1[i],X2[i]] <- L_mat[X1[i],X2[i]] + 1
  }
  return(2*norm(L_mat,type = "F"))
}

threshold_func <- function(x) sapply(x, function(z) max(0,z))

gaussian_kernel <- function(x1,x2,h){
  return(exp(-1/h*(x1-x2)**2))
}

#score_match <- function(x,h,lambda_plus,lambda_){
#  return(-1/2/(lambda_plus-x)+1/2/(x-lambda_)-1/x - 2/h*x)
#}

score_match <- function(x,h,lambda_plus,lambda_){
  return(-x/(lambda_plus**2-x**2)+x/(x**2-lambda_**2)-1/x - 2/h*x)
}
# compute M[x1,x2]
baseline_inner_pro_func_M <- function(X1,X2,M,n_t){
  ret_vec <- 1:n_t
  for (i in 1:n_t) {
    ret_vec[i] <- M[X1[i],X2[i]]
  }
  return(ret_vec)
}

baseline_obj_func <- function(Y,inner_pro){
  return(sum((Y-inner_pro)*(Y-inner_pro)))
}

# compute gradient
baseline_grad_func <- function(Y,inner_pro,X1,X2,n_t,p,q){
  # Y, inner_pro T*n_t
  # X T*n_t*p*q
  grad_mat <- matrix(0,p,q)
  tem_mat <- (inner_pro-Y)
  for (i in 1:n_t) {
    grad_mat[X1[i],X2[i]] <- grad_mat[X1[i],X2[i]] + tem_mat[i]
  }
  return(2*grad_mat)
}

# main function
Nuclear_opt_func <- function(X1,X2,Y,n_t,p,q,lambda,itertime=30000,constant=TRUE,tor=1,init=FALSE,M_input=FALSE){
  set.seed(20231213)
  start <- Sys.time()
  # initial
  L <- Lipschitz_base_func(X1,X2,n_t,p,q) / 5
  #print(L)
  t = 1
  if (init){
    M = M_input
  }
  else{
    M = matrix(rnorm(p*q),p,q) 
  }
  #N <- M
  obj_value_before <- Inf
  # iteration
  for (iter in 1:itertime) {
    #inner_pro <- baseline_inner_pro_func_M(X1,X2,N,n_t)
    inner_pro <- baseline_inner_pro_func_M(X1,X2,M,n_t)
    #grad_N <- baseline_grad_func(Y,inner_pro,X1,X2,n_t,p,q)
    grad_M <- baseline_grad_func(Y,inner_pro,X1,X2,n_t,p,q)
    #svd_G <- svd(N - 1/L*grad_N)
    svd_G <- svd(M - 1/L*grad_M)
    #M_ <- svd_G$u%*%diag(threshold_func(svd_G$d-lambda/L))%*%t(svd_G$v)
    index = svd_G$d < lambda 
    sigma_G = svd_G$d
    sigma_G[index] = 0
    M <- svd_G$u%*%diag(sigma_G)%*%t(svd_G$v)
    #M <- M_
    #M <- svd_G$u%*%diag(threshold_func(svd_G$d-lambda/L))%*%t(svd_G$v)
    #t_ <- (1+sqrt(1+4*t^2))/2
    #N <- M + (t-1)/t_*(M_-M)
    #M <- M_
    #t <- t_
    if (as.integer(iter/20)*20==iter){
      print(iter)
      obj_value <- baseline_obj_func(Y,inner_pro)
      print(obj_value/n_t)
      if (abs(obj_value-obj_value_before) < tor){
        break
      } 
      obj_value_before <- obj_value
    }
  }
  end <- Sys.time()
  print(difftime(end, start, units = "sec"))
  return(M)
}



# main function
Nuclear_opt_modified_func <- function(X1,X2,Y,n_t,p,q,lambda,gamma,beta=0.4,itertime=30000,constant=TRUE,tor=1,init=FALSE,M_input=FALSE){
  set.seed(20231213)
  start <- Sys.time()
  # initial
  L <- Lipschitz_base_func(X1,X2,n_t,p,q) / 5
  #print(L)
  t = 1
  if (init){
    M = M_input
  }
  else{
    M = matrix(rnorm(p*q),p,q)
  }
  N <- M
  obj_value_before <- Inf
  residual_mat = matrix(0,p,q)
  grad_H = matrix(0,p,q)
  # iteration
  for (iter in 1:itertime) {
    if (iter >40){
      gamma_ = gamma
    }
    else{gamma_ = 0}
    inner_pro <- baseline_inner_pro_func_M(X1,X2,N,n_t)
    grad_N <- baseline_grad_func(Y,inner_pro,X1,X2,n_t,p,q)
    for (i in 1:n_t){
      residual_mat[X1[i],X2[i]] = N[X1[i],X2[i]] - Y[i]
    }
    sigma_hat = mean(residual_mat**2)**0.5
    svd_residual = svd(residual_mat)
    #sigma_hat =  sum(svd_residual$d[51:100]) / 441.75
    # temp_H = matrix(rnorm(p*q),p,q)
    # diag_H = svd(temp_H)$d
    # bias = svd_residual$d-diag_H*sigma_hat
    # bias_index = abs(bias) < 0.2
    # bias[bias_index] = 0
    # grad_residual =  svd_residual$u %*% diag(bias) %*% t(svd_residual$v)
    
    lambda_plus = p**(0.5) * sigma_hat * (1 + (q/p)**0.5)
    lambda_ = p**0.5 * sigma_hat * (1 - (q/p)**0.5)
    h = beta * (lambda_plus - lambda_)
    old_sigma = svd_residual$d
    old_sigma[old_sigma>lambda_plus] = lambda_plus -  h * 0.01
    old_sigma[old_sigma<lambda_] = lambda_ +  h * 0.01
    for (inner_iter in 1:10){
      K = sapply(old_sigma,gaussian_kernel,x2=old_sigma,h=h) /q 
      S = sapply(old_sigma, score_match,h=h,lambda_plus=lambda_plus,lambda_=lambda_)
      gradient_sigma = c(S %*% K + 2/h * old_sigma * rowSums(K))
      old_sigma = old_sigma + gradient_sigma
      old_sigma[old_sigma>lambda_plus] = lambda_plus - h * 0.01
      old_sigma[old_sigma<lambda_] = lambda_ +  h * 0.01
    }
    grad_residual =  svd_residual$u %*% diag(old_sigma - svd_residual$d) %*% t(svd_residual$v)
    for (i in 1:n){
      grad_H[X1[i],X2[i]] = grad_residual[X1[i],X2[i]]
    }
    svd_G <- svd(N - 1/L*grad_N + gamma_ * grad_H)
    M_ <- svd_G$u%*%diag(threshold_func(svd_G$d-lambda/L))%*%t(svd_G$v)
    t_ <- (1+sqrt(1+4*t^2))/2
    N <- M + (t-1)/t_*(M_-M)
    M <- M_
    t <- t_
    if (as.integer(iter/20)*20==iter){
      print(iter)
      obj_value <- baseline_obj_func(Y,inner_pro)
      print(obj_value)
      if (abs(obj_value-obj_value_before) < tor){
        break
      } 
      obj_value_before <- obj_value
    }
  }
  end <- Sys.time()
  print(difftime(end, start, units = "sec"))
  return(M)
}




# main function
Nuclear_opt_new_func <- function(X1,X2,Y,n_t,p,q,lambda,gamma,beta=0.2,itertime=30000,constant=TRUE,tor=1,init=FALSE,M_input=FALSE){
  set.seed(20231213)
  start <- Sys.time()
  # initial
  #print(L)
  t = 1
  if (init){
    M = M_input
  }
  else{
    M = matrix(rnorm(p*q),p,q) 
  }
  N <- M
  obj_value_before <- Inf
  residual_mat = matrix(0,p,q)
  grad_H = matrix(0,p,q)
  random_index_mat = matrix(rnorm(p*q),p,q)
  R1 = round(min(p,q)/3)
  R2 = min(p,q) - round(min(p,q)/3)
  sum_lambda = sum(svd(random_index_mat)$d[R1:R2])
  sigma_hat = 0
  # iteration
  for (iter in 1:itertime) {
    for (i in 1:n_t){
      residual_mat[X1[i],X2[i]] = M[X1[i],X2[i]] - Y[i]
    }
    #sigma_hat = mean(residual_mat**2)**0.5 * (1-0.5/log(iter*0.01+exp(1)))
    svd_residual = svd(residual_mat)
    #sigma_hat =  sum(svd_residual$d[R1:R2]) / sum_lambda /iter + sigma_hat*(iter - 1)/iter
    sigma_hat =  sum(svd_residual$d[R1:R2]) / sum_lambda
    #svd_residual = irlba(residual_mat,r)
    #svd_residual2 = irlba(residual_mat,r,smallest = TRUE)
    
    #sigma_hat =  sum(svd_residual$d[26:50]) / 347.59 
    # sigma_hat =  sum(svd_residual$d[51:100]) / 441.75
    # sigma_hat =  sum(svd_residual$d[151:300]) / 860.44
    temp_H1 = matrix(rnorm(p*q),p,q)
    temp_H = matrix(0,p,q)
    for (i in 1:n_t){
      temp_H[X1[i],X2[i]] = temp_H1[X1[i],X2[i]]
    }
    diag_H = svd(temp_H)$d
    #diag_H = irlba(temp_H,r)$d
    #diag_H2 = irlba(temp_H,r,smallest = TRUE)$d
    bias = svd_residual$d-diag_H*sigma_hat
    #bias = c(svd_residual$d,svd_residual2$d)-c(diag_H,diag_H2)*sigma_hat / (n/p/q)**0.5
    #weight = rep(0,2*r)
    weight = rep(1,q)
    #weight[1:(r)] = 10 / log(iter*0.1+exp(1))
    #weight[(q-r):(q)] = 1 * log(iter*0.1+exp(1))
    weight = weight/sum(weight)
    bias = bias * weight * q
    grad_residual =  -svd_residual$u %*% diag(bias) %*% t(svd_residual$v) 
    for (i in 1:n_t){
      grad_H[X1[i],X2[i]] = grad_residual[X1[i],X2[i]]
    }
    svd_G <- svd(M + gamma * grad_H)
    index = svd_G$d < lambda 
    sigma_G = svd_G$d
    sigma_G[index] = 0
    M_ <- svd_G$u%*%diag(sigma_G)%*%t(svd_G$v)
    M <- M_
    if (as.integer(iter/20)*20==iter){
      print(iter)
      inner_pro <- baseline_inner_pro_func_M(X1,X2,M,n_t)
      obj_value <- baseline_obj_func(Y,inner_pro)
      print(obj_value/n_t)
      if (abs(obj_value-obj_value_before) < tor){
        break
      } 
      obj_value_before <- obj_value
    }
  }
  end <- Sys.time()
  print(difftime(end, start, units = "sec"))
  return(M)
}




