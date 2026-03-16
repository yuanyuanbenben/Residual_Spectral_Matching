# Heteroscedastic nonconvex matrix factorization method for matrix completion
# This function implements a nonconvex optimization approach for matrix completion
# under heteroscedastic noise. It uses gradient descent on factor matrices U and V
# with spectral shrinkage of residuals to adapt to heteroscedastic noise structure.
#
# Inputs:
#   X1, X2: vectors of row and column indices for observed entries (length n)
#   Y: vector of observed values (length n)
#   n: number of observations
#   p, q: matrix dimensions (p rows, q columns)
#   lambda1, lambda2: regularization parameters
#   r: target rank
#   spectrum_reference_init: whether to use provided spectrum reference
#   spectrum_reference: precomputed singular values of noise pattern (if init=TRUE)
#   sigma: known noise level (FALSE to estimate from data)
#   beta: step size decay parameter (unused in current version)
#   itertime: maximum number of iterations
#   tor: convergence tolerance
#   init: whether to use provided initial matrix
#   M_input: initial matrix (if init=TRUE)
#   step_size: gradient descent step size
#   penalty: type of penalty ('F_norm', 'Balanced', or 'None')
#
# Output:
#   Estimated p x q matrix (U %*% t(V))

Matrix_factor_new_func_hetero <- function(X1,X2,Y,n,p,q,lambda1,lambda2,r,spectrum_reference_init=FALSE,spectrum_reference = 0,sigma=FALSE, beta=0.2,itertime=30000,tor=1e-4,init=FALSE,M_input=FALSE,step_size=10,penalty='F_norm'){
  threshold_func <- function(x) sapply(x, function(z) max(0,z))
  start <- Sys.time()
  
  if (init){
    svd_m = svd(M_input)
    if (r > 1){
      U = svd_m$u[1:p,1:r] %*% diag(svd_m$d[1:r])**0.5
      V = svd_m$v[1:q,1:r] %*% diag(svd_m$d[1:r])**0.5
    }
    else{
      U = svd_m$u[1:p,1:r] * svd_m$d[1:r]**0.5
      V = svd_m$v[1:q,1:r] * svd_m$d[1:r]**0.5
    }
  }
  else{
    Y_mat = matrix(0,p,q)
    for (i in 1:n){
      Y_mat[X1[i],X2[i]] = Y[i]
    }
    svd_y = svd(Y_mat)
    if (r > 1){
      U = svd_y$u[1:p,1:r] %*% diag((svd_y$d[1:r])**0.5)
      V = svd_y$v[1:q,1:r] %*% diag((svd_y$d[1:r])**0.5)
    }
    else{
      U = svd_y$u[1:p,1:r] * (svd_y$d[1:r]**0.5)
      V = svd_y$v[1:q,1:r] *(svd_y$d[1:r]**0.5)
    }
  }
  
  M = matrix(0,p,q)
  
  for (i in 1:n){
    M[X1[i],X2[i]] = Y[i]
  }
  temp = matrix(0,p,q)
  obj_value_before <- Inf
  grad_H = matrix(0,p,q)
  
  index = TRUE
  weight = rep(1,q)
  #weight[1:(r)] = 5 #/ log(iter*0.1+exp(1))
  #weight[(q-r):(q)] = 10 # * log(iter*0.1+exp(1))
  weight = weight/sum(weight) * q
  if (spectrum_reference_init == FALSE){
    diag_H_mat = matrix(0,q,100)
    for (j in 1:100){
      temp_H1 = matrix(rnorm(p*q),p,q)
      temp_H = matrix(0,p,q)
      for (i in 1:n){
        temp_H[X1[i],X2[i]] =temp_H1[X1[i],X2[i]]
      }
      diag_H_mat[,j] = svd(temp_H)$d
    }
    diag_H = rowMeans(diag_H_mat)
  }
  else{
    diag_H = spectrum_reference
  }
  # random_index_mat = matrix(rnorm(p*q),p,q)
  # noise_index = rbinom(p,1,0.2) + 0.2
  R1 = round(min(p,q)/3)
  R2 = min(p,q) - round(min(p,q)/3)
  # sum_lambda = sum(svd(random_index_mat*noise_index/0.32**0.5)$d[R1:R2])
  sum_lambda = sum(diag_H[R1:R2])
  sigma_hat = 0
  for (iter in 1:itertime){
    M_hat = U%*%t(V)
    
    for (i in 1:n){
      temp[X1[i],X2[i]] = M_hat[X1[i],X2[i]]
    }
    residual_mat = temp - M
    
    svd_residual = svd(residual_mat)
    if (sigma==FALSE){
      sigma_hat =  sum(svd_residual$d[R1:R2]) / sum_lambda 
    }else{
      sigma_hat = sigma
    }
    bias = (svd_residual$d-diag_H*sigma_hat) * weight
    grad_residual =  -svd_residual$u %*% diag(bias) %*% t(svd_residual$v) * lambda2 
    for (i in 1:n){
      grad_H[X1[i],X2[i]] = grad_residual[X1[i],X2[i]]
    }
    residual_mat_ = - grad_H
    # residual_mat_ = - grad_residual * (n/p/q)**0.5
    if (penalty=='F_norm'){
      grad_U = residual_mat_ %*% V / n + lambda1 * U / p / r
      grad_V = t(residual_mat_) %*% U / n + lambda1 * V / q / r
    }
    if (penalty=='Balanced'){
      grad_U = residual_mat_ %*% V / n + lambda1 * U %*% (t(U)%*%U - t(V)%*%V) / (p*q)^0.5 / r
      grad_V = t(residual_mat_) %*% U / n + lambda1 * V  %*% (t(V)%*%V - t(U)%*%U) / (p*q)^0.5 / r
    }
    if (penalty=='None'){
      grad_U = residual_mat_ %*% V / n 
      grad_V = t(residual_mat_) %*% U / n
    }
    
    U = U - step_size * grad_U
    V = V - step_size * grad_V
    
    if (as.integer(iter/20)*20==iter){
      print(iter)
      obj_value <- sum(residual_mat**2) / n
      print(obj_value)
      if (abs(obj_value-obj_value_before) < tor){
        break
      }
      obj_value_before <- obj_value
    }
  }
  end <- Sys.time()
  print(difftime(end, start, units = "sec"))
  return(U %*% t(V))
}

# Heteroscedastic convex relaxed nuclear norm optimization for matrix completion
# This function implements a convex optimization approach for matrix completion
# under heteroscedastic noise. It uses proximal gradient descent with singular value
# thresholding, incorporating spectral shrinkage adapted to heteroscedastic noise.
#
# Inputs:
#   X1, X2: vectors of row and column indices for observed entries (length n_t)
#   Y: vector of observed values (length n_t)
#   n_t: number of observations
#   p, q: matrix dimensions (p rows, q columns)
#   lambda: regularization parameter for nuclear norm penalty
#   gamma: step size parameter
#   spectrum_reference_init: whether to use provided spectrum reference
#   spectrum_reference: precomputed singular values of noise pattern (if init=TRUE)
#   sigma: known noise level (FALSE to estimate from data)
#   beta: unused parameter (kept for compatibility)
#   itertime: maximum number of iterations
#   constant: whether to use constant step size (unused)
#   tor: convergence tolerance
#   init: whether to use provided initial matrix
#   M_input: initial matrix (if init=TRUE)
#
# Output:
#   Estimated p x q matrix after nuclear norm regularization

Nuclear_opt_new_func_hetero <- function(X1,X2,Y,n_t,p,q,lambda,gamma,spectrum_reference_init=FALSE,spectrum_reference = 0,sigma=FALSE,beta=0.2,itertime=30000,constant=TRUE,tor=1e-4,init=FALSE,M_input=FALSE){
  Lipschitz_base_func <- function(X1,X2,n_t,p,q){
    L_mat <- matrix(0,p,q)
    for (i in 1:n_t) {
      L_mat[X1[i],X2[i]] <- L_mat[X1[i],X2[i]] + 1
    }
    return(2*norm(L_mat,type = "F"))
  }
  
  threshold_func <- function(x) sapply(x, function(z) max(0,z))
  
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
  weight = rep(1,q)
  #weight[1:(r)] = 5 #/ log(iter*0.1+exp(1))
  #weight[(q-r):(q)] = 10 # * log(iter*0.1+exp(1))
  weight = weight/sum(weight) * q
  if (spectrum_reference_init == FALSE){
    diag_H_mat = matrix(0,q,100)
    for (j in 1:100){
      temp_H1 = matrix(rnorm(p*q),p,q)
      temp_H = matrix(0,p,q)
      for (i in 1:n){
        temp_H[X1[i],X2[i]] =temp_H1[X1[i],X2[i]]
      }
      diag_H_mat[,j] = svd(temp_H)$d
    }
    diag_H = rowMeans(diag_H_mat)
  }
  else{
    diag_H = spectrum_reference
  }
  # iteration
  for (iter in 1:itertime) {
    for (i in 1:n_t){
      residual_mat[X1[i],X2[i]] = M[X1[i],X2[i]] - Y[i]
    }
    #sigma_hat = mean(residual_mat**2)**0.5 * (1-0.5/log(iter*0.01+exp(1)))
    svd_residual = svd(residual_mat)
    #sigma_hat =  sum(svd_residual$d[R1:R2]) / sum_lambda /iter + sigma_hat*(iter - 1)/iter
    if (sigma==FALSE){
      sigma_hat =  sum(svd_residual$d[R1:R2]) / sum_lambda * (p*q/n)**0.5
    }else{
      sigma_hat = sigma
    }
    #svd_residual = irlba(residual_mat,r)
    #svd_residual2 = irlba(residual_mat,r,smallest = TRUE)
    
    # temp_H1 = matrix(rnorm(p*q),p,q)
    # temp_H = matrix(0,p,q)
    # for (i in 1:n_t){
    #   temp_H[X1[i],X2[i]] = temp_H1[X1[i],X2[i]]
    # }
    # diag_H = svd(temp_H)$d
    #diag_H = irlba(temp_H,r)$d
    #diag_H2 = irlba(temp_H,r,smallest = TRUE)$d
    bias = (svd_residual$d-diag_H*sigma_hat) * weight
    #bias = c(svd_residual$d,svd_residual2$d)-c(diag_H,diag_H2)*sigma_hat / (n/p/q)**0.5
    #weight = rep(0,2*r)
    # weight = rep(1,q)
    # #weight[1:(r)] = 10 / log(iter*0.1+exp(1))
    # #weight[(q-r):(q)] = 1 * log(iter*0.1+exp(1))
    # weight = weight/sum(weight)
    # bias = bias * weight * q
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
      if (abs(obj_value/n_t-obj_value_before/n_t) < tor){
        break
      } 
      obj_value_before <- obj_value
    }
  }
  end <- Sys.time()
  print(difftime(end, start, units = "sec"))
  return(M)
}





