
Matrix_factor_func <- function(X1,X2,Y,n,p,q,lambda,r,itertime=30000,tor=1e-4,init=FALSE,M_input=FALSE,step_size=10,penalty='F_norm'){
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
  for (iter in 1:itertime){
    M_hat = U%*%t(V)
    for (i in 1:n){
      temp[X1[i],X2[i]] = M_hat[X1[i],X2[i]]
    }
    residual_mat = temp - M
    if (penalty=='F_norm'){
      grad_U = residual_mat %*% V / n + lambda * U / p / r
      grad_V = t(residual_mat) %*% U / n + lambda * V / q / r
    }
    if (penalty=='Balanced'){
      grad_U = residual_mat %*% V / n + lambda * U %*% (t(U)%*%U - t(V)%*%V) / (p*q)^0.5 / r
      grad_V = t(residual_mat) %*% U / n + lambda * V  %*% (t(V)%*%V - t(U)%*%U) / (p*q)^0.5 / r
    }
    if (penalty=='None'){
      grad_U = residual_mat %*% V / n
      grad_V = t(residual_mat) %*% U / n
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

Nuclear_opt_func <- function(X1,X2,Y,n_t,p,q,lambda,stepsize,itertime=30000,constant=TRUE,tor=1e-4,init=FALSE,M_input=FALSE){
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
    grad_mat <- matrix(0,p,q)
    tem_mat <- (inner_pro-Y)
    for (i in 1:n_t) {
      grad_mat[X1[i],X2[i]] <- grad_mat[X1[i],X2[i]] + tem_mat[i]
    }
    return(2*grad_mat)
  }
  start <- Sys.time()
  t = 1
  if (init){
    M = M_input
  }
  else{
    M = matrix(rnorm(p*q),p,q)
  }
  obj_value_before <- Inf
  for (iter in 1:itertime) {
    inner_pro <- baseline_inner_pro_func_M(X1,X2,M,n_t)
    grad_M <- baseline_grad_func(Y,inner_pro,X1,X2,n_t,p,q)
    svd_G <- svd(M - stepsize*grad_M)
    index = svd_G$d < lambda
    sigma_G = svd_G$d
    sigma_G[index] = 0
    M <- svd_G$u%*%diag(sigma_G)%*%t(svd_G$v)
    if (as.integer(iter/20)*20==iter){
      print(iter)
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
