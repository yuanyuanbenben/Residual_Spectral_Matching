
# nonconvex method
Matrix_factor_new_func <- function(X1,X2,Y,n,p,q,lambda1,lambda2,r,diag_H,beta=0.2,itertime=30000,tor=1e-4,init=FALSE,M_input=FALSE,step_size=10,penalty='F_norm'){
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
      
      #U = matrix(rnorm(p*r),p,r)
      #V = matrix(rnorm(q*r),q,r)
    }
    else{
      U = svd_y$u[1:p,1:r] * (svd_y$d[1:r]**0.5)
      V = svd_y$v[1:q,1:r] *(svd_y$d[1:r]**0.5)
      #U = matrix(rnorm(p*r),p,r)
      #V = matrix(rnorm(q*r),q,r)
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
  random_index_mat = matrix(rnorm(p*q),p,q)
  R1 = round(min(p,q)/3)
  R2 = min(p,q) - round(min(p,q)/3)
  sum_lambda = sum(svd(random_index_mat)$d[R1:R2])
  sigma_hat = 0
  weight = rep(1,q)
  #weight[1:(r)] = 5 #/ log(iter*0.1+exp(1))
  #weight[(q-r):(q)] = 10 # * log(iter*0.1+exp(1))
  weight = weight/sum(weight) * q
 
  for (iter in 1:itertime){
    M_hat = U%*%t(V)
    
    # if (as.integer(iter/100)*100==iter){
    #   svd_m_hat = svd(M_hat)
    #   u = svd_m_hat$u[1:p,1:r]
    #   v = svd_m_hat$v[1:q,1:r] 
    #   u[u>5/p**0.5] = 5/p**0.5
    #   v[v>5/q**0.5] = 5/q**0.5
    #   u[u<-5/p**0.5] = -5/p**0.5
    #   v[v<-5/q**0.5] = -5/q**0.5
    #   U = u %*% diag(svd_m_hat$d[1:r]**0.5)
    #   V = v %*% diag(svd_m_hat$d[1:r]**0.5)
    #   M_hat = U%*%t(V)
    # }
    #if (as.integer(iter/100)*100==iter){
    #  if (index){
    #    noise_add = rnorm(n)
    #    for (i in 1:n){
    #    M[X1[i],X2[i]] = M[X1[i],X2[i]] + noise_add[i] }
    #    index = FALSE
    #  }
    #  else{
    #    for (i in 1:n){
    #      M[X1[i],X2[i]] = M[X1[i],X2[i]] - noise_add[i] 
    #    }
    #    index = TRUE
    #  }
    #}
    for (i in 1:n){
      temp[X1[i],X2[i]] = M_hat[X1[i],X2[i]]
    }
    residual_mat = temp - M
    
    
    
    #sigma_hat = mean(residual_mat**2)**0.5 * (1-0.5/log(iter*0.01+exp(1)))
    try(svd_residual <- svd(residual_mat))
    #svd_residual = irlba(residual_mat,r)
    #svd_residual2 = irlba(residual_mat,r,smallest = TRUE)
    #sigma_hat =  sum(svd_residual$d[R1:R2]) / sum_lambda /iter + sigma_hat*(iter - 1)/iter
    sigma_hat =  sum(svd_residual$d[R1:R2]) / sum_lambda * (p*q/n)**0.5
    #svd_residual = irlba(residual_mat)
    #sigma_hat =  sum(svd_residual$d[100:200]) /2031.137
    # temp_H1 = matrix(rnorm(p*q),p,q)
    # temp_H = matrix(0,p,q)
    # for (i in 1:n){
    #   temp_H[X1[i],X2[i]] = temp_H1[X1[i],X2[i]]
    # }
    # diag_H = svd(temp_H)$d
    #diag_H = irlba(temp_H,2)$d
    #diag_H2 = irlba(temp_H,r,smallest = TRUE)$d
    bias = (svd_residual$d-diag_H*sigma_hat) * weight
    #bias = c(svd_residual$d,svd_residual2$d)-c(diag_H,diag_H2)*sigma_hat / (n/p/q)**0.5
    #weight = rep(0,2*r)
    grad_residual =  -svd_residual$u %*% diag(bias) %*% t(svd_residual$v) * lambda2 
    #grad_residual =  -svd_residual$u %*%  t(svd_residual$v) * bias * lambda2
    
    #lambda_plus = p**(0.5) * sigma_hat * (1 + (q/p)**0.5)
    #lambda_ = p**0.5 * sigma_hat * (1 - (q/p)**0.5)
    #h = beta * (lambda_plus - lambda_)
    
    #old_sigma = svd_residual$d
    #old_sigma[old_sigma>lambda_plus] = lambda_plus -  h * 0.01
    #old_sigma[old_sigma<lambda_] = lambda_ +  h * 0.01
    #for (inner_iter in 1:5){
    #  K = sapply(old_sigma,gaussian_kernel,x2=old_sigma,h=h) /q 
    #  S = sapply(old_sigma, score_match,h=h,lambda_plus=lambda_plus,lambda_=lambda_)
    #  gradient_sigma = c(S %*% K + 2/h * old_sigma * rowSums(K))
    
    #  old_sigma = old_sigma + gradient_sigma
    #  old_sigma[old_sigma>lambda_plus] = lambda_plus - h * 0.01
    #  old_sigma[old_sigma<lambda_] = lambda_ +  h * 0.01
    #}
    #grad_residual =  svd_residual$u %*% diag(old_sigma- svd_residual$d) %*% t(svd_residual$v) * lambda_2
    
    for (i in 1:n){
      grad_H[X1[i],X2[i]] = grad_residual[X1[i],X2[i]]
    }
    #grad_H = grad_residual
    
    
    residual_mat_ = - grad_H
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
    # grad_U = residual_mat_ %*% V / n + lambda1 * U / p / r 
    # grad_V = t(residual_mat_) %*% U / n + lambda1 * V / q / r 
    U = U - step_size * grad_U
    V = V - step_size * grad_V
    
    
    
    #U_ = U - step_size * 0.001 * t((t(U) %*% svd_residual$u - matrix(rep((rowSums(t(U)**2))**0.5,q),r,q)/p**0.5) %*% t(svd_residual$u))
    #V_ = V - step_size * 0.001* t((t(V) %*% svd_residual$v - matrix(rep((rowSums(t(V)**2))**0.5,q),r,q)/q**0.5) %*% t(svd_residual$v))
    #U = U_ / t(matrix(rep((rowSums(t(U_)**2))**0.5,p),r,p)) * t(matrix(rep((rowSums(t(U)**2))**0.5,p),r,p))
    #V = V_ /t(matrix(rep((rowSums(t(V_)**2))**0.5,q),r,q)) * t(matrix(rep((rowSums(t(V)**2))**0.5,q),r,q))
    
    if (as.integer(iter/20)*20==iter){
      print(iter)
      obj_value <- sum(residual_mat^2) / n 
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

# convex relaxed method
Nuclear_opt_new_func <- function(X1,X2,Y,n_t,p,q,lambda,gamma,diag_H,beta=0.2,itertime=30000,constant=TRUE,tor=1e-4,init=FALSE,M_input=FALSE){
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
  # iteration
  for (iter in 1:itertime) {
    for (i in 1:n_t){
      residual_mat[X1[i],X2[i]] = M[X1[i],X2[i]] - Y[i]
    }
    #sigma_hat = mean(residual_mat**2)**0.5 * (1-0.5/log(iter*0.01+exp(1)))
    try(svd_residual <- svd(residual_mat))
    #sigma_hat =  sum(svd_residual$d[R1:R2]) / sum_lambda /iter + sigma_hat*(iter - 1)/iter
    sigma_hat =  sum(svd_residual$d[R1:R2]) / sum_lambda * (p*q/n_t)**0.5
    #svd_residual = irlba(residual_mat,r)
    #svd_residual2 = irlba(residual_mat,r,smallest = TRUE)
    
    #sigma_hat =  sum(svd_residual$d[26:50]) / 347.59 
    # sigma_hat =  sum(svd_residual$d[51:100]) / 441.75
    # sigma_hat =  sum(svd_residual$d[151:300]) / 860.44
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





