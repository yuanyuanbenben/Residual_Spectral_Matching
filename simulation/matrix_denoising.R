library(foreach)
library(doParallel)
library(ggplot2)
library(reshape2)
library(svd)
setwd('/home/yuanyuanbenben/project_dmc/matrix_test/mc_spectral_method')
source('baseline_methods.R')
source('proposed_method.R')

# setting
m = 500
n = 250
p = n/m
rho = 1
r = 2
sigma = 1
lambda_plus = m**(0.5) * sigma * (1 + p**0.5) 
lambda_ = m**0.5 * sigma * (1 - p**0.5) 
# setting rank
s = r
# stepsize1 = 100
# stepsize2 = 10

error_measure_func <- function(M_1,M_2,m,n,r){
  f_norm = sqrt(sum((M_1-M_2)^2)/m/n)
  spectral_norm = svd::propack.svd(M_1 - M_2)$d[1]/sqrt(m)
  maximal_norm = max(abs(M_1 - M_2))
  svd_1 = svd::propack.svd(M_1)
  svd_2 = svd::propack.svd(M_2)
  subspace_loss = (sqrt(sum((svd_1$u[1:m,1:r] - svd_1$u[1:m,1:r] %*% t(svd_1$u[1:m,1:r]) %*% svd_2$u[1:m,1:r])^2)/r) 
                   + sqrt(sum((svd_1$v[1:n,1:r] - svd_1$v[1:n,1:r] %*% t(svd_1$v[1:n,1:r]) %*% svd_2$v[1:n,1:r])^2)/r))
  return(c(f_norm,spectral_norm,maximal_norm,subspace_loss))
}


cl.cores = detectCores(logical = F)
cl <- 100
registerDoParallel(cl)


# baseline compare
loss_total <- foreach(multi=1:100,.verbose=TRUE,.combine = rbind) %dopar% {
  set.seed(multi)
  lamda = multi * 0.5 * lambda_plus/100  *0.7
  
  loss_return = matrix(0,100,12)
  for (i in 1:100) {
    H = matrix(rnorm(m*n),m,n) * 0.5**0.5 + (matrix(rbinom(m*n,1,0.5),m,n)*2 - 1) * 0.5 ** 0.5
    H = H / 100
    
    S = matrix(rnorm(m*n),m,n)
    svd_S = svd::propack.svd(S)
    u = svd_S$u[1:m,1:r]
    v = svd_S$v[1:n,1:r]
    if (r == 1){
      M_0 =  u  %*% t(v) * lambda_plus
    }
    if (r == 2){
      M_0 =  u %*% diag(c(1.01,1.0)) %*% t(v) * lambda_plus
    }
    if (r == 5){
      M_0 =  u %*% diag(c(1.04,1.03,1.02,1.01,1.0)) %*% t(v) * lambda_plus
    }
    if (r == 10){
      M_0 =  u %*% diag(c(1.09,1.08,1.07,1.06,1.05,1.04,1.03,1.02,1.01,1.0)) %*% t(v) * lambda_plus
    }
    if (r == 20){
      M_0 =  u %*% diag(c(1.19,1.18,1.17,1.16,1.15,1.14,1.13,1.12,1.11,1.0,1.09,1.08,1.07,1.06,1.05,1.04,1.03,1.02,1.01,1.0)) %*% t(v) * lambda_plus
    }
    
    
    M = H * (multi*0.5) + M_0
    svd_m = svd::propack.svd(M)
    # matrix factorization
    if (s == 1){
      M_hat_fac = svd_m$u[1:m,1:s]%*% t(svd_m$v[1:n,1:s])* svd_m$d[1:s]
    }else{
      M_hat_fac = svd_m$u[1:m,1:s]%*% diag(svd_m$d[1:s]) %*% t(svd_m$v[1:n,1:s])
    }
    loss_fac = error_measure_func(M_0,M_hat_fac,m,n,r)
    
    # convex method
    threshold_func <- function(x) sapply(x, function(z) max(0,z))
    
    M_hat_nul = svd_m$u%*% diag(threshold_func(svd_m$d-lamda)) %*% t(svd_m$v)
    
    loss_nul = error_measure_func(M_0,M_hat_nul,m,n,r)
    
    # random matrix method
    adjust_singular <- function(theta,sigma,p,m){
      b = theta**2 - sigma**2*(1+p)*m
      tmp = b**2 - 4*sigma**4*p*m**2
      index1 = tmp > 0
      tmp2 = (b[index1] + tmp[index1]**0.5)/2
      index2 = tmp2 > 0
      ret = theta
      ret[index1][index2] = tmp2[index2]**0.5
      return(ret)
    }
    
    R1 = round(n/3)
    R2 = n - round(n/3)
    sigma_hat = sum(svd_m$d[R1:R2]) / sum(svd(matrix(rnorm(m*n),m,n))$d[R1:R2])
    if (s == 1){
      M_hat_rmt = svd_m$u[1:m,1:s]%*% t(svd_m$v[1:n,1:s])* adjust_singular(svd_m$d[1:s], sigma_hat, p, m)
    }else{
      M_hat_rmt= svd_m$u[1:m,1:s]%*% diag(adjust_singular(svd_m$d[1:s], sigma_hat, p, m)) %*% t(svd_m$v[1:n,1:s])
    }
    loss_rmt = error_measure_func(M_0,M_hat_rmt,m,n,r)
    
    loss_return[i,1:4] = loss_fac
    loss_return[i,5:8] = loss_nul
    loss_return[i,9:12] = loss_rmt
  }
  loss_return = colMeans(loss_return)
  loss_ret = rep(0,13)
  loss_ret[1:12] = loss_return
  loss_ret[13] = multi * 0.5 /100 
  loss_ret
}

loss_fac = data.frame(loss_total[,1:4])
write.csv(loss_fac,paste("output/denoising_loss_fac",m,n,s,r,".csv",sep = "_"))
loss_nul = data.frame(loss_total[,5:8])
write.csv(loss_nul,paste("output/denoising_loss_nul",m,n,s,r,".csv",sep = "_"))
loss_rmt = data.frame(loss_total[,9:12])
write.csv(loss_rmt,paste("output/denoising_loss_rmt",m,n,s,r,".csv",sep = "_"))
multi_list = data.frame(loss_total[,13])
write.csv(multi_list,paste("output/denoising_loss_level",m,n,s,r,".csv",sep = "_"))
# stopCluster(cl)
# 
loss_fac = read.csv(paste("output/denoising_loss_fac",m,n,s,r,".csv",sep = "_"))$X1
loss_nul = read.csv(paste("output/denoising_loss_nul",m,n,s,r,".csv",sep = "_"))$X1
loss_rmt = read.csv(paste("output/denoising_loss_rmt",m,n,s,r,".csv",sep = "_"))$X1
multi_list = read.csv(paste("output/denoising_loss_level",m,n,s,r,".csv",sep = "_"))[,2]

mse_data = data.frame("noise"= multi_list,loss_fac,loss_nul,loss_rmt)
mydata <- melt(mse_data,id="noise")
colnames(mydata)[2] = 'Estimator'
plot <- ggplot(data = mydata,aes(x=noise,y=value,group = Estimator,
                                   color=Estimator,shape=Estimator))+
  geom_line(size=1.2)+
  # geom_point(size=3)+
  theme_bw() +
  # geom_segment(aes(x=0.22,y=0,xend=0.22,yend=0.8),linetype='dashed',size=1.2,color='#999999') +
  scale_shape_discrete(labels=c('M_fac','M_nuc','M_rmt'))+
  scale_colour_discrete(labels=c('M_fac','M_nuc','M_rmt'))+
  scale_x_continuous(name="Noise standard deviation",limits = c(0.,0.5),breaks = seq(0.,0.5,0.05))+
  scale_y_continuous(name="Frobenius norm",limits=c(0.0,0.18),breaks = seq(0.0,0.18,0.02))+
  labs(title="Frobenius norm error with increasing noise level: r=20")+
  theme(panel.grid.minor = element_blank(),legend.position = c(.15,.86),
        legend.box.background = element_rect(color="black"),
        axis.title.x=element_text(size=28),
        axis.title.y=element_text(size=28),
        axis.text.x=element_text(size=24),
        axis.text.y=element_text(size=24),
        legend.title =element_text(size=28),
        legend.text = element_text(size=28),
        plot.title = element_text(size=28,hjust=0.5))

ggsave(paste("plot/denoising_l2_loss_noise",m,n,s,r,'.png',sep='_'),plot=plot,width=12,height=8)

