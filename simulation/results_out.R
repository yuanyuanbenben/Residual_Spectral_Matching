library(ggplot2)
library(reshape2)

setwd('/home/yuanyuanbenben/project_dmc/matrix_test/mc_spectral_method')
rho = 0.2

r=10
s = r
l2_loss =  matrix(0,7,4)
lsp_loss = matrix(0,7,4)
lmax_loss = matrix(0,7,4)
for (i in 1:7) {
  m =(i+1)*100
  n = m/2
  method1_result = read.csv(paste("output/nonconvex_loss_our1",m,n,rho,s,r,".csv",sep = "_"))
  baseline1_result = read.csv(paste("output/nonconvex_loss_baseline1",m,n,rho,s,r,".csv",sep = "_"))
  method2_result = read.csv(paste("output/convex_loss_our1",m,n,rho,r,".csv",sep = "_"))
  baseline2_result = read.csv(paste("output/convex_loss_baseline1",m,n,rho,r,".csv",sep = "_"))
  l2_loss[i,1] = mean(method1_result$X1)
  l2_loss[i,2] = mean(baseline1_result$X1)
  l2_loss[i,3] = mean(method2_result$X1)
  l2_loss[i,4] = mean(baseline2_result$X1)
  lsp_loss[i,1] = mean(method1_result$X2)
  lsp_loss[i,2] = mean(baseline1_result$X2)
  lsp_loss[i,3] = mean(method2_result$X2)
  lsp_loss[i,4] = mean(baseline2_result$X2)
  lmax_loss[i,1] = mean(method1_result$X3)
  lmax_loss[i,2] = mean(baseline1_result$X3)
  lmax_loss[i,3] = mean(method2_result$X3)
  lmax_loss[i,4] = mean(baseline2_result$X3)
  }
print(l2_loss)
print(lsp_loss)
print(lmax_loss)



l2_loss_misrank = matrix(0,7,2)
lsp_loss_misrank = matrix(0,7,2)
lmax_loss_misrank = matrix(0,7,2)
for (i in 1:7){
  m = 500
  n = 250
  s = r - 1 + i
  method1_result = read.csv(paste("output/nonconvex_loss_our1",m,n,rho,s,r,".csv",sep = "_"))
  baseline1_result = read.csv(paste("output/nonconvex_loss_baseline1",m,n,rho,s,r,".csv",sep = "_"))
  l2_loss_misrank[i,1] = mean(method1_result$X1)
  l2_loss_misrank[i,2] = mean(baseline1_result$X1)
  lsp_loss_misrank[i,1] = mean(method1_result$X2)
  lsp_loss_misrank[i,2] = mean(baseline1_result$X2)
  lmax_loss_misrank[i,1] = mean(method1_result$X3)
  lmax_loss_misrank[i,2] = mean(baseline1_result$X3)
}
print(l2_loss_misrank)
print(lsp_loss_misrank)
print(lmax_loss_misrank)

###################################################################################################
# plot
###################################################################################################

# Frobenius norm for different matrix size
rho=0.2
r = 10
s = r
l2_loss =  matrix(0,7,4)
lsp_loss = matrix(0,7,4)
lmax_loss = matrix(0,7,4)
for (i in 1:7) {
  m =(i+1)*100
  n = m/2
  method1_result = read.csv(paste("output/nonconvex_loss_our1",m,n,rho,s,r,".csv",sep = "_"))
  baseline1_result = read.csv(paste("output/nonconvex_loss_baseline1",m,n,rho,s,r,".csv",sep = "_"))
  method2_result = read.csv(paste("output/convex_loss_our1",m,n,rho,r,".csv",sep = "_"))
  baseline2_result = read.csv(paste("output/convex_loss_baseline1",m,n,rho,r,".csv",sep = "_"))
  l2_loss[i,1] = mean(method1_result$X1)
  l2_loss[i,2] = mean(baseline1_result$X1)
  l2_loss[i,3] = mean(method2_result$X1)
  l2_loss[i,4] = mean(baseline2_result$X1)
  lsp_loss[i,1] = mean(method1_result$X2)
  lsp_loss[i,2] = mean(baseline1_result$X2)
  lsp_loss[i,3] = mean(method2_result$X2)
  lsp_loss[i,4] = mean(baseline2_result$X2)
  lmax_loss[i,1] = mean(method1_result$X3)
  lmax_loss[i,2] = mean(baseline1_result$X3)
  lmax_loss[i,3] = mean(method2_result$X3)
  lmax_loss[i,4] = mean(baseline2_result$X3)
}
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
adjust_singular <- function(theta,sigma,p,m){
  b = theta**2 - sigma**2*(1+p)*m
  return(((b + (b**2 - 4*sigma**4*p*m**2)**0.5)/2)**0.5)
}
optimal_lower_bound <- rep(0,7)
for (i in 1:7){
  m = i*100 +100
  n = i*50 + 50
  R1 = round(n/3)
  R2 = n - round(n/3)
  sigma = 1
  lambda_plus = m**(0.5) * sigma * (1 + p**0.5) 
  lambda_ = m**0.5 * sigma * (1 - p**0.5) 
  loss_ = rep(0,10)
  for (seed in 1:10){
    set.seed(seed)
    H = matrix(rnorm(m*n),m,n) * 0.5**0.5 + (matrix(rbinom(m*n,1,0.5),m,n)*2 - 1) * 0.5 ** 0.5
    S = matrix(rnorm(m*n),m,n)
    svd_S = svd(S)
    u = svd_S$u[1:m,1:r]
    v = svd_S$v[1:n,1:r]
    if (r == 5){
      M_0 = u %*% diag(c(1,1.1,1.2,1.3,1.4)) %*% t(v) * lambda_plus * 10 / 3
    }
    if (r == 10){
      M_0 = u %*% diag(c(1,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9)) %*% t(v) * lambda_plus * 10 / 3
    }
    if (r == 20){
      M_0 = u %*% diag(c(1,1.05,1.1,1.15,1.2,1.25,1.3,1.35,1.4,1.45,1.5,1.55,1.6,1.65,1.7,1.75,1.8,1.85,1.9,1.95)) %*% t(v) * lambda_plus * 10 / 3
    }
    
    M = H + M_0
    svd_m = svd(M)
    sigma_hat = sum(svd_m$d[R1:R2]) / sum(svd(matrix(rnorm(m*n),m,n))$d[R1:R2])
    M_hat_lower = svd_m$u[1:m,1:s]%*% diag(adjust_singular(svd_m$d[1:s], sigma_hat, p, m)) %*% t(svd_m$v[1:n,1:s])
    loss_[seed] = error_measure_func(M_0,M_hat_lower,m,n,r)
  }
  optimal_lower_bound[i] = mean(loss_)
}

# 
theor_loss <- function(m){
  m_index = (2:7)*100 + 100
  a = mean(optimal_lower_bound[2:7]*sqrt(1/rho)*(m_index)**0.5)
  return(a/sqrt(m))
}

mse_data = data.frame("m"=c(300,400,500,600,700,800),l2_loss[2:7,1],l2_loss[2:7,2],l2_loss[2:7,3],l2_loss[2:7,4])
mydata <- melt(mse_data,id="m")
colnames(mydata)[2] = 'Estimator'
if (r == 5){
  plot_1 <- ggplot(data = mydata,aes(x=m,y=value,group = Estimator,
                                     color=Estimator,shape=Estimator))+
    geom_line(size=1.2)+
    geom_point(size=3)+
    theme_bw() +
    geom_function(fun=theor_loss,linetype='dashed',size=1.2,color='#999999')+
    scale_shape_discrete(labels=c('Estimator 1','Baseline 1','Estimator 2','Baseline 2'))+
    scale_colour_discrete(labels=c('Estimator 1','Baseline 1','Estimator 2','Baseline 2'))+
    scale_x_continuous(name="Matrix size m",limits = c(300,800),breaks = seq(300,800,100))+
    scale_y_continuous(name="Frobenius norm",limits=c(0.3,0.7),breaks = seq(0.3,0.7,0.05))+
    labs(title="Frobenius norm error with increasing m for r = 5")+
    theme(panel.grid.minor = element_blank(),legend.position = c(.85,.83),
          legend.box.background = element_rect(color="black"),
          axis.title.x=element_text(size=28),
          axis.title.y=element_text(size=28),
          axis.text.x=element_text(size=24),
          axis.text.y=element_text(size=24),
          legend.title =element_text(size=28),
          legend.text = element_text(size=28),
          plot.title = element_text(size=28,hjust=0.5))
  
}
if (r == 10){
  plot_1 <- ggplot(data = mydata,aes(x=m,y=value,group = Estimator,
                                     color=Estimator,shape=Estimator))+
    geom_line(size=1.2)+
    geom_point(size=3)+
    theme_bw() +
    geom_function(fun=theor_loss,linetype='dashed',size=1.2,color='#999999')+
    scale_shape_discrete(labels=c('Estimator 1','Baseline 1','Estimator 2','Baseline 2'))+
    scale_colour_discrete(labels=c('Estimator 1','Baseline 1','Estimator 2','Baseline 2'))+
    scale_x_continuous(name="Matrix size m",limits = c(300,800),breaks = seq(300,800,100))+
    scale_y_continuous(name="Frobenius norm",limits=c(0.4,1.2),breaks = seq(0.4,1.2,0.1))+
    labs(title="Frobenius norm error with increasing m for r = 10")+
    theme(panel.grid.minor = element_blank(),legend.position = c(.85,.83),
          legend.box.background = element_rect(color="black"),
          axis.title.x=element_text(size=28),
          axis.title.y=element_text(size=28),
          axis.text.x=element_text(size=24),
          axis.text.y=element_text(size=24),
          legend.title =element_text(size=28),
          legend.text = element_text(size=28),
          plot.title = element_text(size=28,hjust=0.5))
}
if (r == 20){
  plot_1 <- ggplot(data = mydata,aes(x=m,y=value,group = Estimator,
                                     color=Estimator,shape=Estimator))+
    geom_line(size=1.2)+
    geom_point(size=3)+
    theme_bw() +
    geom_function(fun=theor_loss,linetype='dashed',size=1.2,color='#999999')+
    scale_shape_discrete(labels=c('Estimator 1','Baseline 1','Estimator 2','Baseline 2'))+
    scale_colour_discrete(labels=c('Estimator 1','Baseline 1','Estimator 2','Baseline 2'))+
    scale_x_continuous(name="Matrix size m",limits = c(300,800),breaks = seq(300,800,100))+
    scale_y_continuous(name="Frobenius norm",limits=c(0.6,4.4),breaks = seq(0.6,4.4,0.4))+
    labs(title="Frobenius norm error with increasing m for r = 20")+
    theme(panel.grid.minor = element_blank(),legend.position = c(.85,.83),
          legend.box.background = element_rect(color="black"),
          axis.title.x=element_text(size=28),
          axis.title.y=element_text(size=28),
          axis.text.x=element_text(size=24),
          axis.text.y=element_text(size=24),
          legend.title =element_text(size=28),
          legend.text = element_text(size=28),
          plot.title = element_text(size=28,hjust=0.5))
}

ggsave(paste("plot/l2_loss_",r,".png",sep=''),plot=plot_1,width=12,height=8)


# Frobenius norm for misrank
l2_loss_misrank = matrix(0,7,2)
lsp_loss_misrank = matrix(0,7,2)
lmax_loss_misrank = matrix(0,7,2)
for (i in 1:7){
  m = 500
  n = 250
  s = i + r - 1
  method1_result = read.csv(paste("output/nonconvex_loss_our1",m,n,rho,s,r,".csv",sep = "_"))
  baseline1_result = read.csv(paste("output/nonconvex_loss_baseline1",m,n,rho,s,r,".csv",sep = "_"))
  l2_loss_misrank[i,1] = mean(method1_result$X1)
  l2_loss_misrank[i,2] = mean(baseline1_result$X1)
  lsp_loss_misrank[i,1] = mean(method1_result$X2)
  lsp_loss_misrank[i,2] = mean(baseline1_result$X2)
  lmax_loss_misrank[i,1] = mean(method1_result$X3)
  lmax_loss_misrank[i,2] = mean(baseline1_result$X3)
}

# 
theor_loss_misrank <- function(m){
  m_index = (2:7)*100 + 100
  a = mean(optimal_lower_bound[2:7]*sqrt(1/rho)*(m_index)**0.5)/sqrt(r)
  return(a*sqrt(m)/sqrt(500))
}

mse_data = data.frame("s"=c(r,r+1,r+2,r+3,r+4,r+5,r+6),l2_loss_misrank[,1],l2_loss_misrank[,2])
mydata <- melt(mse_data,id="s")
colnames(mydata)[2] = 'Estimator'
if (r == 5){
  plot_2 <- ggplot(data = mydata,aes(x=s,y=value,group = Estimator,
                                     color=Estimator,shape=Estimator))+
    geom_line(size=1.2)+
    geom_point(size=3)+
    theme_bw() +
    geom_function(fun=theor_loss_misrank,linetype='dashed',size=1.2,color='#999999')+
    scale_shape_discrete(labels=c('Estimator 1','Baseline 1'))+
    scale_colour_discrete(labels=c('Estimator 1','Baseline 1'))+
    scale_x_continuous(name="Used rank s",limits = c(r,r+6),breaks = seq(r,r+6,1))+
    scale_y_continuous(name="Frobenius norm",limits=c(0.3,0.9),breaks = seq(0.3,0.9,0.1))+
    labs(title="Frobenius norm error with increasing s for r = 5")+
    theme(panel.grid.minor = element_blank(),legend.position = c(.15,.86),
          legend.box.background = element_rect(color="black"),
          axis.title.x=element_text(size=28),
          axis.title.y=element_text(size=28),
          axis.text.x=element_text(size=24),
          axis.text.y=element_text(size=24),
          legend.title =element_text(size=28),
          legend.text = element_text(size=28),
          plot.title = element_text(size=28,hjust=0.5))
}
if (r == 10){
  plot_2 <- ggplot(data = mydata,aes(x=s,y=value,group = Estimator,
                                     color=Estimator,shape=Estimator))+
    geom_line(size=1.2)+
    geom_point(size=3)+
    theme_bw() +
    geom_function(fun=theor_loss_misrank,linetype='dashed',size=1.2,color='#999999')+
    scale_shape_discrete(labels=c('Estimator 1','Baseline 1'))+
    scale_colour_discrete(labels=c('Estimator 1','Baseline 1'))+
    scale_x_continuous(name="Used rank s",limits = c(r,r+6),breaks = seq(r,r+6,1))+
    scale_y_continuous(name="Frobenius norm",limits=c(0.5,1.2),breaks = seq(0.5,1.2,0.1))+
    labs(title="Frobenius norm error with increasing s for r = 10")+
    theme(panel.grid.minor = element_blank(),legend.position = c(.15,.86),
          legend.box.background = element_rect(color="black"),
          axis.title.x=element_text(size=28),
          axis.title.y=element_text(size=28),
          axis.text.x=element_text(size=24),
          axis.text.y=element_text(size=24),
          legend.title =element_text(size=28),
          legend.text = element_text(size=28),
          plot.title = element_text(size=28,hjust=0.5))
}
if (r == 20){
  plot_2 <- ggplot(data = mydata,aes(x=s,y=value,group = Estimator,
                                     color=Estimator,shape=Estimator))+
    geom_line(size=1.2)+
    geom_point(size=3)+
    theme_bw() +
    geom_function(fun=theor_loss_misrank,linetype='dashed',size=1.2,color='#999999')+
    scale_shape_discrete(labels=c('Estimator 1','Baseline 1'))+
    scale_colour_discrete(labels=c('Estimator 1','Baseline 1'))+
    scale_x_continuous(name="Used rank s",limits = c(r,r+6),breaks = seq(r,r+6,1))+
    scale_y_continuous(name="Frobenius norm",limits=c(0.7,2.1),breaks = seq(0.7,2.1,0.2))+
    labs(title="Frobenius norm error with increasing s for r = 20")+
    theme(panel.grid.minor = element_blank(),legend.position = c(.15,.86),
          legend.box.background = element_rect(color="black"),
          axis.title.x=element_text(size=28),
          axis.title.y=element_text(size=28),
          axis.text.x=element_text(size=24),
          axis.text.y=element_text(size=24),
          legend.title =element_text(size=28),
          legend.text = element_text(size=28),
          plot.title = element_text(size=28,hjust=0.5))
}


ggsave(paste("plot/l2_loss_misrank_",r,".png",sep=''),plot=plot_2,width=12,height=8)




# Spectral norm for different matrix size
mse_data = data.frame("m"=c(300,400,500,600,700,800),lsp_loss[2:7,1],lsp_loss[2:7,2],lsp_loss[2:7,3],lsp_loss[2:7,4])
mydata <- melt(mse_data,id="m")
colnames(mydata)[2] = 'Estimator'
if (r == 5){
  plot_3 <- ggplot(data = mydata,aes(x=m,y=value,group = Estimator,
                                     color=Estimator,shape=Estimator))+
    geom_line(size=1.2)+
    geom_point(size=3)+
    theme_bw() +
    # geom_function(fun=theor_loss,linetype='dashed',size=1.2,color='#999999')+
    scale_shape_discrete(labels=c('Estimator 1','Baseline 1','Estimator 2','Baseline 2'))+
    scale_colour_discrete(labels=c('Estimator 1','Baseline 1','Estimator 2','Baseline 2'))+
    scale_x_continuous(name="Matrix size m",limits = c(300,800),breaks = seq(300,800,100))+
    scale_y_continuous(name="Spectral norm",limits=c(2.5,3.5),breaks = seq(2.5,3.5,0.2))+
    labs(title="Spectral norm error with increasing m for r = 5")+
    theme(panel.grid.minor = element_blank(),legend.position = c(.85,.83),
          legend.box.background = element_rect(color="black"),
          axis.title.x=element_text(size=28),
          axis.title.y=element_text(size=28),
          axis.text.x=element_text(size=24),
          axis.text.y=element_text(size=24),
          legend.title =element_text(size=28),
          legend.text = element_text(size=28),
          plot.title = element_text(size=28,hjust=0.5))
}
if (r == 10){
  plot_3 <- ggplot(data = mydata,aes(x=m,y=value,group = Estimator,
                                     color=Estimator,shape=Estimator))+
    geom_line(size=1.2)+
    geom_point(size=3)+
    theme_bw() +
    # geom_function(fun=theor_loss,linetype='dashed',size=1.2,color='#999999')+
    scale_shape_discrete(labels=c('Estimator 1','Baseline 1','Estimator 2','Baseline 2'))+
    scale_colour_discrete(labels=c('Estimator 1','Baseline 1','Estimator 2','Baseline 2'))+
    scale_x_continuous(name="Matrix size m",limits = c(300,800),breaks = seq(300,800,100))+
    scale_y_continuous(name="Spectral norm",limits=c(2.5,5),breaks = seq(2.5,5,0.5))+
    labs(title="Spectral norm error with increasing m for r = 10")+
    theme(panel.grid.minor = element_blank(),legend.position = c(.85,.83),
          legend.box.background = element_rect(color="black"),
          axis.title.x=element_text(size=28),
          axis.title.y=element_text(size=28),
          axis.text.x=element_text(size=24),
          axis.text.y=element_text(size=24),
          legend.title =element_text(size=28),
          legend.text = element_text(size=28),
          plot.title = element_text(size=28,hjust=0.5))
}
if (r == 20){
  plot_3 <- ggplot(data = mydata,aes(x=m,y=value,group = Estimator,
                                     color=Estimator,shape=Estimator))+
    geom_line(size=1.2)+
    geom_point(size=3)+
    theme_bw() +
    # geom_function(fun=theor_loss,linetype='dashed',size=1.2,color='#999999')+
    scale_shape_discrete(labels=c('Estimator 1','Baseline 1','Estimator 2','Baseline 2'))+
    scale_colour_discrete(labels=c('Estimator 1','Baseline 1','Estimator 2','Baseline 2'))+
    scale_x_continuous(name="Matrix size m",limits = c(300,800),breaks = seq(300,800,100))+
    scale_y_continuous(name="Spectral norm",limits=c(3,15),breaks = seq(3,15,2))+
    labs(title="Spectral norm error with increasing m for r = 20")+
    theme(panel.grid.minor = element_blank(),legend.position = c(.85,.83),
          legend.box.background = element_rect(color="black"),
          axis.title.x=element_text(size=28),
          axis.title.y=element_text(size=28),
          axis.text.x=element_text(size=24),
          axis.text.y=element_text(size=24),
          legend.title =element_text(size=28),
          legend.text = element_text(size=28),
          plot.title = element_text(size=28,hjust=0.5))
}

ggsave(paste("plot/spectral_loss_",r,".png",sep=''),plot=plot_3,width=12,height=8)


# Spectral norm for misrank
mse_data = data.frame("s"=c(r,r+1,r+2,r+3,r+4,r+5,r+6),lsp_loss_misrank[,1],lsp_loss_misrank[,2])
mydata <- melt(mse_data,id="s")
colnames(mydata)[2] = 'Estimator'
if (r == 5){
  plot_4 <- ggplot(data = mydata,aes(x=s,y=value,group = Estimator,
                                     color=Estimator,shape=Estimator))+
    geom_line(size=1.2)+
    geom_point(size=3)+
    theme_bw() +
    # geom_function(fun=theor_loss_misrank,linetype='dashed',size=1.2,color='#999999')+
    scale_shape_discrete(labels=c('Estimator 1','Baseline 1'))+
    scale_colour_discrete(labels=c('Estimator 1','Baseline 1'))+
    scale_x_continuous(name="Used rank s",limits = c(r,r+6),breaks = seq(r,r+6,1))+
    scale_y_continuous(name="Spectral norm",limits=c(2.5,5),breaks = seq(2.5,5,0.5))+
    labs(title="Spectral norm error with increasing s for r = 5")+
    theme(panel.grid.minor = element_blank(),legend.position = c(.15,.86),
          legend.box.background = element_rect(color="black"),
          axis.title.x=element_text(size=28),
          axis.title.y=element_text(size=28),
          axis.text.x=element_text(size=24),
          axis.text.y=element_text(size=24),
          legend.title =element_text(size=28),
          legend.text = element_text(size=28),
          plot.title = element_text(size=28,hjust=0.5))
}
if (r == 10){
  plot_4 <- ggplot(data = mydata,aes(x=s,y=value,group = Estimator,
                                     color=Estimator,shape=Estimator))+
    geom_line(size=1.2)+
    geom_point(size=3)+
    theme_bw() +
    # geom_function(fun=theor_loss_misrank,linetype='dashed',size=1.2,color='#999999')+
    scale_shape_discrete(labels=c('Estimator 1','Baseline 1'))+
    scale_colour_discrete(labels=c('Estimator 1','Baseline 1'))+
    scale_x_continuous(name="Used rank s",limits = c(r,r+6),breaks = seq(r,r+6,1))+
    scale_y_continuous(name="Spectral norm",limits=c(3,6),breaks = seq(3,6,0.5))+
    labs(title="Spectral norm error with increasing s for r = 10")+
    theme(panel.grid.minor = element_blank(),legend.position = c(.15,.86),
          legend.box.background = element_rect(color="black"),
          axis.title.x=element_text(size=28),
          axis.title.y=element_text(size=28),
          axis.text.x=element_text(size=24),
          axis.text.y=element_text(size=24),
          legend.title =element_text(size=28),
          legend.text = element_text(size=28),
          plot.title = element_text(size=28,hjust=0.5))
}
if (r == 20){
  plot_4 <- ggplot(data = mydata,aes(x=s,y=value,group =Estimator,
                                     color=Estimator,shape=Estimator))+
    geom_line(size=1.2)+
    geom_point(size=3)+
    theme_bw() +
    # geom_function(fun=theor_loss_misrank,linetype='dashed',size=1.2,color='#999999')+
    scale_shape_discrete(labels=c('Estimator 1','Baseline 1'))+
    scale_colour_discrete(labels=c('Estimator 1','Baseline 1'))+
    scale_x_continuous(name="Used rank s",limits = c(r,r+6),breaks = seq(r,r+6,1))+
    scale_y_continuous(name="Spectral norm",limits=c(4.5,8),breaks = seq(4.5,8,0.5))+
    labs(title="Spectral norm error with increasing s for r = 20")+
    theme(panel.grid.minor = element_blank(),legend.position = c(.15,.86),
          legend.box.background = element_rect(color="black"),
          axis.title.x=element_text(size=28),
          axis.title.y=element_text(size=28),
          axis.text.x=element_text(size=24),
          axis.text.y=element_text(size=24),
          legend.title =element_text(size=28),
          legend.text = element_text(size=28),
          plot.title = element_text(size=28,hjust=0.5))
}

ggsave(paste("plot/spcetral_loss_misrank_",r,".png",sep=''),plot=plot_4,width=12,height=8)




# Maximum norm for different matrix size
mse_data = data.frame("m"=c(300,400,500,600,700,800),lmax_loss[2:7,1],lmax_loss[2:7,2],lmax_loss[2:7,3],lmax_loss[2:7,4])
mydata <- melt(mse_data,id="m")
colnames(mydata)[2] = 'Estimator'
if (r == 5){
  plot_5 <- ggplot(data = mydata,aes(x=m,y=value,group = Estimator,
                                     color=Estimator,shape=Estimator))+
    geom_line(size=1.2)+
    geom_point(size=3)+
    theme_bw() +
    # geom_function(fun=theor_loss,linetype='dashed',size=1.2,color='#999999')+
    scale_shape_discrete(labels=c('Estimator 1','Baseline 1','Estimator 2','Baseline 2'))+
    scale_colour_discrete(labels=c('Estimator 1','Baseline 1','Estimator 2','Baseline 2'))+
    scale_x_continuous(name="Matrix size m",limits = c(300,800),breaks = seq(300,800,100))+
    scale_y_continuous(name="Maximum norm",limits=c(2,5),breaks = seq(2,5,0.5))+
    labs(title="Maximum norm error with increasing m for r = 5")+
    theme(panel.grid.minor = element_blank(),legend.position = c(.85,.83),
          legend.box.background = element_rect(color="black"),
          axis.title.x=element_text(size=28),
          axis.title.y=element_text(size=28),
          axis.text.x=element_text(size=24),
          axis.text.y=element_text(size=24),
          legend.title =element_text(size=28),
          legend.text = element_text(size=28),
          plot.title = element_text(size=28,hjust=0.5))
}
if (r == 10){
  plot_5 <- ggplot(data = mydata,aes(x=m,y=value,group = Estimator,
                                     color=Estimator,shape=Estimator))+
    geom_line(size=1.2)+
    geom_point(size=3)+
    theme_bw() +
    # geom_function(fun=theor_loss,linetype='dashed',size=1.2,color='#999999')+
    scale_shape_discrete(labels=c('Estimator 1','Baseline 1','Estimator 2','Baseline 2'))+
    scale_colour_discrete(labels=c('Estimator 1','Baseline 1','Estimator 2','Baseline 2'))+
    scale_x_continuous(name="Matrix size m",limits = c(300,800),breaks = seq(300,800,100))+
    scale_y_continuous(name="Maximum norm",limits=c(3,8.5),breaks = seq(3,8.5,1.1))+
    labs(title="Maximum norm error with increasing m for r = 10")+
    theme(panel.grid.minor = element_blank(),legend.position = c(.85,.83),
          legend.box.background = element_rect(color="black"),
          axis.title.x=element_text(size=28),
          axis.title.y=element_text(size=28),
          axis.text.x=element_text(size=24),
          axis.text.y=element_text(size=24),
          legend.title =element_text(size=28),
          legend.text = element_text(size=28),
          plot.title = element_text(size=28,hjust=0.5))
}
if (r == 20){
  plot_5 <- ggplot(data = mydata,aes(x=m,y=value,group = Estimator,
                                     color=Estimator,shape=Estimator))+
    geom_line(size=1.2)+
    geom_point(size=3)+
    theme_bw() +
    # geom_function(fun=theor_loss,linetype='dashed',size=1.2,color='#999999')+
    scale_shape_discrete(labels=c('Estimator 1','Baseline 1','Estimator 2','Baseline 2'))+
    scale_colour_discrete(labels=c('Estimator 1','Baseline 1','Estimator 2','Baseline 2'))+
    scale_x_continuous(name="Matrix size m",limits = c(300,800),breaks = seq(300,800,100))+
    scale_y_continuous(name="Maximum norm",limits=c(4,36),breaks = seq(4,36,4))+
    labs(title="Maximum norm error with increasing m for r = 20")+
    theme(panel.grid.minor = element_blank(),legend.position = c(.85,.83),
          legend.box.background = element_rect(color="black"),
          axis.title.x=element_text(size=28),
          axis.title.y=element_text(size=28),
          axis.text.x=element_text(size=24),
          axis.text.y=element_text(size=24),
          legend.title =element_text(size=28),
          legend.text = element_text(size=28),
          plot.title = element_text(size=28,hjust=0.5))
}

ggsave(paste("plot/maximum_loss_",r,".png",sep=''),plot=plot_5,width=12,height=8)


# Maximum norm for misrank
mse_data = data.frame("s"=c(r,r+1,r+2,r+3,r+4,r+5,r+6),lmax_loss_misrank[,1],lmax_loss_misrank[,2])
mydata <- melt(mse_data,id="s")
colnames(mydata)[2] = 'Estimator'
if (r == 5){
  plot_6 <- ggplot(data = mydata,aes(x=s,y=value,group = Estimator,
                                     color=Estimator,shape=Estimator))+
    geom_line(size=1.2)+
    geom_point(size=3)+
    theme_bw() +
    # geom_function(fun=theor_loss_misrank,linetype='dashed',size=1.2,color='#999999')+
    scale_shape_discrete(labels=c('Estimator 1','Baseline 1'))+
    scale_colour_discrete(labels=c('Estimator 1','Baseline 1'))+
    scale_x_continuous(name="Used rank s",limits = c(r,r+6),breaks = seq(r,r+6,1))+
    scale_y_continuous(name="Maximum norm",limits=c(2.5,8),breaks = seq(2.5,8,1.1))+
    labs(title="Maximum norm error with increasing s for r = 5")+
    theme(panel.grid.minor = element_blank(),legend.position = c(.15,.86),
          legend.box.background = element_rect(color="black"),
          axis.title.x=element_text(size=28),
          axis.title.y=element_text(size=28),
          axis.text.x=element_text(size=24),
          axis.text.y=element_text(size=24),
          legend.title =element_text(size=28),
          legend.text = element_text(size=28),
          plot.title = element_text(size=28,hjust=0.5))
}
if (r == 10){
  plot_6 <- ggplot(data = mydata,aes(x=s,y=value,group = Estimator,
                                     color=Estimator,shape=Estimator))+
    geom_line(size=1.2)+
    geom_point(size=3)+
    theme_bw() +
    # geom_function(fun=theor_loss_misrank,linetype='dashed',size=1.2,color='#999999')+
    scale_shape_discrete(labels=c('Estimator 1','Baseline 1'))+
    scale_colour_discrete(labels=c('Estimator 1','Baseline 1'))+
    scale_x_continuous(name="Used rank s",limits = c(r,r+6),breaks = seq(r,r+6,1))+
    scale_y_continuous(name="Maximum norm",limits=c(4,10),breaks = seq(4,10,1))+
    labs(title="Maximum norm error with increasing s for r = 10")+
    theme(panel.grid.minor = element_blank(),legend.position = c(.15,.86),
          legend.box.background = element_rect(color="black"),
          axis.title.x=element_text(size=28),
          axis.title.y=element_text(size=28),
          axis.text.x=element_text(size=24),
          axis.text.y=element_text(size=24),
          legend.title =element_text(size=28),
          legend.text = element_text(size=28),
          plot.title = element_text(size=28,hjust=0.5))
}
if (r == 20){
  plot_6 <- ggplot(data = mydata,aes(x=s,y=value,group = Estimator,
                                     color=Estimator,shape=Estimator))+
    geom_line(size=1.2)+
    geom_point(size=3)+
    theme_bw() +
    # geom_function(fun=theor_loss_misrank,linetype='dashed',size=1.2,color='#999999')+
    scale_shape_discrete(labels=c('Estimator 1','Baseline 1'))+
    scale_colour_discrete(labels=c('Estimator 1','Baseline 1'))+
    scale_x_continuous(name="Used rank s",limits = c(r,r+6),breaks = seq(r,r+6,1))+
    scale_y_continuous(name="Maximum norm",limits=c(8,15),breaks = seq(8,15,1))+
    labs(title="Maximum norm error with increasing s for r = 20")+
    theme(panel.grid.minor = element_blank(),legend.position = c(.15,.86),
          legend.box.background = element_rect(color="black"),
          axis.title.x=element_text(size=28),
          axis.title.y=element_text(size=28),
          axis.text.x=element_text(size=24),
          axis.text.y=element_text(size=24),
          legend.title =element_text(size=28),
          legend.text = element_text(size=28),
          plot.title = element_text(size=28,hjust=0.5))
}

ggsave(paste("plot/maximum_loss_misrank_",r,".png",sep=''),plot=plot_6,width=12,height=8)





#############################################################################################
# different noise
#############################################################################################
rho=0.2
r= 5
s = r
l2_loss_noise =  matrix(0,13,2)
lsp_loss_noise = matrix(0,13,2)
lmax_loss_noise = matrix(0,13,2)
index = c(0.01,0.02,0.05,0.1,0.2,0.5,1,1.2,1.5,2,3,4,5)
for (j in 1:13) {
  i = index[j]
  m = 500
  n = m/2
  method1_result = read.csv(paste("output/nonconvex_loss_our1",m,n,rho,s,r,i,".csv",sep = "_"))
  baseline1_result = read.csv(paste("output/nonconvex_loss_baseline1",m,n,rho,s,r,i,".csv",sep = "_"))
  # method2_result = read.csv(paste("output/convex_loss_our1",m,n,rho,r,i,".csv",sep = "_"))
  # baseline2_result = read.csv(paste("output/convex_loss_baseline1",m,n,rho,r,i,".csv",sep = "_"))
  l2_loss_noise[j,1] = mean(method1_result$X1)
  l2_loss_noise[j,2] = mean(baseline1_result$X1)
  # l2_loss[i,3] = mean(method2_result$X1)
  # l2_loss[i,4] = mean(baseline2_result$X1)
  lsp_loss_noise[j,1] = mean(method1_result$X2)
  lsp_loss_noise[j,2] = mean(baseline1_result$X2)
  # lsp_loss[i,3] = mean(method2_result$X2)
  # lsp_loss[i,4] = mean(baseline2_result$X2)
  lmax_loss_noise[j,1] = mean(method1_result$X3)
  lmax_loss_noise[j,2] = mean(baseline1_result$X3)
  # lmax_loss[i,3] = mean(method2_result$X3)
  # lmax_loss[i,4] = mean(baseline2_result$X3)
}

print(l2_loss_noise)
print(lsp_loss_noise)
print(lmax_loss_noise)

# frobenius error for different noise
mse_data = data.frame("R"=index/3/rho**0.5,l2_loss_noise[,1],l2_loss_noise[,2])
mydata <- melt(mse_data,id="R")
colnames(mydata)[2] = 'Estimator'
if (r == 5){
  plot_7 <- ggplot(data = mydata,aes(x=R,y=value,group =Estimator,
                                     color=Estimator,shape=Estimator))+
    geom_line(size=1.2)+
    geom_point(size=3)+
    theme_bw() +
    geom_segment(aes(x=0.275/rho**0.5,y=0,xend=0.275/rho**0.5,yend=0.365),linetype='dashed',size=1.2,color='#999999') + 
    scale_shape_discrete(labels=c('Estimator 1','Baseline 1'))+
    scale_colour_discrete(labels=c('Estimator 1','Baseline 1'))+
    scale_x_continuous(name="Noise level ratio R",limits = c(0,1.8/rho**0.5),breaks = seq(0,1.8/rho**0.5,0.5))+
    scale_y_continuous(name="Frobenius norm",limits=c(0,3.5),breaks = seq(0,3.5,0.5))+
    labs(title="Frobenius norm error with increasing noise level for r = 5")+
    theme(panel.grid.minor = element_blank(),legend.position = c(.15,.86),
          legend.box.background = element_rect(color="black"),
          axis.title.x=element_text(size=28),
          axis.title.y=element_text(size=28),
          axis.text.x=element_text(size=24),
          axis.text.y=element_text(size=24),
          legend.title =element_text(size=28),
          legend.text = element_text(size=28),
          plot.title = element_text(size=28,hjust=0.5))
}

if (r == 10){
  plot_7 <- ggplot(data = mydata,aes(x=R,y=value,group = Estimator,
                                     color=Estimator,shape=Estimator))+
    geom_line(size=1.2)+
    geom_point(size=3)+
    theme_bw() +
    geom_segment(aes(x=0.275/rho**0.5,y=0,xend=0.275/rho**0.5,yend=0.57),linetype='dashed',size=1.2,color='#999999') + 
    scale_shape_discrete(labels=c('Estimator 1','Baseline 1'))+
    scale_colour_discrete(labels=c('Estimator 1','Baseline 1'))+
    scale_x_continuous(name="Noise level ratio R",limits = c(0,1.8/rho**0.5),breaks = seq(0,1.8/rho**0.5,0.5))+
    scale_y_continuous(name="Frobenius norm",limits=c(0,6),breaks = seq(0,6,0.6))+
    labs(title="Frobenius norm error with increasing noise level for r = 10")+
    theme(panel.grid.minor = element_blank(),legend.position = c(.15,.86),
          legend.box.background = element_rect(color="black"),
          axis.title.x=element_text(size=28),
          axis.title.y=element_text(size=28),
          axis.text.x=element_text(size=24),
          axis.text.y=element_text(size=24),
          legend.title =element_text(size=28),
          legend.text = element_text(size=28),
          plot.title = element_text(size=28,hjust=0.5))
}


if (r == 20){
  plot_7 <- ggplot(data = mydata,aes(x=R,y=value,group = Estimator,
                                     color=Estimator,shape=Estimator))+
    geom_line(size=1.2)+
    geom_point(size=3)+
    theme_bw() +
    geom_segment(aes(x=0.22/rho**0.5,y=0,xend=0.22/rho**0.5,yend=0.8),linetype='dashed',size=1.2,color='#999999') + 
    scale_shape_discrete(labels=c('Estimator 1','Baseline 1'))+
    scale_colour_discrete(labels=c('Estimator 1','Baseline 1'))+
    scale_x_continuous(name="Noise level ratio R",limits = c(0,1.8/rho**0.5),breaks = seq(0,1.8/rho**0.5,0.5))+
    scale_y_continuous(name="Frobenius norm",limits=c(0,11),breaks = seq(0,11,1))+
    labs(title="Frobenius norm error with increasing noise level for r = 20")+
    theme(panel.grid.minor = element_blank(),legend.position = c(.15,.86),
          legend.box.background = element_rect(color="black"),
          axis.title.x=element_text(size=28),
          axis.title.y=element_text(size=28),
          axis.text.x=element_text(size=24),
          axis.text.y=element_text(size=24),
          legend.title =element_text(size=28),
          legend.text = element_text(size=28),
          plot.title = element_text(size=28,hjust=0.5))
}

ggsave(paste("plot/l2_loss_noise_",r,".png",sep=''),plot=plot_7,width=12,height=8)


# spectral error for different noise
mse_data = data.frame("R"=index/3/rho**0.5,lsp_loss_noise[,1],lsp_loss_noise[,2])
mydata <- melt(mse_data,id="R")
colnames(mydata)[2] = 'Estimator'
if (r == 5){
  plot_8 <- ggplot(data = mydata,aes(x=R,y=value,group = Estimator,
                                     color=Estimator,shape=Estimator))+
    geom_line(size=1.2)+
    geom_point(size=3)+
    theme_bw() +
    geom_segment(aes(x=0.25/rho**0.5,y=0,xend=0.25/rho**0.5,yend=2),linetype='dashed',size=1.2,color='#999999') + 
    scale_shape_discrete(labels=c('Estimator 1','Baseline 1'))+
    scale_colour_discrete(labels=c('Estimator 1','Baseline 1'))+
    scale_x_continuous(name="Noise level ratio R",limits = c(0,1.8/rho**0.5),breaks = seq(0,1.8/rho**0.5,0.5))+
    scale_y_continuous(name="Spectral norm",limits=c(0,26),breaks = seq(0,26,2))+
    labs(title="Spectral norm error with increasing noise level for r = 5")+
    theme(panel.grid.minor = element_blank(),legend.position = c(.15,.86),
          legend.box.background = element_rect(color="black"),
          axis.title.x=element_text(size=28),
          axis.title.y=element_text(size=28),
          axis.text.x=element_text(size=24),
          axis.text.y=element_text(size=24),
          legend.title =element_text(size=28),
          legend.text = element_text(size=28),
          plot.title = element_text(size=28,hjust=0.5))
}

if (r == 10){
  plot_8 <- ggplot(data = mydata,aes(x=R,y=value,group = Estimator,
                                     color=Estimator,shape=Estimator))+
    geom_line(size=1.2)+
    geom_point(size=3)+
    theme_bw() +
    geom_segment(aes(x=0.25/rho**0.5,y=0,xend=0.25/rho**0.5,yend=2.5),linetype='dashed',size=1.2,color='#999999') + 
    scale_shape_discrete(labels=c('Estimator 1','Baseline 1'))+
    scale_colour_discrete(labels=c('Estimator 1','Baseline 1'))+
    scale_x_continuous(name="Noise level ratio R",limits = c(0,1.8/rho**0.5),breaks = seq(0,1.8/rho**0.5,0.5))+
    scale_y_continuous(name="Spectral norm",limits=c(0,35),breaks = seq(0,35,5))+
    labs(title="Spectral norm error with increasing noise level for r = 10")+
    theme(panel.grid.minor = element_blank(),legend.position = c(.15,.86),
          legend.box.background = element_rect(color="black"),
          axis.title.x=element_text(size=28),
          axis.title.y=element_text(size=28),
          axis.text.x=element_text(size=24),
          axis.text.y=element_text(size=24),
          legend.title =element_text(size=28),
          legend.text = element_text(size=28),
          plot.title = element_text(size=28,hjust=0.5))
}


if (r == 20){
  plot_8 <- ggplot(data = mydata,aes(x=R,y=value,group = Estimator,
                                     color=Estimator,shape=Estimator))+
    geom_line(size=1.2)+
    geom_point(size=3)+
    theme_bw() +
    geom_segment(aes(x=0.23/rho**0.5,y=0,xend=0.23/rho**0.5,yend=3),linetype='dashed',size=1.2,color='#999999') + 
    scale_shape_discrete(labels=c('Estimator 1','Baseline 1'))+
    scale_colour_discrete(labels=c('Estimator 1','Baseline 1'))+
    scale_x_continuous(name="Noise level ratio R",limits = c(0,1.8/rho**0.5),breaks = seq(0,1.8/rho**0.5,0.5))+
    scale_y_continuous(name="Spectral norm",limits=c(0,50),breaks = seq(0,50,10))+
    labs(title="Spectral norm error with increasing noise level for r = 20")+
    theme(panel.grid.minor = element_blank(),legend.position = c(.15,.86),
          legend.box.background = element_rect(color="black"),
          axis.title.x=element_text(size=28),
          axis.title.y=element_text(size=28),
          axis.text.x=element_text(size=24),
          axis.text.y=element_text(size=24),
          legend.title =element_text(size=28),
          legend.text = element_text(size=28),
          plot.title = element_text(size=28,hjust=0.5))
}

ggsave(paste("plot/spectral_loss_noise_",r,".png",sep=''),plot=plot_8,width=12,height=8)



mse_data = data.frame("R"=index/3/rho**0.5,lmax_loss_noise[,1],lmax_loss_noise[,2])
mydata <- melt(mse_data,id="R")
colnames(mydata)[2] = 'Estimator'
if (r == 5){
  plot_9 <- ggplot(data = mydata,aes(x=R,y=value,group = Estimator,
                                     color=Estimator,shape=Estimator))+
    geom_line(size=1.2)+
    geom_point(size=3)+
    theme_bw() +
    geom_segment(aes(x=0.28/rho**0.5,y=0,xend=0.28/rho**0.5,yend=2.5),linetype='dashed',size=1.2,color='#999999') + 
    scale_shape_discrete(labels=c('Estimator 1','Baseline 1'))+
    scale_colour_discrete(labels=c('Estimator 1','Baseline 1'))+
    scale_x_continuous(name="Noise level ratio R",limits = c(0,1.8/rho**0.5),breaks = seq(0,1.8/rho**0.5,0.5))+
    scale_y_continuous(name="Maximum norm",limits=c(0,55),breaks = seq(0,55,5))+
    labs(title="Maximum norm error with increasing noise level for r = 5")+
    theme(panel.grid.minor = element_blank(),legend.position = c(.15,.86),
          legend.box.background = element_rect(color="black"),
          axis.title.x=element_text(size=28),
          axis.title.y=element_text(size=28),
          axis.text.x=element_text(size=24),
          axis.text.y=element_text(size=24),
          legend.title =element_text(size=28),
          legend.text = element_text(size=28),
          plot.title = element_text(size=28,hjust=0.5))
}

if (r == 10){
  plot_9 <- ggplot(data = mydata,aes(x=R,y=value,group = Estimator,
                                     color=Estimator,shape=Estimator))+
    geom_line(size=1.2)+
    geom_point(size=3)+
    theme_bw() +
    geom_segment(aes(x=0.275/rho**0.5,y=0,xend=0.275/rho**0.5,yend=3.5),linetype='dashed',size=1.2,color='#999999') + 
    scale_shape_discrete(labels=c('Estimator 1','Baseline 1'))+
    scale_colour_discrete(labels=c('Estimator 1','Baseline 1'))+
    scale_x_continuous(name="Noise level ratio R",limits = c(0,1.8/rho**0.5),breaks = seq(0,1.8/rho**0.5,0.5))+
    scale_y_continuous(name="Maximum norm",limits=c(0,80),breaks = seq(0,80,10))+
    labs(title="Maximum norm error with increasing noise level for r = 10")+
    theme(panel.grid.minor = element_blank(),legend.position = c(.15,.86),
          legend.box.background = element_rect(color="black"),
          axis.title.x=element_text(size=28),
          axis.title.y=element_text(size=28),
          axis.text.x=element_text(size=24),
          axis.text.y=element_text(size=24),
          legend.title =element_text(size=28),
          legend.text = element_text(size=28),
          plot.title = element_text(size=28,hjust=0.5))
}


if (r == 20){
  plot_9 <- ggplot(data = mydata,aes(x=R,y=value,group = Estimator,
                                     color=Estimator,shape=Estimator))+
    geom_line(size=1.2)+
    geom_point(size=3)+
    theme_bw() +
    geom_segment(aes(x=0.22/rho**0.5,y=0,xend=0.22/rho**0.5,yend=7),linetype='dashed',size=1.2,color='#999999') + 
    scale_shape_discrete(labels=c('Estimator 1','Baseline 1'))+
    scale_colour_discrete(labels=c('Estimator 1','Baseline 1'))+
    scale_x_continuous(name="Noise level ratio R",limits = c(0,1.8/rho**0.5),breaks = seq(0,1.8/rho**0.5,0.5))+
    scale_y_continuous(name="Maximum norm",limits=c(0,80),breaks = seq(0,80,10))+
    labs(title="Maximum norm error with increasing noise level for r = 20")+
    theme(panel.grid.minor = element_blank(),legend.position = c(.15,.86),
          legend.box.background = element_rect(color="black"),
          axis.title.x=element_text(size=28),
          axis.title.y=element_text(size=28),
          axis.text.x=element_text(size=24),
          axis.text.y=element_text(size=24),
          legend.title =element_text(size=28),
          legend.text = element_text(size=28),
          plot.title = element_text(size=28,hjust=0.5))
}

ggsave(paste("plot/maximum_loss_noise_",r,".png",sep=''),plot=plot_9,width=12,height=8)

