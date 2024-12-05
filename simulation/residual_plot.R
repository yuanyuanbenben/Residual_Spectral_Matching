library(foreach)
library(doParallel)
library(ggplot2)
library(ggbreak)

setwd('/home/yuanyuanbenben/project_dmc/matrix_test/mc_spectral_method')
source('baseline_methods.R')
source('proposed_method.R')


m = 500
n = 250
p = n/m
rho = 0.2
r = 10
sigma = 1
lambda_plus = m**(0.5) * sigma * (1 + p**0.5) 
lambda_ = m**0.5 * sigma * (1 - p**0.5) 
# setting rank
s = 10
stepsize1 = 100
stepsize2 = 10

set.seed(20240508)

# noise setting 
#H = matrix(rnorm(m*n),m,n)
#H = matrix(rbinom(m*n,1,0.5),m,n) * 2 - 1
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



# random_noise = rep(0,n)
# for (i in 1:100) {
#   print(i)
#   H = matrix(rnorm(m*n),m,n)
#   H_sparse = matrix(rnorm(m*n),m,n)
#   for (i in 1:sample_size) {
#     H_sparse[X1_index[i],X2_index[i]] = H[X1_index[i],X2_index[i]]
#   }
#   random_noise = random_noise + svd(H_sparse)$d
# }
# random_noise = random_noise/10
# write.csv(random_noise,"output/random_noise_svd_10.csv")

random_noise = read.csv("output/random_noise_svd_10.csv")$x

# singular_value = svd(M_obs)$d
# 
# 
# data_used = data.frame('x1'=singular_value,'x2'=random_noise*sum(singular_value[333:666])/sum(random_noise[333:666]))
# 
# a<-ggplot(data_used,aes(x=x1))+ 
#   theme(panel.grid.minor = element_blank()) + 
#   scale_x_continuous(name="Singular value",limits = c(0,3300),breaks = seq(0,3300,50))+
#   # scale_y_continuous(name="Density",limits = c(0,0.015),breaks = seq(0,0.015,0.003))+
#   geom_histogram(aes(y=..density..), 
#                  color="#88ada6", fill="#fffbf0", 
#                  alpha=1,  
#                  binwidth = 2, 
#                  center = 0) +
#   geom_density(aes(x=x2),color='steelblue',linetype='dashed',size=1.2)+
#   geom_segment(aes(x=singular_value[34],y=0,xend=singular_value[34],yend=0.0009),size=1.1,color='red')+
#   scale_x_break(c(570, 3240))+scale_x_break(c(430,540))+
#   ylab('Density') + 
#   theme_minimal()
# 
# 
# ggsave("plot/histo2.png",plot=a,width=12,height=8)
# 



residual_our = read.csv('output/nonconvex_residual_our1_500_250_0.2_10_10_.csv')
residual_baseline = read.csv('output/nonconvex_residual_baseline1_500_250_0.2_10_10_.csv')

residual_our_nonconvex = residual_our$residual_our
residual_baseline_nonconvex = residual_baseline$residual_baseline1
sigma_hat = sum(residual_our_nonconvex[80:160])/sum(random_noise[80:160])
data_used = data.frame('x1'=residual_our_nonconvex,'x2'=random_noise*sigma_hat)
# root_mp_func <- function(y){
#   x = y/500**0.5
#   x1 = sigma_hat**2 * (1+p**0.5)**2 - x**2 
#   x1_index = x1 < 0 
#   x1[x1_index] = 0
#   x2 = x**2 -sigma_hat**2 * (1-p**0.5)**2 
#   x2_index = x2 < 0 
#   x2[x2_index] = 0
#   return(((x1*x2)**0.5/pi/sigma_hat**2/p/x)*(2*sigma_hat*p**0.5*500**0.5/250))
# }
residual_1<-ggplot(data_used,aes(x=x1))+ 
  theme(panel.grid.minor = element_blank()) + 
  scale_x_continuous(name="Singular value",limits = c(-1,16),breaks = seq(-1,16,1))+
  scale_y_continuous(name="Density",limits = c(0,0.125))+
  geom_histogram(aes(y=..density..), 
                 color="#88ada6", fill="#fffbf0", 
                 alpha=1,  
                 binwidth = 0.35, 
                 center = 0) +
  # geom_function(fun=root_mp_func,linetype='dashed',size=1.2,color='steelblue')+
  geom_density(aes(x=x2),color='steelblue',linetype='dashed',size=1.2,adjust = 0.5)+
  # geom_segment(aes(x=singular_value[8],y=0,xend=singular_value[8],yend=0.0005),size=1.1,color='red')+
  labs(title="Residual matrix\'s singular values of Estimator 1")+
  theme_bw() + 
  theme(axis.title.x=element_text(size=28),
        axis.title.y=element_text(size=28),
        axis.text.x=element_text(size=24),
        axis.text.y=element_text(size=24),
        legend.title =element_text(size=28),
        legend.text = element_text(size=28),
        plot.title = element_text(size=28,hjust=0.5))
  


ggsave("plot/histo_residual_nonconvex.png",plot=residual_1,width=12,height=8)


sigma_hat = sum(residual_baseline_nonconvex[80:160])/sum(random_noise[80:160])
data_used2 = data.frame('x1'=residual_baseline_nonconvex,'x2'=random_noise*sigma_hat)
# root_mp_func <- function(y){
#   x = y/500**0.5
#   x1 = sigma_hat**2 * (1+p**0.5)**2 - x**2 
#   x1_index = x1 < 0 
#   x1[x1_index] = 0
#   x2 = x**2 -sigma_hat**2 * (1-p**0.5)**2 
#   x2_index = x2 < 0 
#   x2[x2_index] = 0
#   return(((x1*x2)**0.5/pi/sigma_hat**2/p/x)*(2*sigma_hat*p**0.5*500**0.5/250))
# }
residual_2<-ggplot(data_used2,aes(x=x1))+ 
  # theme(panel.grid.minor = element_blank()) + 
  scale_x_continuous(name="Singular value",limits = c(-1,16),breaks = seq(-1,16,1))+
  scale_y_continuous(name="Density",limits = c(0,0.125))+
  geom_histogram(aes(y=..density..), 
                 color="#88ada6", 
                 fill = "#fffbf0",
                 alpha=1,  
                 binwidth = 0.35,
                 # bins = 60,
                 center = 0) + 
  # geom_function(fun=root_mp_func,linetype='dashed',size=1.2,color='steelblue')+
  geom_density(aes(x=x2),color='steelblue',linetype='dashed',size=1.2,adjust = 0.5)+
  # geom_segment(aes(x=residual_baseline_nonconvex[1],y=0,xend=residual_baseline_nonconvex[1],yend=0.08/7),size=1.1,color='red')+
  labs(title="Residual matrix\'s singular values of Baseline 1")+
  theme_bw() + 
  theme(axis.title.x=element_text(size=28),
        axis.title.y=element_text(size=28),
        axis.text.x=element_text(size=24),
        axis.text.y=element_text(size=24),
        legend.title =element_text(size=28),
        legend.text = element_text(size=28),
        plot.title = element_text(size=28,hjust=0.5))

ggsave("plot/histo_residual_nonconvex_baseline.png",plot=residual_2,width=12,height=8)


residual_our_convex = residual_our$residual_our_convex
residual_baseline_convex = residual_baseline$residual_baseline1_convex
sigma_hat = sum(residual_our_convex[80:160])/sum(random_noise[80:160])
data_used3 = data.frame('x1'=residual_our_convex, 'x2'=random_noise*sigma_hat)
# root_mp_func <- function(y){
#   x = y/500**0.5
#   x1 = sigma_hat**2 * (1+p**0.5)**2 - x**2 
#   x1_index = x1 < 0 
#   x1[x1_index] = 0
#   x2 = x**2 -sigma_hat**2 * (1-p**0.5)**2 
#   x2_index = x2 < 0 
#   x2[x2_index] = 0
#   return(((x1*x2)**0.5/pi/sigma_hat**2/p/x)*(2*sigma_hat*p**0.5*500**0.5/250))
# }
residual_3<-ggplot(data_used3,aes(x=x1))+ 
  theme(panel.grid.minor = element_blank()) + 
  scale_x_continuous(name="Singular value",limits = c(-1,16),breaks = seq(-1,16,1))+
  scale_y_continuous(name="Density",limits = c(0,0.125))+
  geom_histogram(aes(y=..density..), 
                 color="#88ada6", fill="#fffbf0", 
                 alpha=1,  
                 binwidth = 0.35, 
                 center = 0) +
  # geom_function(fun=root_mp_func,linetype='dashed',size=1.2,color='steelblue')+
  geom_density(aes(x=x2),color='steelblue',linetype='dashed',size=1.2,adjust = 0.5)+
  labs(title="Residual matrix\'s singular values of Estimator 2")+
  theme_bw() + 
  theme(axis.title.x=element_text(size=28),
        axis.title.y=element_text(size=28),
        axis.text.x=element_text(size=24),
        axis.text.y=element_text(size=24),
        legend.title =element_text(size=28),
        legend.text = element_text(size=28),
        plot.title = element_text(size=28,hjust=0.5))


ggsave("plot/histo_residual_convex.png",plot=residual_3,width=12,height=8)


sigma_hat = sum(residual_baseline_convex[80:160])/sum(random_noise[80:160])
data_used4 = data.frame('x1'=residual_baseline_convex,'x2'=random_noise*sigma_hat)
# root_mp_func <- function(y){
#   x = y/500**0.5
#   x1 = sigma_hat**2 * (1+p**0.5)**2 - x**2 
#   x1_index = x1 < 0 
#   x1[x1_index] = 0
#   x2 = x**2 -sigma_hat**2 * (1-p**0.5)**2 
#   x2_index = x2 < 0 
#   x2[x2_index] = 0
#   return(((x1*x2)**0.5/pi/sigma_hat**2/p/x)*(2*sigma_hat*p**0.5*500**0.5/250))
# }
residual_4<-ggplot(data_used4,aes(x=x1))+ 
  theme(panel.grid.minor = element_blank()) + 
  scale_x_continuous(name="Singular value",limits = c(-1,16),breaks = seq(-1,16,1))+
  scale_y_continuous(name="Density",limits = c(0,0.125))+
  geom_histogram(aes(y=..density..), 
                 color="#88ada6", fill="#fffbf0", 
                 alpha=1,  
                 binwidth = 0.35, 
                 center = 0) +
  # geom_function(fun=root_mp_func,linetype='dashed',size=1.2,color='steelblue')+
  geom_density(aes(x=x2),color='steelblue',linetype='dashed',size=1.2,adjust = 0.5)+
  labs(title="Residual matrix\'s singular values of Baseline 2")+
  theme_bw() + 
  theme(axis.title.x=element_text(size=28),
        axis.title.y=element_text(size=28),
        axis.text.x=element_text(size=24),
        axis.text.y=element_text(size=24),
        legend.title =element_text(size=28),
        legend.text = element_text(size=28),
        plot.title = element_text(size=28,hjust=0.5))

ggsave("plot/histo_residual_convex_baseline.png",plot=residual_4,width=12,height=8)













