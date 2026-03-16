library(foreach)
library(doParallel)
library(ggplot2)
library(ggbreak)

setwd('/home/yuanyuanbenben/project_dmc/matrix_test/real_data_example2')
source('baseline_methods.R')
source('proposed_method.R')

train_data = read.csv('data/case1_traindata.csv')
X1_train = train_data$X1_train
X2_train = train_data$X2_train
Y_train = train_data$Y_train
validation_data = read.csv('data/case1_validationdata.csv')
X1_validation = validation_data$X1_validation
X2_validation = validation_data$X2_validation
Y_validation = validation_data$Y_validation
test_data = read.csv('data/case1_testdata.csv')
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


singular_value = svd(M_train)$d
# random_noise = rep(0,n)
# for (i in 1:100) {
#   print(i)
#   H = matrix(rnorm(m*n),m,n)
#   H_sparse = matrix(0,m,n)
#   for (i in 1:sample_size) {
#     H_sparse[X1_train[i],X2_train[i]] = H[X1_train[i],X2_train[i]]
#   }
#   random_noise = svd(H_sparse)$d + random_noise
# 
# }
# random_noise = random_noise/100
# 
# write.csv(random_noise,"output/case1_random_noise_svd.csv")
random_noise = read.csv("output/case1_random_noise_svd.csv")$x

data_used = data.frame('x1'=singular_value,'x2'=random_noise*sum(singular_value[333:666])/sum(random_noise[333:666]))

a<-ggplot(data_used,aes(x=x1))+
  theme(panel.grid.minor = element_blank()) +
  scale_x_continuous(name="Singular value",limits = c(0,190),breaks = seq(0,190,10))+
  # scale_y_continuous(name="Density",limits = c(0,0.015),breaks = seq(0,0.015,0.003))+
  geom_histogram(aes(y=..density..),
                 color="#88ada6", fill="#fffbf0",
                 alpha=1,
                 binwidth = 2,
                 center = 0) +
  geom_density(aes(x=x2),color='steelblue',linetype='dashed',size=1.2,adjust = 0.6)+
  # geom_segment(aes(x=singular_value[1],y=0,xend=singular_value[1],yend=0.0005),size=1.1,color='red')+
  scale_x_break(c(150,170))+
  ylab('Density') +
  theme_minimal() +
  labs(title="Singular values' distribution of rating matrix: Scenario 2")+
  theme(axis.title.x=element_text(size=28),
        axis.text.x=element_text(size=20,angle=15),
        axis.title.y=element_text(size=28),
        axis.text.y=element_text(size=20),
        legend.title =element_text(size=28),
        legend.text = element_text(size=28),
        plot.title = element_text(size=28,hjust=0.5))


ggsave("plot/amazon_histo1.png",plot=a,width=12,height=8)


# random_noise = rep(0,n)
# for (i in 1:100) {
#   print(i)
#   H = matrix(rnorm(m*n),m,n)
#   H_sparse = matrix(0,m,n)
#   for (i in 1:sample_size) {
#     H_sparse[X1_test[i],X2_test[i]] = H[X1_test[i],X2_test[i]]
#   }
#   random_noise = svd(H_sparse)$d + random_noise
# 
# }
# random_noise = random_noise/100
# write.csv(random_noise,"output/case1_test_random_noise_svd.csv")

random_noise = read.csv("output/case1_test_random_noise_svd.csv")$x

residual_our = read.csv('output/case1_nonconvex_residual_our.csv')$test_residual_our
residual_baseline1 = read.csv('output/case1_nonconvex_residual_baseline1.csv')$test_residual_baseline1

residual_our_convex = read.csv('output/case1_convex_residual_our.csv')$test_residual_our
residual_baseline1_convex = read.csv('output/case1_convex_residual_baseline1.csv')$test_residual_baseline1


data_used = data.frame('x1'=residual_our,'x2'=random_noise*sum(residual_our[333:666])/sum(random_noise[333:666]))
residual_1<-ggplot(data_used,aes(x=x1))+ 
  theme(panel.grid.minor = element_blank()) + 
  scale_x_continuous(name="Singular value",limits = c(-2,12),breaks = seq(-2,12,2))+
  scale_y_continuous(name="Density")+
  geom_histogram(aes(y=..density..), 
                 color="#88ada6", fill="#fffbf0", 
                 alpha=1,  
                 binwidth = 0.2,
                 # bins = 60,
                 center = 0) +
  stat_density(aes(x=x2),geom="line",position="identity", color='steelblue',linetype='dashed',size=1.2,adjust=0.2)+
  #geom_segment(aes(x=singular_value[34],y=0,xend=singular_value[34],yend=0.0009),size=1.1,color='red')+
  #scale_x_break(c(570, 3240))+scale_x_break(c(430,540))+
  ylab('Density') +
  labs(title="Residual matrix's singular values of Estimator 1")+
  theme_bw() + 
  theme(axis.title.x=element_text(size=28),
        axis.title.y=element_text(size=28),
        axis.text.x=element_text(size=24),
        axis.text.y=element_text(size=24),
        legend.title =element_text(size=28),
        legend.text = element_text(size=28),
        plot.title = element_text(size=28,hjust=0.5))


ggsave("plot/amazon_histo_residual1.png",plot=residual_1,width=12,height=8)

data_used = data.frame('x1'=residual_baseline1,'x2'=random_noise*sum(residual_our[333:666])/sum(random_noise[333:666]))
residual_baseline1<-ggplot(data_used,aes(x=x1))+
  theme(panel.grid.minor = element_blank()) +
  scale_x_continuous(name="Singular value",limits = c(-2,12),breaks = seq(-2,12,2))+
  scale_y_continuous(name="Density")+
  geom_histogram(aes(y=..density..),
                 color="#88ada6", fill="#fffbf0",
                 alpha=1,
                 binwidth = 0.2,
                 # bins = 60,
                 center = 0) +
  stat_density(aes(x=x2),geom="line",position="identity", color='steelblue',linetype='dashed',size=1.2,adjust=0.2)+
  #geom_segment(aes(x=singular_value[34],y=0,xend=singular_value[34],yend=0.0009),size=1.1,color='red')+
  #scale_x_break(c(570, 3240))+scale_x_break(c(430,540))+
  ylab('Density') +
  labs(title="Residual matrix's singular values of Baseline 1")+
  theme_bw() + 
  theme(axis.title.x=element_text(size=28),
        axis.title.y=element_text(size=28),
        axis.text.x=element_text(size=24),
        axis.text.y=element_text(size=24),
        legend.title =element_text(size=28),
        legend.text = element_text(size=28),
        plot.title = element_text(size=28,hjust=0.5))


ggsave("plot/amazon_histo_residual_baseline_1.png",plot=residual_baseline1,width=12,height=8)


data_used = data.frame('x1'=residual_our_convex,'x2'=random_noise*sum(residual_our_convex[333:666])/sum(random_noise[333:666]))
residual_2<-ggplot(data_used,aes(x=x1))+ 
  theme(panel.grid.minor = element_blank()) + 
  scale_x_continuous(name="Singular value",limits = c(-2,12),breaks = seq(-2,12,2))+
  scale_y_continuous(name="Density")+
  geom_histogram(aes(y=..density..), 
                 color="#88ada6", fill="#fffbf0", 
                 alpha=1,  
                 binwidth = 0.2,
                 # bins = 60,
                 center = 0) +
  stat_density(aes(x=x2),geom="line",position="identity", color='steelblue',linetype='dashed',size=1.2,adjust = 0.2)+
  #geom_segment(aes(x=singular_value[34],y=0,xend=singular_value[34],yend=0.0009),size=1.1,color='red')+
  #scale_x_break(c(570, 3240))+scale_x_break(c(430,540))+
  ylab('Density') + 
  labs(title="Residual matrix's singular values of Estimator 2")+
  theme_bw() + 
  theme(axis.title.x=element_text(size=28),
        axis.title.y=element_text(size=28),
        axis.text.x=element_text(size=24),
        axis.text.y=element_text(size=24),
        legend.title =element_text(size=28),
        legend.text = element_text(size=28),
        plot.title = element_text(size=28,hjust=0.5))


ggsave("plot/amazon_histo_convex_residual1.png",plot=residual_2,width=12,height=8)

data_used = data.frame('x1'=residual_baseline1_convex,'x2'=random_noise*sum(residual_our_convex[333:666])/sum(random_noise[333:666]))
residual_baseline2<-ggplot(data_used,aes(x=x1))+
  theme(panel.grid.minor = element_blank()) +
  scale_x_continuous(name="Singular value",limits = c(-2,12),breaks = seq(-2,12,2))+
  scale_y_continuous(name="Density")+
  geom_histogram(aes(y=..density..),
                 color="#88ada6", fill="#fffbf0",
                 alpha=1,
                 binwidth = 0.2,
                 # bins = 60,
                 center = 0) +
  stat_density(aes(x=x2),geom="line",position="identity", color='steelblue',linetype='dashed',size=1.2,adjust = 0.2)+
  #geom_segment(aes(x=singular_value[34],y=0,xend=singular_value[34],yend=0.0009),size=1.1,color='red')+
  #scale_x_break(c(570, 3240))+scale_x_break(c(430,540))+
  #ylab('Density') +
  ylab('Density') + 
  labs(title="Residual matrix's singular values of Baseline 2")+
  theme_bw() + 
  theme(axis.title.x=element_text(size=28),
        axis.title.y=element_text(size=28),
        axis.text.x=element_text(size=24),
        axis.text.y=element_text(size=24),
        legend.title =element_text(size=28),
        legend.text = element_text(size=28),
        plot.title = element_text(size=28,hjust=0.5))


ggsave("plot/amazon_histo_residual_convex_baseline_1.png",plot=residual_baseline2,width=12,height=8)
###############################################################################################

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


singular_value = svd(M_train)$d
# random_noise = rep(0,n)
# for (i in 1:100) {
#   print(i)
#   H = matrix(rnorm(m*n),m,n)
#   H_sparse = matrix(0,m,n)
#   for (i in 1:sample_size) {
#     H_sparse[X1_train[i],X2_train[i]] = H[X1_train[i],X2_train[i]]
#   }
#   random_noise = svd(H_sparse)$d + random_noise
# 
# }
# random_noise = random_noise/100
# 
# write.csv(random_noise,"output/case2_random_noise_svd.csv")
random_noise = read.csv("output/case2_random_noise_svd.csv")$x

data_used = data.frame('x1'=singular_value,'x2'=random_noise*sum(singular_value[333:666])/sum(random_noise[333:666]))

a<-ggplot(data_used,aes(x=x1))+
  theme(panel.grid.minor = element_blank()) +
  scale_x_continuous(name="Singular value",limits = c(0,190),breaks = seq(0,190,10))+
  # scale_y_continuous(name="Density",limits = c(0,0.015),breaks = seq(0,0.015,0.003))+
  geom_histogram(aes(y=..density..),
                 color="#88ada6", fill="#fffbf0",
                 alpha=1,
                 binwidth = 2,
                 center = 0) +
  geom_density(aes(x=x2),color='steelblue',linetype='dashed',size=1.2,adjust = 0.5)+
  geom_segment(aes(x=singular_value[1],y=0,xend=singular_value[1],yend=0.0005),size=1.1,color='red')+
  scale_x_break(c(150,170))+
  ylab('Density') +
  theme_minimal() +
  labs(title="Singular values' distribution of rating matrix: Scenario 3")+
  theme(axis.title.x=element_text(size=28),
        axis.text.x=element_text(size=20,angle=15),
        axis.title.y=element_text(size=28),
        axis.text.y=element_text(size=20),
        legend.title =element_text(size=28),
        legend.text = element_text(size=28),
        plot.title = element_text(size=28,hjust=0.5))


ggsave("plot/amazon_histo2.png",plot=a,width=12,height=8)


# random_noise = rep(0,n)
# for (i in 1:100) {
#   print(i)
#   H = matrix(rnorm(m*n),m,n)
#   H_sparse = matrix(0,m,n)
#   for (i in 1:sample_size) {
#     H_sparse[X1_test[i],X2_test[i]] = H[X1_test[i],X2_test[i]]
#   }
#   random_noise = svd(H_sparse)$d + random_noise
# 
# }
# random_noise = random_noise/100
# write.csv(random_noise,"output/case2_test_random_noise_svd.csv")

random_noise = read.csv("output/case2_test_random_noise_svd.csv")$x

residual_our = read.csv('output/case2_nonconvex_residual_our.csv')$test_residual_our
residual_baseline1 = read.csv('output/case2_nonconvex_residual_baseline1.csv')$test_residual_baseline1

residual_our_convex = read.csv('output/case2_convex_residual_our.csv')$test_residual_our
residual_baseline1_convex = read.csv('output/case2_convex_residual_baseline1.csv')$test_residual_baseline1

data_used = data.frame('x1'=residual_our,'x2'=random_noise*sum(residual_our[333:666])/sum(random_noise[333:666]))
residual_1<-ggplot(data_used,aes(x=x1))+ 
  theme(panel.grid.minor = element_blank()) + 
  scale_x_continuous(name="Singular value",limits = c(-2,12),breaks = seq(-2,12,2))+
  scale_y_continuous(name="Density")+
  geom_histogram(aes(y=..density..), 
                 color="#88ada6", fill="#fffbf0", 
                 alpha=1,  
                 binwidth = 0.2,
                 # bins = 60,
                 center = 0) +
  stat_density(aes(x=x2),geom="line",position="identity", color='steelblue',linetype='dashed',size=1.2,adjust=0.2)+
  #geom_segment(aes(x=singular_value[34],y=0,xend=singular_value[34],yend=0.0009),size=1.1,color='red')+
  #scale_x_break(c(570, 3240))+scale_x_break(c(430,540))+
  ylab('Density') +
  labs(title="Residual matrix's singular values of Estimator 1")+
  theme_bw() + 
  theme(axis.title.x=element_text(size=28),
        axis.title.y=element_text(size=28),
        axis.text.x=element_text(size=24),
        axis.text.y=element_text(size=24),
        legend.title =element_text(size=28),
        legend.text = element_text(size=28),
        plot.title = element_text(size=28,hjust=0.5))


ggsave("plot/amazon_histo_residual2.png",plot=residual_1,width=12,height=8)

data_used = data.frame('x1'=residual_baseline1,'x2'=random_noise*sum(residual_our[333:666])/sum(random_noise[333:666]))
residual_baseline1<-ggplot(data_used,aes(x=x1))+
  theme(panel.grid.minor = element_blank()) +
  scale_x_continuous(name="Singular value",limits = c(-2,12),breaks = seq(-2,12,2))+
  scale_y_continuous(name="Density")+
  geom_histogram(aes(y=..density..),
                 color="#88ada6", fill="#fffbf0",
                 alpha=1,
                 binwidth = 0.2,
                 # bins = 60,
                 center = 0) +
  stat_density(aes(x=x2),geom="line",position="identity", color='steelblue',linetype='dashed',size=1.2,adjust=0.2)+
  #geom_segment(aes(x=singular_value[34],y=0,xend=singular_value[34],yend=0.0009),size=1.1,color='red')+
  #scale_x_break(c(570, 3240))+scale_x_break(c(430,540))+
  ylab('Density') +
  labs(title="Residual matrix's singular values of Baseline 1")+
  theme_bw() + 
  theme(axis.title.x=element_text(size=28),
        axis.title.y=element_text(size=28),
        axis.text.x=element_text(size=24),
        axis.text.y=element_text(size=24),
        legend.title =element_text(size=28),
        legend.text = element_text(size=28),
        plot.title = element_text(size=28,hjust=0.5))


ggsave("plot/amazon_histo_residual_baseline_2.png",plot=residual_baseline1,width=12,height=8)


data_used = data.frame('x1'=residual_our_convex,'x2'=random_noise*sum(residual_our_convex[333:666])/sum(random_noise[333:666]))
residual_2<-ggplot(data_used,aes(x=x1))+ 
  theme(panel.grid.minor = element_blank()) + 
  scale_x_continuous(name="Singular value",limits = c(-2,12),breaks = seq(-2,12,2))+
  scale_y_continuous(name="Density")+
  geom_histogram(aes(y=..density..), 
                 color="#88ada6", fill="#fffbf0", 
                 alpha=1,  
                 binwidth = 0.2,
                 # bins = 60,
                 center = 0) +
  stat_density(aes(x=x2),geom="line",position="identity", color='steelblue',linetype='dashed',size=1.2,adjust = 0.2)+
  #geom_segment(aes(x=singular_value[34],y=0,xend=singular_value[34],yend=0.0009),size=1.1,color='red')+
  #scale_x_break(c(570, 3240))+scale_x_break(c(430,540))+
  ylab('Density') + 
  labs(title="Residual matrix's singular values of Estimator 2")+
  theme_bw() + 
  theme(axis.title.x=element_text(size=28),
        axis.title.y=element_text(size=28),
        axis.text.x=element_text(size=24),
        axis.text.y=element_text(size=24),
        legend.title =element_text(size=28),
        legend.text = element_text(size=28),
        plot.title = element_text(size=28,hjust=0.5))


ggsave("plot/amazon_histo_convex_residual2.png",plot=residual_2,width=12,height=8)

data_used = data.frame('x1'=residual_baseline1_convex,'x2'=random_noise*sum(residual_our_convex[333:666])/sum(random_noise[333:666]))
residual_baseline2<-ggplot(data_used,aes(x=x1))+
  theme(panel.grid.minor = element_blank()) +
  scale_x_continuous(name="Singular value",limits = c(-2,12),breaks = seq(-2,12,2))+
  scale_y_continuous(name="Density")+
  geom_histogram(aes(y=..density..),
                 color="#88ada6", fill="#fffbf0",
                 alpha=1,
                 binwidth = 0.2,
                 # bins = 60,
                 center = 0) +
  stat_density(aes(x=x2),geom="line",position="identity", color='steelblue',linetype='dashed',size=1.2,adjust = 0.2)+
  #geom_segment(aes(x=singular_value[34],y=0,xend=singular_value[34],yend=0.0009),size=1.1,color='red')+
  #scale_x_break(c(570, 3240))+scale_x_break(c(430,540))+
  #ylab('Density') +
  ylab('Density') + 
  labs(title="Residual matrix's singular values of Baseline 2")+
  theme_bw() + 
  theme(axis.title.x=element_text(size=28),
        axis.title.y=element_text(size=28),
        axis.text.x=element_text(size=24),
        axis.text.y=element_text(size=24),
        legend.title =element_text(size=28),
        legend.text = element_text(size=28),
        plot.title = element_text(size=28,hjust=0.5))


ggsave("plot/amazon_histo_residual_convex_baseline_2.png",plot=residual_baseline2,width=12,height=8)
########################################################################################################################

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


singular_value = svd(M_train)$d
# random_noise = rep(0,n)
# for (i in 1:100) {
#   print(i)
#   H = matrix(rnorm(m*n),m,n)
#   H_sparse = matrix(0,m,n)
#   for (i in 1:sample_size) {
#     H_sparse[X1_train[i],X2_train[i]] = H[X1_train[i],X2_train[i]]
#   }
#   random_noise = svd(H_sparse)$d + random_noise
# 
# }
# random_noise = random_noise/100
# 
# write.csv(random_noise,"output/case3_random_noise_svd.csv")
random_noise = read.csv("output/case3_random_noise_svd.csv")$x

data_used = data.frame('x1'=singular_value,'x2'=random_noise*sum(singular_value[333:666])/sum(random_noise[333:666]))

a<-ggplot(data_used,aes(x=x1))+
  theme(panel.grid.minor = element_blank()) +
  scale_x_continuous(name="Singular value",limits = c(0,190),breaks = seq(0,190,10))+
  # scale_y_continuous(name="Density",limits = c(0,0.015),breaks = seq(0,0.015,0.003))+
  geom_histogram(aes(y=..density..),
                 color="#88ada6", fill="#fffbf0",
                 alpha=1,
                 binwidth = 2,
                 center = 0) +
  geom_density(aes(x=x2),color='steelblue',linetype='dashed',size=1.2,adjust = 0.5)+
  geom_segment(aes(x=singular_value[1],y=0,xend=singular_value[1],yend=0.0005),size=1.1,color='red')+
  scale_x_break(c(150,170))+
  ylab('Density') +
  theme_minimal() +
  labs(title="Singular values' distribution of rating matrix: Scenario 4")+
  theme(axis.title.x=element_text(size=28),
        axis.text.x=element_text(size=20,angle=15),
        axis.title.y=element_text(size=28),
        axis.text.y=element_text(size=20),
        legend.title =element_text(size=28),
        legend.text = element_text(size=28),
        plot.title = element_text(size=28,hjust=0.5))


ggsave("plot/amazon_histo3.png",plot=a,width=12,height=8)


# random_noise = rep(0,n)
# for (i in 1:100) {
#   print(i)
#   H = matrix(rnorm(m*n),m,n)
#   H_sparse = matrix(0,m,n)
#   for (i in 1:sample_size) {
#     H_sparse[X1_test[i],X2_test[i]] = H[X1_test[i],X2_test[i]]
#   }
#   random_noise = svd(H_sparse)$d + random_noise
# 
# }
# random_noise = random_noise/100
# write.csv(random_noise,"output/case3_test_random_noise_svd.csv")

random_noise = read.csv("output/case3_test_random_noise_svd.csv")$x

residual_our = read.csv('output/case3_nonconvex_residual_our.csv')$test_residual_our
residual_baseline1 = read.csv('output/case3_nonconvex_residual_baseline1.csv')$test_residual_baseline1

residual_our_convex = read.csv('output/case3_convex_residual_our.csv')$test_residual_our
residual_baseline1_convex = read.csv('output/case3_convex_residual_baseline1.csv')$test_residual_baseline1

data_used = data.frame('x1'=residual_our,'x2'=random_noise*sum(residual_our[333:666])/sum(random_noise[333:666]))
residual_1<-ggplot(data_used,aes(x=x1))+ 
  theme(panel.grid.minor = element_blank()) + 
  scale_x_continuous(name="Singular value",limits = c(-2,12),breaks = seq(-2,12,2))+
  scale_y_continuous(name="Density")+
  geom_histogram(aes(y=..density..), 
                 color="#88ada6", fill="#fffbf0", 
                 alpha=1,  
                 binwidth = 0.2,
                 # bins = 60,
                 center = 0) +
  stat_density(aes(x=x2),geom="line",position="identity", color='steelblue',linetype='dashed',size=1.2,adjust=0.25)+
  #geom_segment(aes(x=singular_value[34],y=0,xend=singular_value[34],yend=0.0009),size=1.1,color='red')+
  #scale_x_break(c(570, 3240))+scale_x_break(c(430,540))+
  ylab('Density') +
  labs(title="Residual matrix's singular values of Estimator 1")+
  theme_bw() + 
  theme(axis.title.x=element_text(size=28),
        axis.title.y=element_text(size=28),
        axis.text.x=element_text(size=24),
        axis.text.y=element_text(size=24),
        legend.title =element_text(size=28),
        legend.text = element_text(size=28),
        plot.title = element_text(size=28,hjust=0.5))


ggsave("plot/amazon_histo_residual3.png",plot=residual_1,width=12,height=8)

data_used = data.frame('x1'=residual_baseline1,'x2'=random_noise*sum(residual_our[333:666])/sum(random_noise[333:666]))
residual_baseline1<-ggplot(data_used,aes(x=x1))+
  theme(panel.grid.minor = element_blank()) +
  scale_x_continuous(name="Singular value",limits = c(-2,12),breaks = seq(-2,12,2))+
  scale_y_continuous(name="Density")+
  geom_histogram(aes(y=..density..),
                 color="#88ada6", fill="#fffbf0",
                 alpha=1,
                 binwidth = 0.2,
                 # bins = 60,
                 center = 0) +
  stat_density(aes(x=x2),geom="line",position="identity", color='steelblue',linetype='dashed',size=1.2,adjust=0.25)+
  #geom_segment(aes(x=singular_value[34],y=0,xend=singular_value[34],yend=0.0009),size=1.1,color='red')+
  #scale_x_break(c(570, 3240))+scale_x_break(c(430,540))+
  ylab('Density') +
  labs(title="Residual matrix's singular values of Baseline 1")+
  theme_bw() + 
  theme(axis.title.x=element_text(size=28),
        axis.title.y=element_text(size=28),
        axis.text.x=element_text(size=24),
        axis.text.y=element_text(size=24),
        legend.title =element_text(size=28),
        legend.text = element_text(size=28),
        plot.title = element_text(size=28,hjust=0.5))


ggsave("plot/amazon_histo_residual_baseline_3.png",plot=residual_baseline1,width=12,height=8)


data_used = data.frame('x1'=residual_our_convex,'x2'=random_noise*sum(residual_our_convex[333:666])/sum(random_noise[333:666]))
residual_2<-ggplot(data_used,aes(x=x1))+ 
  theme(panel.grid.minor = element_blank()) + 
  scale_x_continuous(name="Singular value",limits = c(-2,12),breaks = seq(-2,12,2))+
  scale_y_continuous(name="Density")+
  geom_histogram(aes(y=..density..), 
                 color="#88ada6", fill="#fffbf0", 
                 alpha=1,  
                 binwidth = 0.2,
                 # bins = 60,
                 center = 0) +
  stat_density(aes(x=x2),geom="line",position="identity", color='steelblue',linetype='dashed',size=1.2,adjust = 0.25)+
  #geom_segment(aes(x=singular_value[34],y=0,xend=singular_value[34],yend=0.0009),size=1.1,color='red')+
  #scale_x_break(c(570, 3240))+scale_x_break(c(430,540))+
  ylab('Density') + 
  labs(title="Residual matrix's singular values of Estimator 2")+
  theme_bw() + 
  theme(axis.title.x=element_text(size=28),
        axis.title.y=element_text(size=28),
        axis.text.x=element_text(size=24),
        axis.text.y=element_text(size=24),
        legend.title =element_text(size=28),
        legend.text = element_text(size=28),
        plot.title = element_text(size=28,hjust=0.5))


ggsave("plot/amazon_histo_convex_residual3.png",plot=residual_2,width=12,height=8)

data_used = data.frame('x1'=residual_baseline1_convex,'x2'=random_noise*sum(residual_our_convex[333:666])/sum(random_noise[333:666]))
residual_baseline2<-ggplot(data_used,aes(x=x1))+
  theme(panel.grid.minor = element_blank()) +
  scale_x_continuous(name="Singular value",limits = c(-2,12),breaks = seq(-2,12,2))+
  scale_y_continuous(name="Density")+
  geom_histogram(aes(y=..density..),
                 color="#88ada6", fill="#fffbf0",
                 alpha=1,
                 binwidth = 0.2,
                 # bins = 60,
                 center = 0) +
  stat_density(aes(x=x2),geom="line",position="identity", color='steelblue',linetype='dashed',size=1.2,adjust = 0.25)+
  #geom_segment(aes(x=singular_value[34],y=0,xend=singular_value[34],yend=0.0009),size=1.1,color='red')+
  #scale_x_break(c(570, 3240))+scale_x_break(c(430,540))+
  #ylab('Density') +
  ylab('Density') + 
  labs(title="Residual matrix's singular values of Baseline 2")+
  theme_bw() + 
  theme(axis.title.x=element_text(size=28),
        axis.title.y=element_text(size=28),
        axis.text.x=element_text(size=24),
        axis.text.y=element_text(size=24),
        legend.title =element_text(size=28),
        legend.text = element_text(size=28),
        plot.title = element_text(size=28,hjust=0.5))


ggsave("plot/amazon_histo_residual_convex_baseline_3.png",plot=residual_baseline2,width=12,height=8)

########################################################################################################################

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


singular_value = svd(M_train)$d
# random_noise = rep(0,n)
# for (i in 1:100) {
#   print(i)
#   H = matrix(rnorm(m*n),m,n)
#   H_sparse = matrix(0,m,n)
#   for (i in 1:sample_size) {
#     H_sparse[X1_train[i],X2_train[i]] = H[X1_train[i],X2_train[i]]
#   }
#   random_noise = svd(H_sparse)$d + random_noise
# 
# }
# random_noise = random_noise/100
# 
# write.csv(random_noise,"output/case4_random_noise_svd.csv")
random_noise = read.csv("output/case4_random_noise_svd.csv")$x

data_used = data.frame('x1'=singular_value,'x2'=random_noise*sum(singular_value[333:666])/sum(random_noise[333:666]))

a<-ggplot(data_used,aes(x=x1))+
  theme(panel.grid.minor = element_blank()) +
  scale_x_continuous(name="Singular value",limits = c(0,190),breaks = seq(0,190,10))+
  # scale_y_continuous(name="Density",limits = c(0,0.015),breaks = seq(0,0.015,0.003))+
  geom_histogram(aes(y=..density..),
                 color="#88ada6", fill="#fffbf0",
                 alpha=1,
                 binwidth = 2,
                 center = 0) +
  geom_density(aes(x=x2),color='steelblue',linetype='dashed',size=1.2,adjust = 0.5)+
  geom_segment(aes(x=singular_value[1],y=0,xend=singular_value[1],yend=0.0005),size=1.1,color='red')+
  scale_x_break(c(150,170))+
  ylab('Density') +
  theme_minimal() +
  labs(title="Singular values' distribution of rating matrix: Scenario 1")+
  theme(axis.title.x=element_text(size=28),
        axis.text.x=element_text(size=20,angle=15),
        axis.title.y=element_text(size=28),
        axis.text.y=element_text(size=20),
        legend.title =element_text(size=28),
        legend.text = element_text(size=28),
        plot.title = element_text(size=28,hjust=0.5))


ggsave("plot/amazon_histo4.png",plot=a,width=12,height=8)

# 
# random_noise = rep(0,n)
# for (i in 1:100) {
#   print(i)
#   H = matrix(rnorm(m*n),m,n)
#   H_sparse = matrix(0,m,n)
#   for (i in 1:sample_size) {
#     H_sparse[X1_test[i],X2_test[i]] = H[X1_test[i],X2_test[i]]
#   }
#   random_noise = svd(H_sparse)$d + random_noise
# 
# }
# random_noise = random_noise/100
# write.csv(random_noise,"output/case4_test_random_noise_svd.csv")

random_noise = read.csv("output/case4_test_random_noise_svd.csv")$x

residual_our = read.csv('output/case4_nonconvex_residual_our.csv')$test_residual_our
residual_baseline1 = read.csv('output/case4_nonconvex_residual_baseline1.csv')$test_residual_baseline1

residual_our_convex = read.csv('output/case4_convex_residual_our.csv')$test_residual_our
residual_baseline1_convex = read.csv('output/case4_convex_residual_baseline1.csv')$test_residual_baseline1


data_used = data.frame('x1'=residual_our,'x2'=random_noise*sum(residual_our[333:666])/sum(random_noise[333:666]))
residual_1<-ggplot(data_used,aes(x=x1))+ 
  theme(panel.grid.minor = element_blank()) + 
  scale_x_continuous(name="Singular value",limits = c(-2,12),breaks = seq(-2,12,2))+
  scale_y_continuous(name="Density")+
  geom_histogram(aes(y=..density..), 
                 color="#88ada6", fill="#fffbf0", 
                 alpha=1,  
                 binwidth = 0.2,
                 # bins = 60,
                 center = 0) +
  stat_density(aes(x=x2),geom="line",position="identity", color='steelblue',linetype='dashed',size=1.2,adjust=0.2)+
  #geom_segment(aes(x=singular_value[34],y=0,xend=singular_value[34],yend=0.0009),size=1.1,color='red')+
  #scale_x_break(c(570, 3240))+scale_x_break(c(430,540))+
  ylab('Density') +
  labs(title="Residual matrix's singular values of Estimator 1")+
  theme_bw() + 
  theme(axis.title.x=element_text(size=28),
        axis.title.y=element_text(size=28),
        axis.text.x=element_text(size=24),
        axis.text.y=element_text(size=24),
        legend.title =element_text(size=28),
        legend.text = element_text(size=28),
        plot.title = element_text(size=28,hjust=0.5))


ggsave("plot/amazon_histo_residual4.png",plot=residual_1,width=12,height=8)

data_used = data.frame('x1'=residual_baseline1,'x2'=random_noise*sum(residual_our[333:666])/sum(random_noise[333:666]))
residual_baseline1<-ggplot(data_used,aes(x=x1))+
  theme(panel.grid.minor = element_blank()) +
  scale_x_continuous(name="Singular value",limits = c(-2,12),breaks = seq(-2,12,2))+
  scale_y_continuous(name="Density")+
  geom_histogram(aes(y=..density..),
                 color="#88ada6", fill="#fffbf0",
                 alpha=1,
                 binwidth = 0.2,
                 # bins = 60,
                 center = 0) +
  stat_density(aes(x=x2),geom="line",position="identity", color='steelblue',linetype='dashed',size=1.2,adjust=0.2)+
  #geom_segment(aes(x=singular_value[34],y=0,xend=singular_value[34],yend=0.0009),size=1.1,color='red')+
  #scale_x_break(c(570, 3240))+scale_x_break(c(430,540))+
  ylab('Density') +
  labs(title="Residual matrix's singular values of Baseline 1")+
  theme_bw() + 
  theme(axis.title.x=element_text(size=28),
        axis.title.y=element_text(size=28),
        axis.text.x=element_text(size=24),
        axis.text.y=element_text(size=24),
        legend.title =element_text(size=28),
        legend.text = element_text(size=28),
        plot.title = element_text(size=28,hjust=0.5))


ggsave("plot/amazon_histo_residual_baseline_4.png",plot=residual_baseline1,width=12,height=8)


data_used = data.frame('x1'=residual_our_convex,'x2'=random_noise*sum(residual_our_convex[333:666])/sum(random_noise[333:666]))
residual_2<-ggplot(data_used,aes(x=x1))+ 
  theme(panel.grid.minor = element_blank()) + 
  scale_x_continuous(name="Singular value",limits = c(-2,12),breaks = seq(-2,12,2))+
  scale_y_continuous(name="Density")+
  geom_histogram(aes(y=..density..), 
                 color="#88ada6", fill="#fffbf0", 
                 alpha=1,  
                 binwidth = 0.2,
                 # bins = 60,
                 center = 0) +
  stat_density(aes(x=x2),geom="line",position="identity", color='steelblue',linetype='dashed',size=1.2,adjust = 0.2)+
  #geom_segment(aes(x=singular_value[34],y=0,xend=singular_value[34],yend=0.0009),size=1.1,color='red')+
  #scale_x_break(c(570, 3240))+scale_x_break(c(430,540))+
  ylab('Density') + 
  labs(title="Residual matrix's singular values of Estimator 2")+
  theme_bw() + 
  theme(axis.title.x=element_text(size=28),
        axis.title.y=element_text(size=28),
        axis.text.x=element_text(size=24),
        axis.text.y=element_text(size=24),
        legend.title =element_text(size=28),
        legend.text = element_text(size=28),
        plot.title = element_text(size=28,hjust=0.5))


ggsave("plot/amazon_histo_convex_residual4.png",plot=residual_2,width=12,height=8)

data_used = data.frame('x1'=residual_baseline1_convex,'x2'=random_noise*sum(residual_our_convex[333:666])/sum(random_noise[333:666]))
residual_baseline2<-ggplot(data_used,aes(x=x1))+
  theme(panel.grid.minor = element_blank()) +
  scale_x_continuous(name="Singular value",limits = c(-2,12),breaks = seq(-2,12,2))+
  scale_y_continuous(name="Density")+
  geom_histogram(aes(y=..density..),
                 color="#88ada6", fill="#fffbf0",
                 alpha=1,
                 binwidth = 0.2,
                 # bins = 60,
                 center = 0) +
  stat_density(aes(x=x2),geom="line",position="identity", color='steelblue',linetype='dashed',size=1.2,adjust = 0.2)+
  #geom_segment(aes(x=singular_value[34],y=0,xend=singular_value[34],yend=0.0009),size=1.1,color='red')+
  #scale_x_break(c(570, 3240))+scale_x_break(c(430,540))+
  #ylab('Density') +
  ylab('Density') + 
  labs(title="Residual matrix's singular values of Baseline 2")+
  theme_bw() + 
  theme(axis.title.x=element_text(size=28),
        axis.title.y=element_text(size=28),
        axis.text.x=element_text(size=24),
        axis.text.y=element_text(size=24),
        legend.title =element_text(size=28),
        legend.text = element_text(size=28),
        plot.title = element_text(size=28,hjust=0.5))


ggsave("plot/amazon_histo_residual_convex_baseline_4.png",plot=residual_baseline2,width=12,height=8)
