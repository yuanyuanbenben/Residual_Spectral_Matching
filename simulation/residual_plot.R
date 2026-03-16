# Residual matrix singular value distribution visualization
# This script generates publication-quality plots showing the singular value distributions
# of residual matrices from four matrix completion methods:
# 1. Proposed nonconvex method (Estimator 1)
# 2. Baseline nonconvex method (Baseline 1)
# 3. Proposed convex method (Estimator 2)
# 4. Baseline convex method (Baseline 2)
#
# The plots compare empirical residual distributions with theoretical Marchenko-Pastur predictions,
# demonstrating the effectiveness of spectral correction.
#
# Input: CSV files with residual singular values from simulation scripts
# Output: PNG plots in plot/ directory

library(ggplot2)
library(ggbreak)

source('../R/baseline_methods.R')
source('../R/proposed_method.R')

# Simulation parameters (must match the simulation script that generated the data)
m = 500
n = 250
p = n/m
rho = 0.2
r = 10
sigma = 1
lambda_plus = (m*n)**(0.5) * (1 + p**0.5)
s = 10
stepsize1 = 100
stepsize2 = 10

set.seed(20240508)

# Generate reference noise distribution
# Average singular values of 100 random Gaussian matrices with same sparsity pattern
H = matrix(rnorm(m*n),m,n) * 0.5**0.5 + (matrix(rbinom(m*n,1,0.5),m,n)*2 - 1) * 0.5 ** 0.5

S = matrix(rnorm(m*n),m,n)
svd_S = svd(S)
u = svd_S$u[1:m,1:r]
v = svd_S$v[1:n,1:r]
if (r == 5){
  M_0 = u %*% diag(c(1,1.2,1.4,1.6,1.8)) %*% t(v) * lambda_plus
}
if (r == 10){
  M_0 = u %*% diag(c(1,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9)) %*% t(v) * lambda_plus
}
if (r == 20){
  M_0 = u %*% diag(c(1,1.05,1.1,1.15,1.2,1.25,1.3,1.35,1.4,1.45,1.5,1.55,1.6,1.65,1.7,1.75,1.8,1.85,1.9,1.95)) %*% t(v) * lambda_plus
}

M = H*sigma + M_0

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

# Generate reference noise singular values
random_noise = rep(0,n)
for (i in 1:100) {
  print(i)
  H = matrix(rnorm(m*n),m,n)
  H_sparse = matrix(rnorm(m*n),m,n)
  for (i in 1:sample_size) {
    H_sparse[X1_index[i],X2_index[i]] = H[X1_index[i],X2_index[i]]
  }
  random_noise = random_noise + svd(H_sparse)$d
}
random_noise = random_noise/100
write.csv(random_noise,"output/random_noise_svd_10.csv")

# Read residual data
random_noise = read.csv("output/random_noise_svd_10.csv")$x
residual_our = read.csv('output/residual_our1_500_250_0.2_10_10_.csv')
residual_baseline = read.csv('output/residual_baseline1_500_250_0.2_10_10_.csv')

# Plot 1: Nonconvex proposed method
residual_our_nonconvex = residual_our$residual_our
residual_baseline_nonconvex = residual_baseline$residual_baseline1
sigma_hat = sum(residual_our_nonconvex[80:160])/sum(random_noise[80:160])
data_used = data.frame('x1'=residual_our_nonconvex,'x2'=random_noise*sigma_hat)

residual_1 <- ggplot(data_used,aes(x=x1))+
  theme(panel.grid.minor = element_blank()) +
  scale_x_continuous(name="Singular value",limits = c(-1,16),breaks = seq(-1,16,1))+
  scale_y_continuous(name="Density",limits = c(0,0.125))+
  geom_histogram(aes(y=..density..),
                 color="#88ada6", fill="#fffbf0",
                 alpha=1,
                 binwidth = 0.35,
                 center = 0) +
  geom_density(aes(x=x2),color='steelblue',linetype='dashed',size=1.2,adjust = 0.5)+
  labs(title="Residual matrix's singular values of Estimator 1")+
  theme_bw() +
  theme(axis.title.x=element_text(size=28),
        axis.title.y=element_text(size=28),
        axis.text.x=element_text(size=24),
        axis.text.y=element_text(size=24),
        legend.title =element_text(size=28),
        legend.text = element_text(size=28),
        plot.title = element_text(size=28,hjust=0.5))

ggsave("plot/histo_residual_nonconvex.png",plot=residual_1,width=12,height=8)

# Plot 2: Nonconvex baseline method
sigma_hat = sum(residual_baseline_nonconvex[80:160])/sum(random_noise[80:160])
data_used2 = data.frame('x1'=residual_baseline_nonconvex,'x2'=random_noise*sigma_hat)

residual_2 <- ggplot(data_used2,aes(x=x1))+
  scale_x_continuous(name="Singular value",limits = c(-1,16),breaks = seq(-1,16,1))+
  scale_y_continuous(name="Density",limits = c(0,0.125))+
  geom_histogram(aes(y=..density..),
                 color="#88ada6",
                 fill = "#fffbf0",
                 alpha=1,
                 binwidth = 0.35,
                 center = 0) +
  geom_density(aes(x=x2),color='steelblue',linetype='dashed',size=1.2,adjust = 0.5)+
  labs(title="Residual matrix's singular values of Baseline 1")+
  theme_bw() +
  theme(axis.title.x=element_text(size=28),
        axis.title.y=element_text(size=28),
        axis.text.x=element_text(size=24),
        axis.text.y=element_text(size=24),
        legend.title =element_text(size=28),
        legend.text = element_text(size=28),
        plot.title = element_text(size=28,hjust=0.5))

ggsave("plot/histo_residual_nonconvex_baseline.png",plot=residual_2,width=12,height=8)

# Plot 3: Convex proposed method
residual_our_convex = residual_our$residual_our_convex
residual_baseline_convex = residual_baseline$residual_baseline1_convex
sigma_hat = sum(residual_our_convex[80:160])/sum(random_noise[80:160])
data_used3 = data.frame('x1'=residual_our_convex, 'x2'=random_noise*sigma_hat)

residual_3 <- ggplot(data_used3,aes(x=x1))+
  theme(panel.grid.minor = element_blank()) +
  scale_x_continuous(name="Singular value",limits = c(-1,16),breaks = seq(-1,16,1))+
  scale_y_continuous(name="Density",limits = c(0,0.125))+
  geom_histogram(aes(y=..density..),
                 color="#88ada6", fill="#fffbf0",
                 alpha=1,
                 binwidth = 0.35,
                 center = 0) +
  geom_density(aes(x=x2),color='steelblue',linetype='dashed',size=1.2,adjust = 0.5)+
  labs(title="Residual matrix's singular values of Estimator 2")+
  theme_bw() +
  theme(axis.title.x=element_text(size=28),
        axis.title.y=element_text(size=28),
        axis.text.x=element_text(size=24),
        axis.text.y=element_text(size=24),
        legend.title =element_text(size=28),
        legend.text = element_text(size=28),
        plot.title = element_text(size=28,hjust=0.5))

ggsave("plot/histo_residual_convex.png",plot=residual_3,width=12,height=8)

# Plot 4: Convex baseline method
sigma_hat = sum(residual_baseline_convex[80:160])/sum(random_noise[80:160])
data_used4 = data.frame('x1'=residual_baseline_convex,'x2'=random_noise*sigma_hat)

residual_4 <- ggplot(data_used4,aes(x=x1))+
  theme(panel.grid.minor = element_blank()) +
  scale_x_continuous(name="Singular value",limits = c(-1,16),breaks = seq(-1,16,1))+
  scale_y_continuous(name="Density",limits = c(0,0.125))+
  geom_histogram(aes(y=..density..),
                 color="#88ada6", fill="#fffbf0",
                 alpha=1,
                 binwidth = 0.35,
                 center = 0) +
  geom_density(aes(x=x2),color='steelblue',linetype='dashed',size=1.2,adjust = 0.5)+
  labs(title="Residual matrix's singular values of Baseline 2")+
  theme_bw() +
  theme(axis.title.x=element_text(size=28),
        axis.title.y=element_text(size=28),
        axis.text.x=element_text(size=24),
        axis.text.y=element_text(size=24),
        legend.title =element_text(size=28),
        legend.text = element_text(size=28),
        plot.title = element_text(size=28,hjust=0.5))

ggsave("plot/histo_residual_convex_baseline.png",plot=residual_4,width=12,height=8)
