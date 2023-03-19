#### Charr asymptotic size thought experiment ####
### Lizy and Michael
## 10th March 2023

### Expectations
## Summer
exp_S_Linf <- round(mean(92 - (((mean(results$mu[1,,])/mean(results$beta[1,,]))-((mean(results$mu[1,,])*(((results$sd_b_s_sum)^2)+((results$sd_b_t_sum)^2)+((results$sd_b_ts_sum)^2))))/(mean(results$beta[1,,])^3))+ (((results$cor_ab_s_sum*results$sd_a_s_sum*results$sd_b_s_sum)+(results$cor_ab_t_sum*results$sd_a_t_sum*results$sd_b_t_sum)+(results$cor_ab_ts_sum*results$sd_a_ts_sum*results$sd_b_ts_sum))/(mean(results$beta[1,,])^2)))),digits=3)
  
## Winter
exp_W_Linf <- round(mean(92 - (((mean(results$mu[2,,])/mean(results$beta[2,,]))-((mean(results$mu[2,,])*(((results$sd_b_s_win)^2)+((results$sd_b_t_win)^2)+((results$sd_b_ts_win)^2))))/(mean(results$beta[1,,])^3))+ (((results$cor_ab_s_win*results$sd_a_s_win*results$sd_b_s_win)+(results$cor_ab_t_win*results$sd_a_t_win*results$sd_b_t_win)+(results$cor_ab_ts_win*results$sd_a_ts_win*results$sd_b_ts_win))/(mean(results$beta[2,,])^2)))),digits=3)
  
# Upper bound
## Summer
exp_S_LinfU <- round(92 - (HPDinterval(as.mcmc(results$mu[1,,]))[2]/HPDinterval(as.mcmc(results$beta[1,,]))[2])-(HPDinterval(as.mcmc(results$mu[1,,]))[2]*(HPDinterval(as.mcmc(results$sd_b_s_sum[,,1])^2)[2]+HPDinterval(as.mcmc(results$sd_b_t_sum[,,1])^2)[2]+HPDinterval(as.mcmc(results$sd_b_ts_sum[,,1])^2)[2]))/(HPDinterval(as.mcmc(results$beta[1,,])^3)[2])+ (HPDinterval(as.mcmc(results$cor_ab_s_sum[,,1]))[2]*HPDinterval(as.mcmc(results$sd_a_s_sum[,,1]))[2]*HPDinterval(as.mcmc(results$sd_b_s_sum[,,1]))[2]+HPDinterval(as.mcmc(results$cor_ab_t_sum[,,1]))[2]*HPDinterval(as.mcmc(results$sd_a_t_sum[,,1]))[2]*HPDinterval(as.mcmc(results$sd_b_t_sum[,,1]))[2]+HPDinterval(as.mcmc(results$cor_ab_ts_sum[,,1]))[2]*HPDinterval(as.mcmc(results$sd_a_ts_sum[,,1]))[2]*HPDinterval(as.mcmc(results$sd_b_ts_sum[,,1]))[2])/(HPDinterval(as.mcmc(results$beta[1,,])^2)[2]),digits=3)

## Winter
exp_W_LinfU <- round(92 - (HPDinterval(as.mcmc(results$mu[2,,]))[2]/HPDinterval(as.mcmc(results$beta[2,,]))[2])-(HPDinterval(as.mcmc(results$mu[1,,]))[2]*(HPDinterval(as.mcmc(results$sd_b_s_win[,,1])^2)[2]+HPDinterval(as.mcmc(results$sd_b_t_win[,,1])^2)[2]+HPDinterval(as.mcmc(results$sd_b_ts_win[,,1])^2)[2]))/(HPDinterval(as.mcmc(results$beta[2,,])^3)[2])+ (HPDinterval(as.mcmc(results$cor_ab_s_win[,,1]))[2]*HPDinterval(as.mcmc(results$sd_a_s_win[,,1]))[2]*HPDinterval(as.mcmc(results$sd_b_s_win[,,1]))[2]+HPDinterval(as.mcmc(results$cor_ab_t_win[,,1]))[2]*HPDinterval(as.mcmc(results$sd_a_t_win[,,1]))[2]*HPDinterval(as.mcmc(results$sd_b_t_win[,,1]))[2]+HPDinterval(as.mcmc(results$cor_ab_ts_win[,,1]))[2]*HPDinterval(as.mcmc(results$sd_a_ts_win[,,1]))[2]*HPDinterval(as.mcmc(results$sd_b_ts_win[,,1]))[2])/(HPDinterval(as.mcmc(results$beta[2,,])^2)[2]),digits=3)
  
# Lower bound
exp_S_LinfL <- round(92 - (HPDinterval(as.mcmc(results$mu[1,,]))[1]/HPDinterval(as.mcmc(results$beta[1,,]))[1])-(HPDinterval(as.mcmc(results$mu[1,,]))[1]*(HPDinterval(as.mcmc(results$sd_b_s_sum[,,1])^2)[1]+HPDinterval(as.mcmc(results$sd_b_t_sum[,,1])^2)[1]+HPDinterval(as.mcmc(results$sd_b_ts_sum[,,1])^2)[1]))/(HPDinterval(as.mcmc(results$beta[1,,])^3)[1])+ (HPDinterval(as.mcmc(results$cor_ab_s_sum[,,1]))[1]*HPDinterval(as.mcmc(results$sd_a_s_sum[,,1]))[1]*HPDinterval(as.mcmc(results$sd_b_s_sum[,,1]))[1]+HPDinterval(as.mcmc(results$cor_ab_t_sum[,,1]))[1]*HPDinterval(as.mcmc(results$sd_a_t_sum[,,1]))[1]*HPDinterval(as.mcmc(results$sd_b_t_sum[,,1]))[1]+HPDinterval(as.mcmc(results$cor_ab_ts_sum[,,1]))[1]*HPDinterval(as.mcmc(results$sd_a_ts_sum[,,1]))[1]*HPDinterval(as.mcmc(results$sd_b_ts_sum[,,1]))[1])/(HPDinterval(as.mcmc(results$beta[1,,])^2)[1]),digits=3)

## Winter
exp_W_LinfL <- round(92 - (HPDinterval(as.mcmc(results$mu[2,,]))[1]/HPDinterval(as.mcmc(results$beta[2,,]))[1])-(HPDinterval(as.mcmc(results$mu[1,,]))[1]*(HPDinterval(as.mcmc(results$sd_b_s_win[,,1])^2)[1]+HPDinterval(as.mcmc(results$sd_b_t_win[,,1])^2)[1]+HPDinterval(as.mcmc(results$sd_b_ts_win[,,1])^2)[1]))/(HPDinterval(as.mcmc(results$beta[2,,])^3)[1])+ (HPDinterval(as.mcmc(results$cor_ab_s_win[,,1]))[1]*HPDinterval(as.mcmc(results$sd_a_s_win[,,1]))[1]*HPDinterval(as.mcmc(results$sd_b_s_win[,,1]))[1]+HPDinterval(as.mcmc(results$cor_ab_t_win[,,1]))[1]*HPDinterval(as.mcmc(results$sd_a_t_win[,,1]))[1]*HPDinterval(as.mcmc(results$sd_b_t_win[,,1]))[1]+HPDinterval(as.mcmc(results$cor_ab_ts_win[,,1]))[1]*HPDinterval(as.mcmc(results$sd_a_ts_win[,,1]))[1]*HPDinterval(as.mcmc(results$sd_b_ts_win[,,1]))[1])/(HPDinterval(as.mcmc(results$beta[2,,])^2)[1]),digits=3)

### Variances -- change the correlations to covariances and make the second term +ve
## Summer
# Space
var_Ss_Linf <- format(round(mean(((mean(results$mu[1,,]^2)*((results$sd_b_s_sum)^2))/(mean(results$beta[1,,])^4)) + (((results$sd_a_s_sum)^2)/(mean(results$beta[1,,])^2)) - ((2*mean(results$mu[1,,])*(results$cor_ab_s_sum*results$sd_a_s_sum*results$sd_b_s_sum))/(mean(results$beta[1,,])^3))),digits=3),nsmall=3)
# Time
var_St_Linf <-  format(round(mean(((((results$mu[1,,1])^2)*((results$sd_b_t_sum)^2))/((results$beta[1,,1])^4)) + (((results$sd_a_t_sum)^2)/((results$beta[1,,1])^2)) - ((2*(results$mu[1,,1])*(results$cor_ab_t_sum*results$sd_a_t_sum*results$sd_b_t_sum))/((results$beta[1,,1])^3))),digits=3),nsmall=3)
# Space-Time
var_Sst_Linf <-  format(round(mean((((mean(results$mu[1,,])^2)*((results$sd_b_ts_sum)^2))/(mean(results$beta[1,,])^4)) + (((results$sd_a_ts_sum)^2)/(mean(results$beta[1,,])^2)) -((2*mean(results$mu[1,,])*(results$cor_ab_ts_sum*results$sd_a_ts_sum*results$sd_b_ts_sum))/(mean(results$beta[1,,])^3))),digits=3),nsmall=3)
## Winter
# Space
var_Ws_Linf <-  format(round(mean((((mean(results$mu[2,,])^2)*((results$sd_b_s_win)^2))/(mean(results$beta[2,,])^4)) + (((results$sd_a_s_win)^2)/(mean(results$beta[2,,])^2)) - ((2*mean(results$mu[2,,])*(results$cor_ab_s_win*results$sd_a_s_win*results$sd_b_s_win))/(mean(results$beta[2,,])^3))),digits=3),nsmall=3)
# Time
var_Wt_Linf <- format(round(mean((((mean(results$mu[2,,])^2)*((results$sd_b_t_win)^2))/(mean(results$beta[2,,])^4)) + (((results$sd_a_t_win)^2)/(mean(results$beta[2,,])^2)) - ((2*mean(results$mu[2,,])*(results$cor_ab_t_win*results$sd_a_t_win*results$sd_b_t_win))/(mean(results$beta[2,,])^3))),digits=3),nsmall=3)
# Space-Time
var_Wst_Linf <- format(round(mean((((mean(results$mu[2,,])^2)*((results$sd_b_ts_win)^2))/(mean(results$beta[2,,])^4)) + (((results$sd_a_ts_win)^2)/(mean(results$beta[2,,])^2)) - ((2*mean(results$mu[2,,])*(results$cor_ab_ts_win*results$sd_a_ts_win*results$sd_b_ts_win))/(mean(results$beta[2,,])^3))),digits=3),nsmall=3)
  
# Upper bound
## Summer
# Space
var_Ss_LinfU <- round((HPDinterval(as.mcmc(results$mu[1,,])^2)[2]*HPDinterval(as.mcmc(results$sd_b_s_sum[,,1])^2)[2]/HPDinterval(as.mcmc(results$beta[1,,])^4)[2])+(HPDinterval(as.mcmc(results$sd_b_s_sum[,,1])^2)[2]/HPDinterval(as.mcmc(results$beta[1,,])^2)[2])-(2*HPDinterval(as.mcmc(results$mu[1,,]))[2]*HPDinterval(as.mcmc(results$cor_ab_s_sum[,,1]*results$sd_a_s_sum[,,1]*results$sd_b_s_sum[,,1]))[2])/(HPDinterval(as.mcmc(results$beta[1,,])^3)[2]),digits=3)
# Time
var_St_LinfU <- round((HPDinterval(as.mcmc(results$mu[1,,])^2)[2]*HPDinterval(as.mcmc(results$sd_b_t_sum[,,1])^2)[2]/HPDinterval(as.mcmc(results$beta[1,,])^4)[2])+(HPDinterval(as.mcmc(results$sd_b_t_sum[,,1])^2)[2]/HPDinterval(as.mcmc(results$beta[1,,])^2)[2])-(2*HPDinterval(as.mcmc(results$mu[1,,]))[2]*HPDinterval(as.mcmc(results$cor_ab_t_sum[,,1]*results$sd_a_t_sum[,,1]*results$sd_b_t_sum[,,1]))[2])/(HPDinterval(as.mcmc(results$beta[1,,])^3)[2]),digits=3)
# Space-Time
var_Sst_LinfU <- round((HPDinterval(as.mcmc(results$mu[1,,])^2)[2]*HPDinterval(as.mcmc(results$sd_b_ts_sum[,,1])^2)[2]/HPDinterval(as.mcmc(results$beta[1,,])^4)[2])+(HPDinterval(as.mcmc(results$sd_b_ts_sum[,,1])^2)[2]/HPDinterval(as.mcmc(results$beta[1,,])^2)[2])-(2*HPDinterval(as.mcmc(results$mu[1,,]))[2]*HPDinterval(as.mcmc(results$cor_ab_ts_sum[,,1]*results$sd_a_ts_sum[,,1]*results$sd_b_ts_sum[,,1]))[2])/(HPDinterval(as.mcmc(results$beta[1,,])^3)[2]),digits=3)
## Winter
# Space
var_Ws_LinfU <- round((HPDinterval(as.mcmc(results$mu[2,,])^2)[2]*HPDinterval(as.mcmc(results$sd_b_s_win[,,1])^2)[2]/HPDinterval(as.mcmc(results$beta[2,,])^4)[2])+(HPDinterval(as.mcmc(results$sd_b_s_win[,,1])^2)[2]/HPDinterval(as.mcmc(results$beta[2,,])^2)[2])-(2*HPDinterval(as.mcmc(results$mu[2,,]))[2]*HPDinterval(as.mcmc(results$cor_ab_s_win[,,1]*results$sd_a_s_win[,,1]*results$sd_b_s_win[,,1]))[2])/(HPDinterval(as.mcmc(results$beta[2,,])^3)[2]),digits=3)
# Time
var_Wt_LinfU <- round((HPDinterval(as.mcmc(results$mu[2,,])^2)[2]*HPDinterval(as.mcmc(results$sd_b_t_win[,,1])^2)[2]/HPDinterval(as.mcmc(results$beta[2,,])^4)[2])+(HPDinterval(as.mcmc(results$sd_b_t_win[,,1])^2)[2]/HPDinterval(as.mcmc(results$beta[2,,])^2)[2])-(2*HPDinterval(as.mcmc(results$mu[2,,]))[2]*HPDinterval(as.mcmc(results$cor_ab_t_win[,,1]*results$sd_a_t_win[,,1]*results$sd_b_t_win[,,1]))[2])/(HPDinterval(as.mcmc(results$beta[2,,])^3)[2]),digits=3)
# Space-Time
var_Wst_LinfU <- round((HPDinterval(as.mcmc(results$mu[2,,])^2)[2]*HPDinterval(as.mcmc(results$sd_b_ts_win[,,1])^2)[2]/HPDinterval(as.mcmc(results$beta[2,,])^4)[2])+(HPDinterval(as.mcmc(results$sd_b_ts_win[,,1])^2)[2]/HPDinterval(as.mcmc(results$beta[2,,])^2)[2])-(2*HPDinterval(as.mcmc(results$mu[2,,]))[2]*HPDinterval(as.mcmc(results$cor_ab_ts_win[,,1]*results$sd_a_ts_win[,,1]*results$sd_b_ts_win[,,1]))[2])/(HPDinterval(as.mcmc(results$beta[2,,])^3)[2]),digits=3)
  
# Lower bound
## Summer
# Space
var_Ss_LinfL <- round((HPDinterval(as.mcmc(results$mu[1,,])^2)[1]*HPDinterval(as.mcmc(results$sd_b_s_sum[,,1])^2)[1]/HPDinterval(as.mcmc(results$beta[1,,])^4)[1])+(HPDinterval(as.mcmc(results$sd_b_s_sum[,,1])^2)[1]/HPDinterval(as.mcmc(results$beta[1,,])^2)[1])-(2*HPDinterval(as.mcmc(results$mu[1,,]))[1]*HPDinterval(as.mcmc(results$cor_ab_s_sum[,,1]*results$sd_a_s_sum[,,1]*results$sd_b_s_sum[,,1]))[1])/(HPDinterval(as.mcmc(results$beta[1,,])^3)[1]),digits=3)
# Time
var_St_LinfL <- round((HPDinterval(as.mcmc(results$mu[1,,])^2)[1]*HPDinterval(as.mcmc(results$sd_b_t_sum[,,1])^2)[1]/HPDinterval(as.mcmc(results$beta[1,,])^4)[1])+(HPDinterval(as.mcmc(results$sd_b_t_sum[,,1])^2)[1]/HPDinterval(as.mcmc(results$beta[1,,])^2)[1])-(2*HPDinterval(as.mcmc(results$mu[1,,]))[1]*HPDinterval(as.mcmc(results$cor_ab_t_sum[,,1]*results$sd_a_t_sum[,,1]*results$sd_b_t_sum[,,1]))[1])/(HPDinterval(as.mcmc(results$beta[1,,])^3)[1]),digits=3)
# Space-Time
var_Sst_LinfL <- round((HPDinterval(as.mcmc(results$mu[1,,])^2)[1]*HPDinterval(as.mcmc(results$sd_b_ts_sum[,,1])^2)[1]/HPDinterval(as.mcmc(results$beta[1,,])^4)[1])+(HPDinterval(as.mcmc(results$sd_b_ts_sum[,,1])^2)[1]/HPDinterval(as.mcmc(results$beta[1,,])^2)[1])-(2*HPDinterval(as.mcmc(results$mu[1,,]))[1]*HPDinterval(as.mcmc(results$cor_ab_ts_sum[,,1]*results$sd_a_ts_sum[,,1]*results$sd_b_ts_sum[,,1]))[1])/(HPDinterval(as.mcmc(results$beta[1,,])^3)[1]),digits=3)
## Winter
# Space
var_Ws_LinfL <- round((HPDinterval(as.mcmc(results$mu[2,,])^2)[1]*HPDinterval(as.mcmc(results$sd_b_s_win[,,1])^2)[1]/HPDinterval(as.mcmc(results$beta[2,,])^4)[1])+(HPDinterval(as.mcmc(results$sd_b_s_win[,,1])^2)[1]/HPDinterval(as.mcmc(results$beta[2,,])^2)[1])-(2*HPDinterval(as.mcmc(results$mu[2,,]))[1]*HPDinterval(as.mcmc(results$cor_ab_s_win[,,1]*results$sd_a_s_win[,,1]*results$sd_b_s_win[,,1]))[1])/(HPDinterval(as.mcmc(results$beta[2,,])^3)[1]),digits=3)
# Time
var_Wt_LinfL <- round((HPDinterval(as.mcmc(results$mu[2,,])^2)[1]*HPDinterval(as.mcmc(results$sd_b_t_win[,,1])^2)[1]/HPDinterval(as.mcmc(results$beta[2,,])^4)[1])+(HPDinterval(as.mcmc(results$sd_b_t_win[,,1])^2)[1]/HPDinterval(as.mcmc(results$beta[2,,])^2)[1])-(2*HPDinterval(as.mcmc(results$mu[2,,]))[1]*HPDinterval(as.mcmc(results$cor_ab_t_win[,,1]*results$sd_a_t_win[,,1]*results$sd_b_t_win[,,1]))[1])/(HPDinterval(as.mcmc(results$beta[2,,])^3)[1]),digits=3)
# Space-Time
var_Wst_LinfL <- round((HPDinterval(as.mcmc(results$mu[2,,])^2)[1]*HPDinterval(as.mcmc(results$sd_b_ts_win[,,1])^2)[1]/HPDinterval(as.mcmc(results$beta[2,,])^4)[1])+(HPDinterval(as.mcmc(results$sd_b_ts_win[,,1])^2)[1]/HPDinterval(as.mcmc(results$beta[2,,])^2)[1])-(2*HPDinterval(as.mcmc(results$mu[2,,]))[1]*HPDinterval(as.mcmc(results$cor_ab_ts_win[,,1]*results$sd_a_ts_win[,,1]*results$sd_b_ts_win[,,1]))[1])/(HPDinterval(as.mcmc(results$beta[2,,])^3)[1]),digits=3)

##### MC comparison
library(mvtnorm)
set.seed(1)
## Summer
# Overall values
alphaS <- mean(results$mu[1,,])
betaS <- mean(results$beta[1,,])

# Expectations
# Covariance matrix
var_alphaS <- mean(results$sd_a_s_sum^2)+mean(results$sd_a_t_sum^2)+mean(results$sd_a_ts_sum^2)
var_betaS <- mean(results$sd_b_s_sum^2)+ mean(results$sd_b_t_sum^2)+ mean(results$sd_b_ts_sum^2)
cov_alpha_betaS <- mean((results$cor_ab_s_sum+results$cor_ab_t_sum+results$cor_ab_ts_sum)*sqrt(var_alphaS)*sqrt(var_betaS))
sigma_timeS <- matrix(c(var_alphaS,cov_alpha_betaS,cov_alpha_betaS,var_betaS),2,2)
# Posterior samples of intercept and slope
mc_alpha_betaS <- rmvnorm(1000,c(alphaS,betaS),sigma_timeS)
# Posterior predictive samples of Linf and the variance
mc_LinfS <- 92-mc_alpha_betaS[,1]/mc_alpha_betaS[,2]
var_mc_LinfS <- var(mc_LinfS)
exp_mc_LinfS <- round(mean(mc_LinfS),digits=3)
exp_mc_LinfSU <- round(quantile(mc_LinfS,0.975),digits=3)
exp_mc_LinfSL <- round(quantile(mc_LinfS,0.025),digits=3)

### Variances
#### Space 
# Covariance matrix
var_alphaSs <- mean(results$sd_a_s_sum^2)
var_betaSs <- mean(results$sd_b_s_sum^2)
cov_alpha_betaSs <- mean(results$cor_ab_s_sum*sqrt(var_alphaSs)*sqrt(var_betaSs))
sigma_timeSs <- matrix(c(var_alphaSs,cov_alpha_betaSs,cov_alpha_betaSs,var_betaSs),2,2)
#cov2cor(sigma_timeSs)
# Posterior samples of intercept and slope
mc_alpha_betaSs <- rmvnorm(1000,c(alphaS,betaS),sigma_timeSs)
# Posterior predictive samples of Linf and the variance
mc_LinfSs <- 92-mc_alpha_betaSs[,1]/mc_alpha_betaSs[,2]
var_mc_LinfSs <- var(mc_LinfSs)
#### Time
# Covariance matrix
var_alphaSt <- mean(results$sd_a_t_sum^2)
var_betaSt <- mean(results$sd_b_t_sum^2)
cov_alpha_betaSt <- mean(results$cor_ab_t_sum*sqrt(var_alphaSt)*sqrt(var_betaSt))
sigma_timeSt <- matrix(c(var_alphaSt,cov_alpha_betaSt,cov_alpha_betaSt,var_betaSt),2,2)
#cov2cor(sigma_timeSt)
# Posterior samples of intercept and slope
mc_alpha_betaSt <- rmvnorm(1000,c(alphaS,betaS),sigma_timeSt)
# Posterior predictive samples of Linf and the variance
mc_LinfSt <- 92-mc_alpha_betaSt[,1]/mc_alpha_betaSt[,2]
var_mc_LinfSt <- var(mc_LinfSt)
#### Space - time
# Covariance matrix
var_alphaSts <- mean(results$sd_a_ts_sum^2)
var_betaSts <- mean(results$sd_b_ts_sum^2)
cov_alpha_betaSts <- mean(results$cor_ab_ts_sum*sqrt(var_alphaSts)*sqrt(var_betaSts))
sigma_timeSts <- matrix(c(var_alphaSts,cov_alpha_betaSts,cov_alpha_betaSts,var_betaSts),2,2)
#cov2cor(sigma_timeSts)
# Posterior samples of intercept and slope
mc_alpha_betaSts <- rmvnorm(1000,c(alphaS,betaS),sigma_timeSts)
# Posterior predictive samples of Linf and the variance
mc_LinfSts <- 92-mc_alpha_betaSts[,1]/mc_alpha_betaSts[,2]
var_mc_LinfSts <- var(mc_LinfSts)
exp_mc_LinfSts <- mean(mc_LinfSts)

## Winter
# Overall values
alphaW <- mean(results$mu[2,,])
betaW <- mean(results$beta[2,,])

# Expectations
# Covariance matrix
var_alphaW <- mean(results$sd_a_s_win^2)+mean(results$sd_a_t_win^2)+mean(results$sd_a_ts_win^2)
var_betaW <- mean(results$sd_b_s_win^2)+ mean(results$sd_b_t_win^2)+ mean(results$sd_b_ts_win^2)
cov_alpha_betaW <- mean((results$cor_ab_s_win+results$cor_ab_t_win+results$cor_ab_ts_win)*sqrt(var_alphaW)*sqrt(var_betaW))
sigma_timeW <- matrix(c(var_alphaW,cov_alpha_betaW,cov_alpha_betaW,var_betaW),2,2)
# Posterior samples of intercept and slope
mc_alpha_betaW <- rmvnorm(1000,c(alphaW,betaW),sigma_timeW)
# Posterior predictive samples of Linf and the variance
mc_LinfW <- 92-mc_alpha_betaW[,1]/mc_alpha_betaW[,2]
var_mc_LinfW <- var(mc_LinfW)
exp_mc_LinfW <- round(mean(mc_LinfW),digits=3)
exp_mc_LinfWU <- round(quantile(mc_LinfW,0.975),digits=3)
exp_mc_LinfWL <- round(quantile(mc_LinfW,0.025),digits=3)


#### Space 
# Covariance matrix
var_alphaWs <- mean(results$sd_a_s_win^2)
var_betaWs <- mean(results$sd_b_s_win^2)
cov_alpha_betaWs <- mean(results$cor_ab_s_win*sqrt(var_alphaWs)*sqrt(var_betaWs))
sigma_timeWs <- matrix(c(var_alphaWs,cov_alpha_betaWs,cov_alpha_betaWs,var_betaWs),2,2)
#cov2cor(sigma_timeWs)
# Posterior samples of intercept and slope
mc_alpha_betaWs <- rmvnorm(1000,c(alphaW,betaW),sigma_timeWs)
# Posterior predictive samples of Linf and the variance
mc_LinfWs <- 92-mc_alpha_betaWs[,1]/mc_alpha_betaWs[,2]
var_mc_LinfWs <- var(mc_LinfWs)
#### Time
# Covariance matrix
var_alphaWt <- mean(results$sd_a_t_win^2)
var_betaWt <- mean(results$sd_b_t_win^2)
cov_alpha_betaWt <- mean(results$cor_ab_t_win*sqrt(var_alphaWt)*sqrt(var_betaWt))
sigma_timeWt <- matrix(c(var_alphaWt,cov_alpha_betaWt,cov_alpha_betaWt,var_betaWt),2,2)
#cov2cor(sigma_timeSt)
# Posterior samples of intercept and slope
mc_alpha_betaWt <- rmvnorm(1000,c(alphaW,betaW),sigma_timeWt)
# Posterior predictive samples of Linf and the variance
mc_LinfWt <- 92-mc_alpha_betaWt[,1]/mc_alpha_betaWt[,2]
var_mc_LinfWt <- var(mc_LinfWt)
#### Space - time
# Covariance matrix
var_alphaWts <- mean(results$sd_a_ts_win^2)
var_betaWts <- mean(results$sd_b_ts_win^2)
cov_alpha_betaWts <- mean(results$cor_ab_ts_win*sqrt(var_alphaWts)*sqrt(var_betaWts))
sigma_timeWts <- matrix(c(var_alphaWts,cov_alpha_betaWts,cov_alpha_betaWts,var_betaWts),2,2)
#cov2cor(sigma_timeWts)
# Posterior samples of intercept and slope
mc_alpha_betaWts <- rmvnorm(1000,c(alphaW,betaW),sigma_timeWts)
# Posterior predictive samples of Linf and the variance
mc_LinfWts <- 92-mc_alpha_betaWts[,1]/mc_alpha_betaWts[,2]
var_mc_LinfWts <- var(mc_LinfWts)