###############
# CHARR MODEL #
###############
# Lizy
# Latest re-run: 7th July 2021

library(rjags)

# Get data
data <- read.csv("model_data.csv")
data <- data[,-1]
data$k <- as.numeric(data$k)
data$fl <- as.numeric(data$fl)
data$date <- as.Date(data$date)
data$av_mid_dat <- as.Date(data$av_mid_dat)
data$days <- as.integer(data$days)
data$day_index <- as.integer(data$day_index)
data$cave_season <- as.character(data$cave_season)
data$cs_index <- as.character(data$cs_index)
data$cave_k_season <- as.character(data$cave_k_season)
data$cave_k <- as.character(data$cave_k)
#str(data)
# 9262 measurements


## Vector with first capture occassion (k) for each fish (ordered on id)
first_cap <- NULL
for(i in levels(as.factor(data$id))){
	a <- data[which(data$id %in% i),]
	a <- as.data.frame(a[order(a$date),])
	b <- a$date[1]
	c <- a$fl[1]
	row.names(a) <- NULL
	a$first_date <- b
	a$first_fl <- c
	a$cap_num <- as.factor(rownames(a))
	first_cap <- rbind(first_cap,a)
}
first <- first_cap[c("id","k","cap_num")]
caps <- first
first <- first[which(first$cap_num == "1"),]
first <- first$k

# Index cave based on id
data$cave_ori <- data$cave
data$cave <- as.integer(data$cave)
rndid <- with(data, ave(id, id, FUN=function(x){sample.int(length(x))}))
cave <- data[rndid<=1,]
cave <- cave[order(cave$id),]
cave <- cave[,-c(2:5,7:21)]
rownames(cave) <- NULL
cave <- cave[,-1]

# Index year based on k
data$year <- as.integer(as.factor(data$year))
# Get a random selection of rows with each k
rndid <- with(data, ave(k, k, FUN=function(x){sample.int(length(x))}))
year <- data[rndid<=1,]
year <- year[order(year$k),]
year <- year[,-c(1:2,4,6:21)]
rownames(year) <- NULL
year <- year[,-1]

## Include summer/winter
# Assume that june (even k's) is winter growth because it is growth to this from the previous k
# august (odd k's) is summer growth
k_season <- sort(unique(data$k))
is.even <- function(x) x %% 2 == 0
season <- k_season
season <- as.data.frame(cbind(k_season,season))
for(i in 1:15){
	if(is.even(season$k_season[i])){
		season$season[i] <- "winter"
		}
	else{
		season$season[i] <- "summer"
	}
}
num_season <- as.integer(as.factor(season$season))

cave_k_mat <- matrix(1:300, nrow = 20, ncol = 15) # Gives an integer value for each cave at each capture event

n <- length(unique(data$id)) # 3804 individual fish
k <- length(unique(data$k)) # 15 capture occassions
y <- length(unique(data$year)) # 8 years
d <- length(unique(data$day_index)) # 5 max caught within one k
c <- length(unique(data$cave)) # 20 caves

# Tracing 10 individual growth trajectories
# Get multicapture data
multi <- as.data.frame(table(data$id))
multi <- multi[multi[,2]>1,]
nm <- multi[,1]
# Randomly select 10 individuals to remove a measurement from their data and trace their growth trajectories
r <- sample(nm, 10, replace=F)
# Get these individuals out of the data
gro_ind <- data[which(data$id %in% r),]
gro_ind_id <- as.factor(unique(gro_ind[,1]))
fish <- NULL
for(i in levels(gro_ind_id)){
	fish[i] <- gro_ind[which(gro_ind$id %in% i),15]
}
gro_ind_first <- fish
names(gro_ind_first) <- NULL
#write.csv(fish, file=paste("fish_traced_07_07_21.csv",sep=""))

gro_ind_id <- as.character(gro_ind_id)

##### MODEL -- TEMPERATURE USING MONTHLY AVERAGES ######
temp_model <-"var size[n,k], growth[n,k];
model{
# growth model
for(i in 1:n){
	size[i,(first[i])] ~ dnorm(0,0.000001)
	for(j in (first[i]+1):k){
		size[i,j] ~ dnorm(size[i,j-1]
		+ mu[season[j]] + rand_gr_coefs_t[j,1] + rand_gr_coefs_s[cave[i],season[j],1] + rand_gr_coefs_t_s[cave_k[cave[i],j],1] + beta_temp[season[j]]*(temp[cave[i],j]-mu_temp[season[j]])
		+ (beta[season[j]] + rand_gr_coefs_t[j,2] + rand_gr_coefs_s[cave[i],season[j],2] + rand_gr_coefs_t_s[cave_k[cave[i],j],2])*(size[i,j-1]-92)
		+ beta_temp_size[season[j]]*(temp[cave[i],j]-mu_temp[season[j]])*(size[i,j-1]-92)
		,tau[j])
	}
}

# observation model fork length
for(i in 1:n.obs){
	fl[i] ~ dnorm(size[id[i],occasion[i]] + beta_g*days[i],meas)
}

# trace 10 individuals
for(i in 1:10){
	for(j in inds_first[i]:15){
		lengths[i,j] <- size[inds[i],j]
	}
	lengths[i,16] <- inds[i]
}

# state model temperature
for(i in 1:c){
	for(j in 1:k){
		temp[i,j] ~ dnorm(mu_temp[season[j]] + b_temp_cave[cave[i],season[j]] + b_temp_year[year[j],season[j]], tau_temp[season[j]])
	}
}

# observation model temperature
for(i in 1:n.temps){
	temp_obs[i] ~ dnorm(temp[cave_obs[i],k_obs[i]], tau_temp_res[season_obs[i]])
}

# temperature related priors
mu_temp[1] ~ dnorm(0,0.0001)
mu_temp[2] ~ dnorm(0,0.0001)
beta_temp[1] ~ dnorm(0,0.0001)
beta_temp[2] ~ dnorm(0,0.0001)
beta_temp_size[1] ~ dnorm(0,0.0001)
beta_temp_size[2] ~ dnorm(0,0.0001)

for(i in 1:c){
	b_temp_cave[i,1] ~ dnorm(0,tau_temp_cave[1])
	b_temp_cave[i,2] ~ dnorm(0,tau_temp_cave[2])
}

for(j in 1:y){
	b_temp_year[j,1] ~ dnorm(0,tau_temp_year[1])
	b_temp_year[j,2] ~ dnorm(0,tau_temp_year[2])	
}

tau_temp_cave[1] ~ dgamma(0.001,0.001)
tau_temp_cave[2] ~ dgamma(0.001,0.001)
sd_temp_cave[1] <- sqrt(1/tau_temp_cave[1])
sd_temp_cave[2] <- sqrt(1/tau_temp_cave[2])

tau_temp_year[1] ~ dgamma(0.001,0.001)
tau_temp_year[2] ~ dgamma(0.001,0.001)
sd_temp_year[1] <- sqrt(1/tau_temp_year[1])
sd_temp_year[2] <- sqrt(1/tau_temp_year[2])

tau_temp[1] ~ dgamma(0.001,0.001)
tau_temp[2] ~ dgamma(0.001,0.001)
sd_temp[1] <- sqrt(1/tau_temp[1])
sd_temp[2] <- sqrt(1/tau_temp[2])

tau_temp_res[1] ~ dunif(1000,1001) #~ dgamma(0.001,0.001) # want these to be high precision priors~  dt(0,pow(25,-2),1)
tau_temp_res[2] ~ dunif(1000,1001) #~ dgamma(0.001,0.001) # dt(0,pow(25,-2),1) # ~ dgamma(0.001,0.001)
sd_temp_res[1] <- sqrt(1/tau_temp_res[1])
sd_temp_res[2] <- sqrt(1/tau_temp_res[2])

# growth priors
for(j in 2:k){ 
	tau[j] ~ dgamma(0.001,0.001)
	sd[j] <- sqrt(1/tau[j])
	rand_gr_coefs_t[j,1:2] ~ dmnorm(zeros[1:2],tau_t[season[j],1:2,1:2])
	}
for(j in 1:20){
	rand_gr_coefs_s[j,1,1:2] ~ dmnorm(zeros[1:2],tau_s[1,1:2,1:2])
	rand_gr_coefs_s[j,2,1:2] ~ dmnorm(zeros[1:2],tau_s[2,1:2,1:2])
}
for(i in 1:20){
	for(j in 2:k){
#		rand_gr_coefs_t_s[cave_k[i,j],season[j],1:2] ~ dmnorm(zeros[1:2],tau_ts[season[j],1:2,1:2])
		rand_gr_coefs_t_s[cave_k[i,j],1:2] ~ dmnorm(zeros[1:2],tau_ts[season[j],1:2,1:2])
	}
}
for(i in 1:n.obs){
	days[i] ~ dnorm(0,0.0001)
}
beta[1] ~ dnorm(0,0.0001)
beta[2] ~ dnorm(0,0.0001)
beta_g ~ dnorm(0,0.0001)
mu[1] ~ dnorm(0,0.0001)
mu[2] ~ dnorm(0,0.0001)

tau_a_t_sum ~ dgamma(0.001,0.001)
tau_b_t_sum ~ dgamma(0.001,0.001)
cor_ab_t_sum ~ dunif(-1,1)

sd_a_t_sum <- sqrt(1/tau_a_t_sum)
sd_b_t_sum <- sqrt(1/tau_b_t_sum)

cov_t_sum[1,1] <- 1/tau_a_t_sum
cov_t_sum[2,2] <- 1/tau_b_t_sum
cov_t_sum[1,2] <- sd_a_t_sum*sd_b_t_sum*cor_ab_t_sum
cov_t_sum[2,1] <- sd_a_t_sum*sd_b_t_sum*cor_ab_t_sum
tau_t[1,1:2,1:2] <- inverse(cov_t_sum[1:2,1:2])
	
tau_a_t_win ~ dgamma(0.001,0.001)
tau_b_t_win ~ dgamma(0.001,0.001)
cor_ab_t_win ~ dunif(-1,1)
	
sd_a_t_win <- sqrt(1/tau_a_t_win)
sd_b_t_win <- sqrt(1/tau_b_t_win)

cov_t_win[1,1] <- 1/tau_a_t_win
cov_t_win[2,2] <- 1/tau_b_t_win
cov_t_win[1,2] <- sd_a_t_win*sd_b_t_win*cor_ab_t_win
cov_t_win[2,1] <- sd_a_t_win*sd_b_t_win*cor_ab_t_win
tau_t[2,1:2,1:2] <- inverse(cov_t_win[1:2,1:2])
	
tau_a_s_sum ~ dgamma(0.001,0.001)
tau_b_s_sum ~ dgamma(0.001,0.001)
cor_ab_s_sum ~ dunif(-1,1)
	
sd_a_s_sum <- sqrt(1/tau_a_s_sum)
sd_b_s_sum <- sqrt(1/tau_b_s_sum)

cov_s_sum[1,1] <- 1/tau_a_s_sum
cov_s_sum[2,2] <- 1/tau_b_s_sum
cov_s_sum[1,2] <- sd_a_s_sum*sd_b_s_sum*cor_ab_s_sum
cov_s_sum[2,1] <- sd_a_s_sum*sd_b_s_sum*cor_ab_s_sum
tau_s[1,1:2,1:2] <- inverse(cov_s_sum[1:2,1:2])
	
tau_a_s_win ~ dgamma(0.001,0.001)
tau_b_s_win ~ dgamma(0.001,0.001)
cor_ab_s_win ~ dunif(-1,1)
	
sd_a_s_win <- sqrt(1/tau_a_s_win)
sd_b_s_win <- sqrt(1/tau_b_s_win)

cov_s_win[1,1] <- 1/tau_a_s_win
cov_s_win[2,2] <- 1/tau_b_s_win
cov_s_win[1,2] <- sd_a_s_win*sd_b_s_win*cor_ab_s_win
cov_s_win[2,1] <- sd_a_s_win*sd_b_s_win*cor_ab_s_win
tau_s[2,1:2,1:2] <- inverse(cov_s_win[1:2,1:2])
	
tau_a_ts_sum ~ dgamma(0.001,0.001)
tau_b_ts_sum ~ dgamma(0.001,0.001)
cor_ab_ts_sum ~ dunif(-1,1)
	
sd_a_ts_sum <- sqrt(1/tau_a_ts_sum)
sd_b_ts_sum <- sqrt(1/tau_b_ts_sum)

cov_ts_sum[1,1] <- 1/tau_a_ts_sum
cov_ts_sum[2,2] <- 1/tau_b_ts_sum
cov_ts_sum[1,2] <- sd_a_ts_sum*sd_b_ts_sum*cor_ab_ts_sum
cov_ts_sum[2,1] <- sd_a_ts_sum*sd_b_ts_sum*cor_ab_ts_sum
tau_ts[1,1:2,1:2] <- inverse(cov_ts_sum[1:2,1:2])
	
tau_a_ts_win ~ dgamma(0.001,0.001)
tau_b_ts_win ~ dgamma(0.001,0.001)
cor_ab_ts_win ~ dunif(-1,1)
	
sd_a_ts_win <- sqrt(1/tau_a_ts_win)
sd_b_ts_win <- sqrt(1/tau_b_ts_win)

cov_ts_win[1,1] <- 1/tau_a_ts_win
cov_ts_win[2,2] <- 1/tau_b_ts_win
cov_ts_win[1,2] <- sd_a_ts_win*sd_b_ts_win*cor_ab_ts_win
cov_ts_win[2,1] <- sd_a_ts_win*sd_b_ts_win*cor_ab_ts_win
tau_ts[2,1:2,1:2] <- inverse(cov_ts_win[1:2,1:2])

meas <- 0.61
}"
writeLines(temp_model,"./temp_model.jags")

data_fish_temp <- list(
n = n,
c = c,
k = k,
y = y,
year = year,
n.temps = dim(data)[1],
days = data$days,
cave = cave,
season = num_season,
first = first,
fl = data$fl,
id = data$id,
occasion = data$k,
n.obs = dim(data)[1],
zeros=c(0,0),
cave_k = cave_k_mat,
cave_obs = data$cave,
season_obs = data$s2,
k_obs = data$k,
temp_obs = data$temp,
inds = gro_ind_id,
inds_first = gro_ind_first
)

temp_model <- jags.model(file="./temp_model.jags", data=data_fish_temp, n.chains=4, n.adapt=10000)

results <- jags.samples(model=temp_model, variable.names=c("sd","mu","mu_temp","beta","beta_temp","beta_temp_size","b_temp_cave","b_temp_year","sd_temp","sd_temp_cave","sd_temp_year","sd_temp_res","sd_a_t_sum","sd_b_t_sum","sd_a_t_win","sd_b_t_win","cor_ab_t_sum","cor_ab_t_win","sd_a_s_sum","sd_b_s_sum","sd_a_s_win","sd_b_s_win","cor_ab_s_sum","cor_ab_s_win","sd_a_ts_sum","sd_b_ts_sum","sd_a_ts_win","sd_b_ts_win","cor_ab_ts_sum","cor_ab_ts_win","rand_gr_coefs_t","rand_gr_coefs_s","rand_gr_coefs_t_s","beta_g","temp","lengths"), n.iter=100000, thin=10)

#save(results,file="charr_results.rda")

# Check the model convergence
f <-function(x){
	hist(x)
	plot(x[,1],type='l',col=1)
	if(dim(x)[2]>1){
		for(c in 2:dim(x)[2]){
			lines(x[,c],col=c)
    }
  }
}

par(mfrow=c(4,2))
f(results$mu[1,,])
f(results$mu[2,,])
f(results$beta[1,,])
f(results$beta[2,,])

par(mfrow=c(4,2))
f(results$mu_temp[1,,])
f(results$mu_temp[2,,])
f(results$beta_temp[1,,])
f(results$beta_temp[2,,])

par(mfrow=c(6,2),mar=c(1,1,1,1))
f(results$sd_a_s_sum[1,,])
f(results$sd_a_s_win[1,,])
f(results$sd_b_s_sum[1,,])
f(results$sd_b_s_win[1,,])
f(results$cor_ab_s_sum[1,,])
f(results$cor_ab_s_win[1,,])

f(results$sd_a_t_sum[1,,])
f(results$sd_a_t_win[1,,])
f(results$sd_b_t_sum[1,,])
f(results$sd_b_t_win[1,,])
f(results$cor_ab_t_sum[1,,])
f(results$cor_ab_t_win[1,,])

par(mfrow=c(6,2),mar=c(1,1,1,1))
f(results$sd_a_ts_sum[1,,])
f(results$sd_a_ts_win[1,,])
f(results$sd_b_ts_sum[1,,])
f(results$sd_b_ts_win[1,,])
f(results$cor_ab_ts_sum[1,,])
f(results$cor_ab_ts_win[1,,])

par(mfrow=c(4,2),mar=c(1,1,1,1))
f(results$sd_temp[1,,])
f(results$sd_temp[2,,])
f(results$sd_temp_res[1,,])
f(results$sd_temp_res[2,,])

f(results$sd_temp_cave[1,,])
f(results$sd_temp_cave[2,,])
f(results$sd_temp_year[1,,])
f(results$sd_temp_year[2,,])
