# Charr growth model validation analyses
# Lizy
# June 2021

library(rjags)

# Get the full observed data
data <- read.csv("model_data.csv")
data <- data[,-1]
data$fl <- as.numeric(data$fl)

# make each k it's mid-date
data$date <- as.Date(data$date)
data$av_mid_dat <- as.Date(data$av_mid_dat)
k_date <- data[,c("k","av_mid_dat")]
k_date <- k_date[!duplicated(k_date)==TRUE,]
k_date <- k_date[order(k_date$k),]

month <- rep(c("August","June"),8)[1:15]
year <- c(2012,rep(2013:2019,each=2))

# Get the predicted data and observations randomly removed
all <- NULL
for(z in 1:100){
	load(paste("charr_results_tests_",z,".rda",sep=""))
	deleted <- read.csv(paste("deleted_",z,".csv",sep=""))
	
	pred <- matrix(NA,10,15)
	pred_ci_low <- matrix(NA,10,15)
	pred_ci_high <- matrix(NA,10,15)
	obs_slim <- matrix(NA,10,15)
	both_slim <- NULL
	for(i in 1:10){
		for(j in 1:15){
			a <- mean(results$lengths[i,j,,],na.rm=TRUE)
			b <- quantile(results$lengths[i,j,,],0.025,na.rm=TRUE)
			c <- quantile(results$lengths[i,j,,],0.975,na.rm=TRUE)
			pred[i,j] <- a
			pred_ci_low[i,j] <- b
			pred_ci_high[i,j] <- c
			id <- results$lengths[i,16,,][1]
		
			e <- deleted[deleted$id%in%id,]
			f <- e[e$k==j,]
			g <- f$fl
			if(length(g)==0) obs_slim[i,j] <- NA 
			else obs_slim[i,j] <- g
		}
		k <- rep(1:15)
		h <- cbind(pred[i,],pred_ci_low[i,],pred_ci_high[i,],obs_slim[i,],id,k)
		both_slim <- rbind(both_slim,h)
	}
	all <- rbind(all,both_slim)
}
inc_pred <- all
all <- all[!is.na(all[,4]),]

all <- as.data.frame(all)
names(all)[1] <- "predicted"
names(all)[2] <- "pred_ci_low"
names(all)[3] <- "pred_ci_high"
names(all)[4] <- "observed"
all$residuals <- all$observed - all$predicted

# index for later
all$ind <- paste(all$id,all$observed,all$k,sep="_")
data$ind <- paste(data$id,data$fl,data$k,sep="_")

# Correlations
cor(all$predicted,all$observed)
cor(all$residuals,all$predicted)

# Plot the observed values against the predicted values
par(mfrow=c(1,1))
plot(all[,4],jitter(all[,1]),xlab="Observed size (mm)",ylab="Predicted size (mm)",pch=19,cex=0.4,yaxt="n",xaxt="n",ylim=c(min(all$pred_ci_low),max(all$pred_ci_high)))
axis(side=2,at=c(20,40,60,80,100,120,140,160,180,200),las=1)
axis(side=1,at=c(40,60,80,100,120,140,160,180,200),las=1)
arrows(all[,4], all[,2], all[,4], all[,3], length=0.05, angle=90, code=3, col="grey")
points(all[,4],jitter(all[,1]),pch=19,cex=0.4)
abline(lm(all[,1]~all[,4]),lty=2,col="blue")
abline(0,1,lty=3,col="orange")

# Residuals against predicted and observed
par(mfrow=c(2,1),oma=c(0.2,0.2,0.2,0.2),mar=c(4,5,2,1))
plot(all$predicted~jitter(all$residuals),ylab="Predicted size (mm)",xlab="Residuals",pch=19,cex=0.5,yaxt="n")
axis(side=2,at=c(0,20,40,60,80,100,120,140,160,180),las=1)
points(all$predicted[abs(all$residuals)>10]~jitter(all$residuals[abs(all$residuals)>10]),pch=19,cex=0.5, col="red")
text(-25,180,"(a)")
plot(all$observed~jitter(all$residuals),ylab="Observed size (mm)",xlab="Residuals",pch=19,cex=0.5,las=1,yaxt="n")
axis(side=2,at=c(50,70,90,110,130,150,170,190,210),las=1)
points(all$observed[abs(all$residuals)>10]~jitter(all$residuals[abs(all$residuals)>10]),pch=19,cex=0.5, col="red")
text(-25,200,"(b)")

# What proportion of observations fall within the 95%CIs for the predicted values?
high <- all[which(all$observed > all$pred_ci_high),] 
dim(high)[1] 
(dim(high)[1]/dim(all)[1])*100 
low <- all[which(all$observed < all$pred_ci_low),] 
dim(low)[1]
(dim(low)[1]/dim(all)[1])*100

outliers_cis <- rbind(high,low)

# Mean absolute residual
mean(abs(all$residuals))

# Mean squared error
sqrt((sum(all$residuals^2))/dim(all)[1])

# Full dataset, get the other information on each individual
data2 <- data[which(data$id %in% all$id),]
info <- NULL
for(i in 1:length(all[,1])){
	a <- all$ind[i] # index
	b <- data2[which(data2$ind %in% a),] # observation missing
	c <- data2[which(data2$id %in% b$id),]		
	g <- min(c$k) # first k
	o <- max(c$k) # last observed k prior to deletion
	if(dim(c)[1]==dim(b)[1]) c <- c[2,] else c <- c[which(!(c$ind %in% b$ind)),] # other data on individual
	if(dim(b)[1]>1) b <- b[1,] 
	d <- length(c[,1]) # number of observations seen not deleted
	if(d>1) p <- max(c$k) else p <- c$k # last observed k after deletion
	if(d>1) e <- min(abs(c$k - b$k)) else e <- abs(c$k - b$k) # nearest k regardless of direction
	if (d>1) f <- max(abs(c$k - b$k)) else f <- abs(c$k - b$k) # furthest k regardless of direction
	h <- NULL
	for(z in 1:length(c[,1])){
		h[z] <- b$k - c$k[z]
	}
	m <- min(h[which(h>0)]) # number of k since last observation
	j <- min(h[which(h<0)]) # number of k to next observation
	l <- c(b$id,g,o,p,b$k,a,d,e,f,m,j) # individual, first capture occasion, k that was deleted, index of observation, number obs seen by model, minimum distance to k, maximum distance to k, number of k since previous observation, number of k to next observation
	info <- rbind(info,l)
}
info <- as.data.frame(info)
names(info)[1] <- "id"
names(info)[2] <- "first_k"
names(info)[3] <- "last_k_inc_del"
names(info)[4] <- "last_k_wo_del"
names(info)[5] <- "del_k"
names(info)[6] <- "ind"
names(info)[7] <- "obs_in_mod"
names(info)[8] <- "min_dist_k"
names(info)[9] <- "max_dist_k"
names(info)[10] <- "dist_prev_k"
names(info)[11] <- "dist_next_k"
rownames(info) <- NULL
info$dist_prev_k[info$dist_prev_k==Inf] <- 0
info$dist_next_k[info$dist_next_k==Inf] <- 0
info
table(info$dist_prev_k)

# Using outliers from cis
info_out_cis <- info[which(info$ind %in% outliers_cis$ind),]
table(info_out_cis$dist_prev_k)
(dim(info_out_cis[which(info_out_cis$del_k == info_out_cis$first_k),])[1]/dim(info_out_cis)[1])*100

dim(info_out_cis[which(info_out_cis$del_k %in% info_out_cis$del_k),])
dim(info_out_cis)
table(info_out_cis$first_k,info_out_cis$del_k)

# Link the information on k to the predicted values
all$dist_prev_k <- info$dist_prev_k[match(info$ind,all$ind)]
all$dist_next_k <- info$dist_next_k[match(info$ind,all$ind)]
all$nu_obs_mod <- info$obs_in_mod[match(info$ind,all$ind)]
all$min_dist_k <- info$min_dist_k[match(info$ind,all$ind)]
all$max_dist_k <- info$max_dist_k[match(info$ind,all$ind)]

### Plot number of k against the residuals
par(mfrow=c(2,2),oma=c(0.2,0.2,0.2,0.2),mar=c(4,5,2,1))
plot(all$dist_prev_k,all$residuals,ylab="Residual",xlab="Number of k to previous k",pch=19,cex=0.4,las=1)
abline(h=0)
text(0.2,20,"(a)")
plot(all$dist_next_k,all$residuals,ylab="Residual",xlab="Number of k to next k",pch=19,cex=0.4,las=1)
abline(h=0)
text(-11,20,"(b)")
plot(all$min_dist_k,all$residuals,ylab="Residual",xlab="Minimum number of k to nearest observation",pch=19,cex=0.4,las=1)
abline(h=0)
text(0,20,"(c)")
plot(all$max_dist_k,all$residuals,ylab="Residual",xlab="Maximum number of k to nearest observation",pch=19,cex=0.4,las=1)
abline(h=0)
text(0,20,"(d)")

### Take just those that are not k=0 measurements (within the same sampling occasion) and get results again
all2 <- all
all <- all[which(all$min_dist_k!=0),]
all <- all[which(all$dist_prev_k!=0),]
all <- all[which(all$dist_next_k!=0),]

par(mfrow=c(2,2),oma=c(0.2,0.2,0.2,0.2),mar=c(4,5,2,1))
plot(all$dist_prev_k,all$residuals,ylab="Residual",xlab="Number of k to previous k",pch=19,cex=0.4,las=1)
abline(h=0)
text(0.2,20,"(a)")
plot(all$dist_next_k,all$residuals,ylab="Residual",xlab="Number of k to next k",pch=19,cex=0.4,las=1)
abline(h=0)
text(-11,20,"(b)")
plot(all$min_dist_k,all$residuals,ylab="Residual",xlab="Minimum number of k to nearest observation",pch=19,cex=0.4,las=1)
abline(h=0)
text(0,20,"(c)")
plot(all$max_dist_k,all$residuals,ylab="Residual",xlab="Maximum number of k to nearest observation",pch=19,cex=0.4,las=1)
abline(h=0)
text(0,20,"(d)")

# Correlations
cor(all$predicted,all$observed)
cor(all$residuals,all$predicted)

# Plot the observed values against the predicted values
par(mfrow=c(1,1))
plot(all[,4],jitter(all[,1]),xlab="Observed size (mm)",ylab="Predicted size (mm)",pch=19,cex=0.4,yaxt="n",xaxt="n",ylim=c(min(all$pred_ci_low),max(all$pred_ci_high)))#,ylim=c(15,200))#
axis(side=2,at=c(20,40,60,80,100,120,140,160,180,200),las=1)
axis(side=1,at=c(40,60,80,100,120,140,160,180,200),las=1)
arrows(all[,4], all[,2], all[,4], all[,3], length=0.05, angle=90, code=3, col="grey")
points(all[,4],jitter(all[,1]),pch=19,cex=0.4)
abline(lm(all[,1]~all[,4]),lty=2,col="blue")
abline(0,1,lty=3,col="orange")

# Residuals against predicted and observed
par(mfrow=c(2,1),oma=c(0.2,0.2,0.2,0.2),mar=c(4,5,2,1))
plot(all$predicted~jitter(all$residuals),ylab="Predicted size (mm)",xlab="Residuals",pch=19,cex=0.5,yaxt="n")
axis(side=2,at=c(0,20,40,60,80,100,120,140,160,180),las=1)
points(all$predicted[abs(all$residuals)>10]~jitter(all$residuals[abs(all$residuals)>10]),pch=19,cex=0.5, col="red")
text(-25,180,"(a)")
plot(all$observed~jitter(all$residuals),ylab="Observed size (mm)",xlab="Residuals",pch=19,cex=0.5,las=1,yaxt="n")
axis(side=2,at=c(50,70,90,110,130,150,170,190,210),las=1)
points(all$observed[abs(all$residuals)>10]~jitter(all$residuals[abs(all$residuals)>10]),pch=19,cex=0.5, col="red")
text(-25,200,"(b)")

# What proportion of observations fall within the 95%CIs for the predicted values?
dim(all)[1]
high <- all[which(all$observed > all$pred_ci_high),] # 
dim(high)[1] 
(dim(high)[1]/dim(all)[1])*100 # 2.81%
low <- all[which(all$observed < all$pred_ci_low),] # 
dim(low)[1]
(dim(low)[1]/dim(all)[1])*100 # 3.75%

outliers_cis <- rbind(high,low)

# Mean absolute residual
mean(abs(all$residuals))

# Mean squared error
sqrt((sum(all$residuals^2))/dim(all)[1])


