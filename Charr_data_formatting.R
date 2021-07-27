#########################
# CHARR DATA FORMATTING #
#########################
# Lizy
# 12th March 2020

library(tidyr)

#############
# Fish data #
#############

# Get data
dat <- read.csv("charr_data.csv")
dat <- dat[,-1]

# Format date
dat$date <- as.Date(dat$date, "%Y-%m-%d")
# 9262 measurements; 3810 fish (recovered 147 fish in tag reassignments)
str(dat)
dat_1 <- dat

# Data check -- should be no obvious big drops
plot(dat$fl ~ dat$date)
for (i in 1:100){
	a <- sample(dat$id,1)
	lines(dat$fl[dat$id %in% a]~dat$date[dat$id %in% a], col = 2, type = "l")
}

# Number of individuals for each number of repeat observations
table(table(dat$id))
# Number or observations per cave
table(dat$cave)
table(dat$year) # per year
table(dat$season) # per season
table(dat$date) # per date

# Replace june with "a" and august with "b"
dat$season_2 <- dat$season
dat$season_2 <- gsub("june", "a", dat$season_2)
dat$season_2 <- gsub("august", "b", dat$season_2)
dat$sample_date <- paste(dat$year,"_",dat$season_2,"_",dat$visit, sep="")
length(unique(dat$sample_date))
# 30 visits overall
# Order data
dat <- dat[order(dat$sample_date),]

# Create k capture occasion (season/year)
dat$capture_month <- paste(dat$year,"_",dat$season_2, sep="")
dat$k <- as.integer(as.factor(dat$capture_month))
table(dat$k) # Number of observations per capture occasion

dat$id_chr <- dat$id
dat$id <- as.factor(dat$id)

dat$year_ori <- dat$year
dat$year <- as.integer(as.factor(dat$year))

dat$cave_ori <- dat$cave
dat$cave <- as.integer(as.factor(dat$cave))

## When is each fish first seen and what was their size?
first_cap <- NULL
for(i in levels(dat$id)){
	a <- dat[which(dat$id %in% i),]
	a <- as.data.frame(a[order(a$date),])
	b <- a$date[1]
	c <- a$fl[1]
	row.names(a) <- NULL
	a$first_date <- b
	a$first_fl <- c
	a$cap_num <- as.factor(rownames(a))
	first_cap <- rbind(first_cap,a)
}
first_cap[1:20,]
summary(first_cap)
# Number of captures of each fish
table(first_cap$cap_num)

# Vector with first capture occassion (k) for each fish (ordered on id)
first <- first_cap[c("id","k","cap_num")]
caps <- first
first[1:40,]
first <- first[which(first$cap_num == "1"),]
first <- first$k
first

# What are the mid-dates of each sample month?
mid_dates <- NULL
for(i in levels(as.factor(dat$capture_month))){
	a <- dat[which(dat$capture_month %in% i),]
	b <- mean(a$date)
	a$mid_date <- b
	mid_dates <- rbind(mid_dates,a)
}
mid_dates[1:20,]
str(mid_dates)
a <- unique(mid_dates$mid_date)
a <- format(a, format="%m-%d")
a <- sort(a)
b <- as.numeric(format(as.Date(a[1:7],format="%m-%d"),format="%d"))
mean(b) # 16th June
c <- as.numeric(format(as.Date(a[8:15],format="%m-%d"),format="%d"))
mean(c) # 21st August

# What are the average sizes when fish are captured multiple times within a catch month?
# For each sample month
# Are they caught multiple times?
# If they are then take the average
# If not then leave it
av_fl <- NULL
for(i in levels(as.factor(mid_dates$capture_month))){
	a <- mid_dates[which(mid_dates$capture_month %in% i),]
	for(k in levels(droplevels(a$id))){
		b <- a[which(a$id %in% k),]
		if(nrow(b) > 1){
			b$mean_fl <- mean(b$fl)
			av_fl <- rbind(av_fl,b)
		}
		else{
			b$mean_fl <- b$fl
			av_fl <- rbind(av_fl,b)
		}
	}
}
av_fl[1:30,]
summary(av_fl)

# Average mid-date for each sample month: 16th June and 21st August
# Put in for each year
av_mid_dates <- NULL
# 2012 only has august
a <- av_fl[which(av_fl$year_ori != "2012"),]
#a <- av_fl[which(av_fl$sample_date != "2012_b_2"),]
for(i in levels(droplevels(as.factor(a$year)))){
	b <- a[which(a$year %in% i),]
	c <- b[which(b$season == "june"),]
	c$av_mid_dat <- paste(c$year_ori,"-06-16",sep="")
	d <- b[which(b$season == "august"),]
	d$av_mid_dat <- paste(d$year_ori,"-08-21",sep="")
	b <- rbind(c,d)
	av_mid_dates <- rbind(av_mid_dates,b)
}
aug_2012 <- av_fl[which(mid_dates$year_ori == "2012"),]
aug_2012$av_mid_dat <- paste(aug_2012$year_ori,"-08-21",sep="")
av_mid_dates <- rbind(aug_2012,av_mid_dates)
av_mid_dates$av_mid_dat <- as.Date(av_mid_dates$av_mid_dat, "%Y-%m-%d")
str(av_mid_dates)
av_mid_dates[1:20,]
unique(av_mid_dates$av_mid_dat)

# Distance from mid-date
av_mid_dates$days <- as.numeric(as.Date(as.character(av_mid_dates$date),format="%Y-%m-%d") - as.Date(as.character(av_mid_dates$av_mid_dat),format="%Y-%m-%d"))
str(av_mid_dates)
av_mid_dates[1:20,]

# Indicate number of repeats in each capture occasion
av_mid_dates$day_index <- paste(as.integer(av_mid_dates$id),"_",av_mid_dates$k,sep="")
days <- NULL
for(i in levels(as.factor(av_mid_dates$day_index))){
	a <- av_mid_dates[which(av_mid_dates$day_index %in% i),]
	rownames(a) <- NULL
	a$day_index <- rownames(a)
	days <- rbind(days,a)
}
days[1:40,]
str(days)

data <- days[,c("id","season","k","fl","year","cave","date","av_mid_dat","days","day_index","id_chr","id_ori","year_ori","cave_ori")]
data$id <- as.integer(data$id)
data$fl <- as.numeric(data$fl)
data$k <- as.numeric(data$k)
str(data)

# Include first capture date
first_2 <- as.data.frame(first)
first_2$id <- as.integer(rownames(first_2))
str(first_2)
data <- merge(data,first_2, by="id")
str(data)
data[1:40,]
str(data)

# Index cave and season combined
# Index cave based on obs
head(data)
data$s2 <- as.integer(as.factor(data$season))
data$cave_season <- paste(data$cave,"_",data$s2,sep="")
data$cs_index <- paste(as.integer(data$id),"_",data$cave,"_",data$k,sep="")

# Include the k for yearly variation
data$cave_k_season <- paste(data$cave,"_",data$k,"_",data$s2,sep="")
data$cave_k <- paste(data$cave,",",data$k,sep="")
head(data)

####################
# Temperature data #
####################
temps <- read.csv("temps_data_wide.csv")
temps <- temps[,-1]
temps$date <- as.Date(temps$date)
str(temps)
# 201320 observations

# Daily means for each cave
daily_means <- aggregate(temps[,5:24],list(temps$date),mean)
colnames(daily_means)[1] <- "Date"
str(daily_means)
# This gives 2518 dates that have a daily estimate

# 17th January is mid-winter
daily_means$Day <- format(daily_means$Date, format="%m-%d")
winter <- daily_means[which(daily_means$Day == "01-17"),]
winter$k <- c(2,4,6,8,10,12,14)
# 19th July is mid-summer
summer <- daily_means[which(daily_means$Day == "07-19"),]
summer$k <- c(3,5,7,9,11,13,15)

temp_day_means <- rbind(summer,winter)

# Convert to long format
temps_day_long <- gather(temp_day_means, cave, temperature, C1:C27, factor_key = TRUE)
temps <- temps_day_long

temps$cave_ori <- temps$cave
temps$cave <- as.integer(temps$cave)
str(temps)

# Add the temperature of each cave for each k where data is available
temps$cave_k <- paste(temps$cave,temps$k,sep=",")
str(temps)
data$temp <- temps$temperature[match(data$cave_k,temps$cave_k)]
table(is.na(data$temp))
# 7046 with temperatures, 2201 without

#write.csv(data,"model_data.csv")
