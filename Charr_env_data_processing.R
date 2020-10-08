#########################################
## Charr Environmental Data Processing ##
#########################################
# Lizy
# 24th July 2020 -- final update

library(tidyr)
library(reshape2)

################
# TEMPERATURES #
################
temps <- read.csv("cave_temps.csv")

# Create column names
for(i in 1:26){
	temps[,i] <- as.character(temps[,i])
}
temps <- temps[,-27]
temps[1,4:26] <- sub("^", "C", temps[1,4:26])
temps[1,5:6] <- sub(" ", "_", temps[1,5:6])
temps[1,15] <- sub("B","b",temps[1,15])
# Cave 27 has C27_front and C27_back
# Use the one at the back for now so rename it as C27
temps[1,5] <- sub("_back","",temps[1,5])
temps[1,3] <- "Date_time"
colnames(temps) <- temps[1,]
temps <- temps[-1,]
str(temps)

# Split dataframe on date format
long_date <- grep("M", temps$Date_time, value = TRUE)
long <- subset(temps[which(temps$Date_time %in% long_date),])
str(long) # 24th July 2020 -- 7091/10067 have the AM / PM time format in the Date_time column

na_date <- subset(temps[which(temps$Date_time == ""),])
str(na_date) # none with no date 24/07/2020

short <- subset(temps[which(!(temps$Date_time %in% long$Date_time)),])
short <- subset(short[which(!(short$Date_time %in% na_date$Date_time)),])
str(short) # 2976/10067 have the 24 hour clock format 24/07/2020

# Format all as 24 hour date times
short$Date_time <- as.POSIXct(short$Date_time, format = "%m/%d/%Y %H:%M")
typos <- short[which(short$Date_time > "2019-11-25 00:00:00"),]
# From October 2018 -- present, every month the 1st -- 12th (inclusive)
unique(typos$Date_time)
# No typos 24th July 2020

long$Date_time <- as.POSIXct(long$Date_time, format = "%m.%d.%y %I:%M:%S %p")
long_date[is.na(as.POSIXct(long_date, format = "%m.%d.%y %I:%M:%S %p"))] # line 7149, 21st August 2017 18h is lost as an NA, not sure why? Remove for now
long <- long[!is.na(long$Date_time),]

temps_2 <- rbind(long,short)
temps_2 <- temps_2[order(temps_2$Date_time),]
temps_2[1400:1500,]
for(i in 4:26){
	temps_2[,i] <- as.numeric(temps_2[,i])
}
str(temps_2)
# Total 10066 with dates now 24th July 2020

# Remove C5 values that are much too high
for(i in 1:length(temps_2$C5)){
	if(isTRUE(temps_2$C5[i] > 20)){
		temps_2$C5[i] <- NA
	}
}
temps_2$C5[temps_2$C5 > 20]

temps_2$date <- as.Date(temps_2$Date_time)

### Caves relevant to the project: 
Caves <- c("C1","C2","C5","C6","C7","C10","C11","C12","C17","C17b","C18","C19","C20","C21","C22","C23","C24","C25","C26","C27")
names <- c("year","month","date","Date_time",Caves)
temps_wide <- subset(temps_2, select = names)

# Convert to long format
temps_long <- gather(temps_wide, cave, temperature, C1:C27, factor_key = TRUE)

## CLEAN DATA -- wide format ##
str(temps_wide)
#write.csv(temps_wide,"temps_data_wide.csv")

## CLEAN DATA -- long format ##
str(temps_long)
#write.csv(temps_long,"temps_data_long.csv")





