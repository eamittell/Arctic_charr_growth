###########################
## Processing Charr data ##
###########################
# Lizy
# 3rd July 2020 -- final update

library(readxl)
library(tidyverse)

# Using the most recent data
data_r <- read_excel("Masterfile_Vertical_19_09_19.xls", sheet = "final")
# Check and rename 
colnames(data_r)
names(data_r)[names(data_r) == "Date of capture"] <- "date"
names(data_r)[names(data_r) == "length"] <- "fl"
names(data_r)[names(data_r) == "Location"] <- "cave"
names(data_r)[names(data_r) == "tag"] <- "id"

str(data_r)
summary(data_r)

unique(data_r$date)
# August 2018 has 2022 instead -- correct
data_r$date <- gsub("2022", "2018", data_r$date)
# C17b is both small and capital b. Make all small
data_r$cave <- gsub("B", "b", data_r$cave)
# Some have little c instead of capital for cave
data_r$cave <- gsub("c", "C", data_r$cave)
table(data_r$cave)

# Remove individuals from C13, C8, C4, C3, C9 as these are only once in the first dataset and there isn't continuous information on these caves
data_r[which(data_r$cave == "C13"),]
data_r[which(data_r$id=="184910"),]
data_r <- subset(data_r[which(data_r$cave!="C13"),])
data_r[which(data_r$cave == "C8"),]
data_r[which(data_r$id=="4EDD48"),]
data_r[which(data_r$id=="4EEA41"),]
data_r[which(data_r$id=="C9839E"),]
data_r <- subset(data_r[which(data_r$cave!="C8"),])
data_r <- subset(data_r[which(data_r$cave!="C3"),])
data_r <- subset(data_r[which(data_r$cave!="C4"),])
data_r <- subset(data_r[which(data_r$cave!="C9"),])

# Make season uniform
data_r$season <- gsub("June", "june", data_r$season)
data_r$season <- gsub("JUNE", "june", data_r$season)
data_r$season <- gsub("AUGUST", "august", data_r$season)

# Check data and remove missing data
str(data_r)
data_r[is.na(data_r$id),]
data_r <- subset(data_r[which(!is.na(data_r$id)),])
data_r$id <- factor(data_r$id)
data_r$id_num <- as.numeric(data_r$id)
data_r$visit <- as.integer(as.character(data_r$visit))
data_r <- subset(data_r[which(!is.na(data_r$fl)),])
data_r$fl <- as.numeric(data_r$fl)
#data_r$fl <- gsub("?", "", data_r$fl) 
# ? is a special character in R so have just removed them from the data instead for now. But only in the "final" sheet and not the others.
data_r <- subset(data_r[which(!is.na(data_r$date)),])
data_r$date <- as.Date(data_r$date, "%Y-%m-%d")
data_r$season <- as.factor(data_r$season)
data_r$year <- as.integer(data_r$year)

# Below young of the year that are caught size -- potential typos
data_r[which(data_r$fl < 25),]
# Change it
data_r$fl[data_r$fl < 25] <- data_r$fl[data_r$fl < 25]*10

# List of uncertain tags for Ignacy
uncertain <- c("?????","?","E1","E2","E3","NOT VIE TAGGED","NA","-","YOY","XXXXX")
uncertain_data <- data_r[which(data_r$id %in% uncertain),]
#write.csv(uncertain_data,"ambiguous_tags.csv")

# Remove uncertain tags
data_r <- data_r[which(data_r$id != "?????"),]
data_r <- data_r[which(data_r$id != "?"),]
data_r <- data_r[which(data_r$id != "E1"),]
data_r <- data_r[which(data_r$id != "E2"),]
data_r <- data_r[which(data_r$id != "E3"),]
data_r <- data_r[which(data_r$id != "NOT VIE TAGGED"),]
data_r <- data_r[which(data_r$id != "NA"),]
data_r <- data_r[which(data_r$id != "-"),]
data_r <- data_r[which(data_r$id != "YOY"),]
data_r <- data_r[which(data_r$id != "XXXXX"),]
data_r$id <- droplevels(data_r$id)
unique(data_r$id)
str(data_r)

rownames(data_r) <- NULL
data_r$index <- rownames(data_r)
# Keep original ids before processing
data_r$id_ori <- data_r$id
## 3/7/2020 -- 9531 measurements

## Have 97 deaths observed from 85 individuals tag numbers (could be re-used and then die in a different fish) that are assigned as dead -- 3/7/2020
dead <- as.data.frame(data_r[which(data_r$dead=="1"),])
length(unique(dead$id))
str(dead)
#write.csv(dead, "fish_observed_dead.csv")

dead_a_17 <- dead[which(dead$season == "august" & dead$year == "2017"),]
dim(dead_a_17) # 19 from august 2017 most of which seem to have died when tagged so just one measurement, then tag re-used in a different fish
dead_j_16 <- dead[which(dead$season == "june" & dead$year == "2016"),]
dim(dead_j_16) # 11 from june 2016
plot(data_r$fl~data_r$date)
lines(data_r$fl[data_r$id %in% dead$id]~data_r$date[data_r$id %in% dead$id], col = 2, type = "l")
dead_2 <- data_r[data_r$id %in% dead$id,] # 85 dead tags 3/7/2020 -- 407 measurements
str(dead_2)
#write.csv(dead_2, "fish_with_dead_tag.csv")

#### NOT GOING THROUGH DEAD PROCESSING ####
# These have never been associated directly with a dead tag
clean_data <- data_r[!(data_r$id %in% dead_2$id),]
str(clean_data)
# 3/7/2020 -- 9124 measurements, 3605 ids
########################################### 

# By eye 27 individuals found with re-used tags
# 19 from August 2017
# 6 from June 2016
# 1 each from June 2017 and August 2016 -- These two will be more complicated to find automatically because they are both dead on the second date. 897959 C7 June 2017 is dead on the recap and then the tag is used again in a different cave so should put cave id into the loop if want to catch examples like this. However, 2549888 C25 dead August 2016 is used in a different cave yes (C19), but it is also used the day before and the fish died on the second day. This does not make sense. So I could catch this as a different individual here using the movement between caves, but it still has to be a typo of some sort.
# For each tag
# Create a list of unique id_numbers to use first
dead_2$id_dead <- as.numeric(droplevels(dead_2$id)) ## dead_2 contains all measurements with dead id
# Sort by date
dead_2 <- arrange(dead_2,date)
dim(dead_2)

# 28 of the dead individuals are just there once and have not been re-used and therefore they are not re-captured
# 3/7/2020 -- 28 indivs
indivs <- dead_2[!(duplicated(dead_2$id)|duplicated(dead_2$id,fromLast=TRUE)),]
indivs$id <- as.character(indivs$id)
# Remove from dead_2
dead_2 <- dead_2[which(!(dead_2$id %in% indivs$id)),]
# Add a "D" just in case they are reused at some point in the future
indivs$id <- paste(indivs$id,"D",sep="")
indivs

# Colour tags have been repeated in different caves?
# Make these id's compound based on cave
levels(droplevels(dead_2$id)) # all have a V in them 
c_t <- grep("V",dead_2$id,value=T)
col_tags <- as.data.frame(dead_2[which(dead_2$id %in% c_t),])
col_tags[1:40,]
# Remove from dead_2
dead_2 <- dead_2[which(!(dead_2$id %in% c_t)),]
col_tags$id <- paste(col_tags$id,"_",col_tags$cave,sep="")
# Put back into dead_2
dead_2 <- rbind(dead_2,col_tags)

dim(dead_2) # 379 obs from 164 unique tags
dead_2$id_dead <- as.numeric(droplevels(dead_2$id))
reused_tags <- data.frame()
dead_tags <- data.frame()
# For each tag
for(i in 1:length(unique(dead_2$id_dead))){
	# Get entries for the tag
	a <- as.data.frame(dead_2[which(dead_2$id_dead == i),])
	# If multiple dates and dead in first row
	if(nrow(a) > 1 & isTRUE(a[1,"dead"] == "1")){
		# Add a "D" to the tag id
		a$id <- as.character(a$id)
		a$id[1] <- paste(a$id[1],"D",sep="")
		reused_tags <- rbind(reused_tags,a)
	}
	else if(length(unique(a$cave)) > 1){
		# Check if the tag is in multiple caves
		# Select rows with cave with dead fish
		c <- a[which(a$dead == "1"),]
		d <- a[which(a$cave %in% c$cave),]
		# Remove from a
		e <- rbind(a,d)
		e <- e[!duplicated(e,fromLast=FALSE)&!duplicated(e,fromLast=TRUE),]
		# Add a "D" to the tag id for the cave that has dead in it
		d$id <- paste(d$id,"D",sep="")
		# Take those that did not get a D and add them to the same dataframe
		f <- rbind(d,e)
		reused_tags <- rbind(reused_tags,f)
	}
	else{
		# If single record of dead fish
		dead_tags <- rbind(dead_tags,a)
	}
}
dim(reused_tags)
dim(dead_tags)
#write.csv(reused_tags, "tags_reused_from_dead_fish.csv")
unique(reused_tags$id)

# Combined changed names and those dead whose tags have not been reused
final_dead <- rbind(reused_tags,indivs,dead_tags)
dim(final_dead)
#write.csv(final_dead,"final_dead.csv")

# Plot length over time with growth trajectories of dead individuals
plot(data_r$fl ~ data_r$date)
#lines(final_dead$fl[final_dead$id =="123867"]~final_dead$date[final_dead$id == "123867"], col = 2, type = "l")
for (i in 1:length(final_dead$id)){
	a <- final_dead$id[i]
	#a <- sample(final_dead$id,1)
	lines(final_dead$fl[final_dead$id %in% a]~final_dead$date[final_dead$id %in% a], col = 2, type = "l")
}

# Recombine dead processed and other data
final_dead_1 <- final_dead[,-16]
data <- rbind(final_dead_1,clean_data)

##### SEVEN DIGIT TAGS #####
# Using from clean data set, but need to remember that there may be some seven digit tags in the dead processing
# 249 obs in total data; 236 not among the dead ids 3/7/2020
seven <- clean_data[which(nchar(as.character(clean_data$id))=="7"),]
seven_dead <- final_dead[which(nchar(as.character(final_dead$id_ori))=="7"),]
# 183 individuals in total data; 173 not among the dead ids 3/7/2020
length(unique(seven$id))
seven_dead <- seven_dead[,-16]
seven <- rbind(seven,seven_dead)
#write.csv(seven,"seven_digit_tags.csv")

# There are 81 7digit tags in the comments column. These are the ones that have had changes in their id
z <- sub('\\D*(\\d{7}).*', '\\1', grep("[[:digit:]]{7}",data$comment,value=T))
# 79 of them are unique
length(unique(z))

# Get rows from data that have a 7tag in the comments column
c_seven <- data[which(data$comment %in% grep("[[:digit:]]{7}",data$comment,value=T)),]
str(c_seven)

# Which 7tags are in the comments column (z) but not in the id column?
# 6 of them
z_2 <- z[which(!(z %in% data$id))]
# Get these rows out of the main dataframe to find out what is going on with them
poss_typo_sevens <- c_seven[which(sub('\\D*(\\d{7}).*', '\\1', grep("[[:digit:]]{7}",c_seven$comment,value=T)) %in% z_2),]
# One is from a clear typo: 981362
#write.csv(poss_typo_sevens,"poss_typo_sevens.csv")
# There are FIVE that are potential for error as they are not found as ids in the main data but cannot know without photos which individuals these are. It is likely that they are from the very early capture occasion (June 2012) that is not used here
data[which(data$id %in% z_2),]

# 75 in comments column that have an entry in id column as well
seven_2 <- c_seven[which(!(sub('\\D*(\\d{7}).*', '\\1', grep("[[:digit:]]{7}",c_seven$comment,value=T)) %in% z_2)),]
as.data.frame(seven_2)
#write.csv(seven_2,"sevens_with_comment.csv")
# There are 239 observations related to these ids
str(data[which(data$id_ori %in% seven_2$id_ori),])

# All entries that have an old_tag from these fish
z_3 <- z[!(z %in% z_2)]
old <- data[which(as.character(data$id) %in% z_3),]
str(old) # 106

# Rename the old_tags with the new_tags
tags <- cbind(as.character(seven_2$id),sub('\\D*(\\d{7}).*', '\\1', grep("[[:digit:]]{7}",seven_2$comment,value=T)))
tags <- as.data.frame(tags)
colnames(tags)[1] <- "new"
colnames(tags)[2] <- "old"
str(tags)
# Remove those with duplicates from tags dataframe 
dup <- tags[which(duplicated(tags$old)),]
tags_2 <- tags[which(!(tags$old %in% as.character(dup$old))),]

change <- NULL
not <- NULL
a <- data
row.names(a) <- NULL
a$index <- row.names(a)
for(i in 1:length(unique(a$index))){
	b <- a[which(a$index==i),]
	if(b$id %in% tags_2$old){
		c <- tags_2[which(as.character(tags_2$old) %in% as.character(b$id)),]
		b$id <- c$new
		change <- rbind(change,b)
	}
	else{
		d <- b$id
		not <- rbind(not,d)
	}		
}
dim(change)
dim(not)
dim(data)
#write.csv(change,"seven_tags_changed.csv")
# Expecting and got 104 changes. The 2 duplicates need to be dealt with next

# Duplicates
tags_3 <- tags[which(tags$old %in% as.character(dup$old)),]
lost <- data[which(data$id %in% tags_3$new),]
#write.csv(lost,"seven_tags_uncertain.csv")
# Replace 3943040 and 655371 with 981272
a$id <- replace(a$id, a$id == "3943040", "981272")
a$id <- replace(a$id, a$id == "655371", "981272")
lost_1 <- a[which(a$id == "981272"),]
# Replace 6524416 and 655295 with 158517
a$id <- replace(a$id, a$id == "6524416", "158517")
a$id <- replace(a$id, a$id == "655295", "158517")
lost_2 <- a[which(a$id == "158517"),]

lost_all <- rbind(lost_1,lost_2)

tag_changes <- rbind(change,lost_all)
dim(tag_changes)
#write.csv(tag_changes,"final_seven_tag_changes.csv")

# Plot length over time with growth trajectories of tag changes
plot(data_r$fl ~ data$date)
for (i in 1:length(tag_changes$id)){
	a <- tag_changes$id[i]
	lines(tag_changes$fl[tag_changes$id %in% a]~tag_changes$date[tag_changes$id %in% a], col = 2, type = "l")
}

# None of the tag changes have died
tag_changes[which(!is.na(tag_changes$dead)),]

#### NOT GOING THROUGH 7DIGIT TAG PROCESSING ####
clean_data_2 <- clean_data[!(clean_data$index %in% tag_changes$index),]
#################################################

##### Consistent IDs #####
# Combine things that have not changed with these changes to have a new clean dataset with the correct ids
final_dead <- final_dead[,-16] # remove id_dead
charr_data <- rbind(clean_data_2, final_dead, tag_changes)

# Save data
#write.csv(charr_data, "charr_data_id_changes.csv")

# Plot length over time with a few random growth trajectories 
plot(charr_data$fl ~ charr_data$date)
for (i in 1:100){
	a <- sample(charr_data$id,1)
	lines(charr_data$fl[charr_data$id %in% a]~charr_data$date[charr_data$id %in% a], col = 2, type = "l")
}

# Most are ok, but there are still some random downward jumps
# Extract these from the data to see which these are
# Individuals that have a change in fork length that decreases
# Get change in fork length over time
# Get average and variation
# Check for those that have gone down or had an excessive jump up?

# Get dataframe with all repeat measurements first -- 9534 total measurements -- 1643 single captures -- 7891 multiple measurements
multiple_fl <- charr_data[duplicated(charr_data$id) | duplicated(charr_data$id, fromLast = TRUE),]
multiple_fl$id <- droplevels(multiple_fl$id)
singles <- charr_data[which(!(charr_data$id %in% multiple_fl$id)),]
str(charr_data)
str(multiple_fl)
str(singles)

decreases_1 <- NULL
for(i in levels(multiple_fl$id)){
	a <- multiple_fl[which(multiple_fl$id %in% i),]
	a <- as.data.frame(a[order(a$date),])
	for(k in 2:length(a$id)){
		if(a$fl[k-1] > a$fl[k] & (a$fl[k] - a$fl[k-1]) < -1){
			b <- a[(k-1):k,]
			decreases_1 <- rbind(decreases_1,b)
		}
	}
}
str(decreases_1)
decreases_1[1:20,]
decreases_1$id <- droplevels(decreases_1$id)
length(grep("recap",decreases_1$comment,value=T))
#write.csv(decreases_1,"fl_decreases_over_one_mm.csv") 
# 522 with >1mm threshold, 17 recaps?

decreases_2 <- NULL
for(i in levels(multiple_fl$id)){
	a <- multiple_fl[which(multiple_fl$id %in% i),]
	a <- as.data.frame(a[order(a$date),])
	for(k in 2:length(a$id)){
		if(a$fl[k-1] > a$fl[k] & (a$fl[k] - a$fl[k-1]) < -2){
			b <- a[(k-1):k,]
			decreases_2 <- rbind(decreases_2,b)
		}
	}
}
str(decreases_2)
decreases_2[1:20,]
decreases_2$id <- droplevels(decreases_2$id)
length(grep("recap",decreases_2$comment,value=T))
#write.csv(decreases_2,"fl_decreases_over_two_mm.csv") 
# 354 with >2mm, 10? recaps

decreases_3 <- NULL
for(i in levels(multiple_fl$id)){
	a <- multiple_fl[which(multiple_fl$id %in% i),]
	a <- as.data.frame(a[order(a$date),])
	for(k in 2:length(a$id)){
		if(a$fl[k-1] > a$fl[k] & (a$fl[k] - a$fl[k-1]) < -3){
			b <- a[(k-1):k,]
			decreases_3 <- rbind(decreases_3,b)
		}
	}
}
str(decreases_3)
decreases_3[1:20,]
decreases_3$id <- droplevels(decreases_3$id)
length(grep("recap",decreases_3$comment,value=T))
#write.csv(decreases_3,"fl_decreases_over_three_mm.csv") 
# 272 with >3mm, 4? recaps

# Plot length over time with a few random growth trajectories and decrease in size highlighted
decreases <- decreases_3
plot(charr_data$fl ~ charr_data$date)
for(i in 1:100){
	a <- sample(charr_data$id,1)
	lines(charr_data$fl[charr_data$id %in% a]~charr_data$date[charr_data$id %in% a], col = 2, type = "l")
}
for(i in levels(decreases$id)){
		lines(decreases$fl[decreases$id %in% i]~decreases$date[decreases$id %in% i], col = 3, type = "l")	
}

# For now I am removing all those with >3mm drop from the data set, plus the recaps of same season with >2mm
charr_data <- charr_data[which(!(charr_data$index %in% decreases_3$index)),]
recap_2 <- decreases_2[which(decreases_2$comment == "RECAP"| decreases_2$comment == "Recap"| decreases_2$comment == "recap"),]
charr_data <- charr_data[which(!(charr_data$index %in% recap_2$index)),]
str(charr_data) # 9262 observations from 3810 fish with the ids as good as possible

#### CLEAN DATA ####
plot(charr_data$fl ~ charr_data$date)
for(i in 1:100){
	a <- sample(charr_data$id,1)
	lines(charr_data$fl[charr_data$id %in% a]~charr_data$date[charr_data$id %in% a], col = 2, type = "l")
}
# Save data
#write.csv(charr_data, "charr_data_fl_changed.csv")
#write.csv(charr_data, "charr_data.csv")
