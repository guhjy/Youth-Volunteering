# Title: Setting up data to make a voluteering flow diagram
# Author: Meenakshi Parameshwaran
# Date: 05/11/2015


# Set working directory and load data ----------------------------------------------------

# clear the workspace
rm(list = ls())

# set working directory
setwd("C:/Users/Meenakshi/Dropbox/Meena's work/The State of Social Capital in Britain/Meena dofiles")

# load pacman package and other required packages
library(pacman)
pacman::p_load(readstata13, dplyr, tidyr, plyr, gdata)

# load in the data
mydf <- read.dta13("C:/Users/Meenakshi/Documents/USocData/youthlong.dta")

# check variable names
names(mydf)

# remove the yp prefix again
names(mydf) <- sub("^yp", "", names(mydf))  


# Prepare volunteering vars -----------------------------------------------

# find volunteering variables
names(select(mydf, contains("vol"))) # use dplyr to search
table(mydf$fvolunt)

# make a dummy for volunteering at least once a year
levels(mydf$fvolunt)
mydf$voldum <- mydf$fvolunt
levels(mydf$voldum) <- c(NA, NA, NA, NA, 1, 1, 1, 1, 1, 0)
levels(mydf$voldum)
table(mydf$voldum)

# Make a df of just ID, wave and volunteering activity ---------------------------------------
voldf <- dplyr::select(mydf, pidp, wave, voldum) # just keep ID, wave and voldum columns
voldf <- na.omit(voldf) ## get rid of NAs
voldf <- voldf[order(voldf$pidp, voldf$wave),] # sort the data by ID and then wave


# Count volunteering in each wave -----------------------------------------

dplyr::count(voldf, wave) # how many respondents in each wave
dplyr::count(voldf, wave, voldum) # and by volunteering status
## 4921 in wave 2, split into 2606 vounteers and 2315 non-volunteers
# then 3977 in wave 4, split into 2281 volunteers and 1696 non-volunteers

# other calculations
dplyr::count(voldf) ## 8898 rows
voldf %>% group_by(wave) %>% summarise(n_distinct(pidp)) #6709

4921 - 3977 # 944 youths missing in wave 4 from wave 2

# figure out a way to keep just the rows where there is an observation for both waves
library(dplyr)
volcomp <- voldf %>% group_by(pidp) %>% filter(length(wave) > 1)

# another way to just keep those rows with obs for both waves
# voldf[duplicated(voldf$pidp)|duplicated(voldf$pidp, fromLast=TRUE),]

dplyr::count(volcomp, wave, voldum) # counts by wave and volunteering status


# Reshape to wide format --------------------------------------------------
voldfw <- spread(data = volcomp, wave, voldum)
names(voldfw)[2] <- "volwave2" # rename the 2nd column
names(voldfw)[3] <- "volwave4" # rename the 3rd column

# clean up
vol <- voldfw
gdata:: keep(vol, sure = T)


# Create volunteering pattern variable ------------------------------------
vol$pattern[vol$volwave2 == 0 & vol$volwave4 == 0] <- 1 # code 1 for no volunteering in either wave
vol$pattern[vol$volwave2 == 1 & vol$volwave4 == 0] <- 2 # code 2 for volunteering in wave 2 only
vol$pattern[vol$volwave2 == 0 & vol$volwave4 == 1] <- 3 # code 3 for volunteering in wave 4 only
vol$pattern[vol$volwave2 == 1 & vol$volwave4 == 1] <- 4 # code 4 for volunteering in both waves

table(vol$pattern) # numbers in different volunteering patterns
table(vol$volwave2) # how many volunteer in wave 2
table(vol$volwave2, vol$volwave4) # table showing volunteering in each wave
# use numbers in two table above to make the sankey on sankeymatic.com



# Frequency of volunteering -----------------------------------------------

# repeat the analysis above by this time with frequency of volunteering

# clear the workspace
rm(list = ls())

# set working directory
setwd("C:/Users/Meenakshi/Dropbox/Meena's work/The State of Social Capital in Britain/Meena dofiles")

# load pacman package and other required packages
library(pacman)
pacman::p_load(readstata13, dplyr, tidyr, plyr, gdata)

# load in the data
mydf <- read.dta13("C:/Users/Meenakshi/Documents/USocData/youthlong.dta")

# check variable names
names(mydf)

# remove the yp prefix again
names(mydf) <- sub("^yp", "", names(mydf))  


# Prepare volunteering vars 

# find volunteering variables
names(select(mydf, contains("vol"))) # use dplyr to search
table(mydf$fvolunt)

# clean up the frequency var
levels(mydf$fvolunt)
mydf$volf <- mydf$fvolunt
levels(mydf$volf) <- c(NA, NA, NA, NA, 1, 2, 3, 4, 5, 6)
levels(mydf$volf)
table(mydf$volf)

# keep just pidp, wave and vol freq.
voldf <- dplyr::select(mydf, pidp, wave, volf) # just keep ID, wave and volf columns
voldf <- na.omit(voldf) ## get rid of NAs
voldf <- voldf[order(voldf$pidp, voldf$wave),] # sort the data by ID and then wave

# figure out a way to keep just the rows where there is an observation for both waves
library(dplyr)
volcomp <- voldf %>% group_by(pidp) %>% filter(length(wave) > 1)

# reshape the data
voldfw <- spread(data = volcomp, wave, volf)
names(voldfw)[2] <- "volwave2" # rename the 2nd column
names(voldfw)[3] <- "volwave4" # rename the 3rd column

# clean up
vol <- voldfw
gdata:: keep(vol, sure = T)

# add a column to show if frequency of volunteering has gone up, stayed the same, or gone down
levels(vol$volwave2)
vol$volwave2 <- as.numeric(vol$volwave2) # convert to numeric to allow comparison
vol$volwave4 <- as.numeric(vol$volwave4) # convert to numeric to allow comparison

# create vol dummes for wave 2 and wave 4
vol$wave2dum[vol$volwave2 == 6] <- 0 # 0 for those who volunteer never/almost never
vol$wave2dum[vol$volwave2 < 6] <- 1 # 1 for everyone else
vol$wave4dum[vol$volwave4 == 6] <- 0 # 0 for those who volunteer never/almost never
vol$wave4dum[vol$volwave4 < 6] <- 1 # 1 for everyone else

# remember that larger volwave numbers mean less frequent volunteering

vol$pattern[vol$volwave4 == vol$volwave2 & vol$wave2dum == 0 ] <- 1 # never volunteered
vol$pattern[vol$volwave4 < vol$volwave2 & vol$wave2dum == 0 ] <- 2 # more vol in wave 4
vol$pattern[vol$wave4dum == 0 & vol$wave2dum == 1 ] <- 3 # volunteered in wave 2 but then stopped
vol$pattern[vol$volwave4 > vol$volwave2 & vol$wave2dum == 1 & vol$wave4dum != 0] <- 4 # volunteered in wave 2 and does less in wave 4
vol$pattern[vol$volwave4 == vol$volwave2 & vol$wave2dum == 1 ] <- 5 # volunteered in wave 2 and does the same in wave 4
vol$pattern[vol$volwave4 < vol$volwave2 & vol$wave2dum == 1 ] <- 6 # volunteered in wave 2 and does more in wave 4

# check the distribution
addmargins(table(vol$pattern))

# get the numbers for the graph
nrow(vol) # 2189 obs
mytable <- addmargins(table(vol$wave2dum, vol$pattern)) # table showing volunteering in each wave
round(mytable/2189,2)*100 # convert to percentages with 2189 total youth as the base
# enter these percentages to make the sankey on sankeymatic.com

# final tidy up
rm(list = ls())

# END #
