# Title: Setting up data to make a voluteering flow diagram
# Author: Meenakshi Parameshwaran
# Date: 09/11/2015


# Set working directory and load data ----------------------------------------------------

# clear the workspace
rm(list = ls())

# set working directory
setwd("C:/Users/Meenakshi/Documents/GitHub/Youth-Volunteering"

# load pacman package and other required packages
library(pacman)
pacman::p_load(readstata13, dplyr, tidyr, plyr, gdata)

# load in the data
mydf <- read.dta13("C:/Users/Meenakshi/Documents/USocData/flowdiagramcases.dta")

# check variable names
names(mydf)

# check structure of df
str(mydf)


# Prepare the variables ---------------------------------------------------

# convert wave to a factor
mydf$wave <- factor(mydf$wave)
levels(mydf$wave)
levels(mydf$wave) <- c("Wave 2", "Wave 4") # use better names for the levels

# convert to factor and re-level volunteering frequency 
mydf$vol <- factor(mydf$ypvolfreq)
levels(mydf$vol)
levels(mydf$vol) <- c("Never/almost never", "Once a year or less", "Several times a year", "At least once a month", "At least once a week", "Most days")
table(mydf$vol) # check the distribution

mydf$ypvolfreq <- NULL # get rid of the duplicate ypvolfreq var


# Reshape the data to wide --------------------------------------------------------

voldfw <- spread(data = mydf, wave, vol)
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
vol$wave2dum[vol$volwave2 == 1] <- 0 # 0 for those who volunteer never/almost never
vol$wave2dum[vol$volwave2 > 1] <- 1 # 1 for everyone else
vol$wave4dum[vol$volwave4 == 1] <- 0 # 0 for those who volunteer never/almost never
vol$wave4dum[vol$volwave4 > 1] <- 1 # 1 for everyone else

# remember that larger volwave numbers mean more frequent volunteering

vol$pattern[vol$volwave4 == vol$volwave2 & vol$wave2dum == 0 ] <- 1 # never volunteered
vol$pattern[vol$volwave4 > vol$volwave2 & vol$wave2dum == 0 ] <- 2 # no vol in wave 2 and more vol in wave 4
vol$pattern[vol$wave4dum == 0 & vol$wave2dum == 1 ] <- 3 # volunteered in wave 2 but then stopped
vol$pattern[vol$volwave4 < vol$volwave2 & vol$wave2dum == 1 & vol$wave4dum == 1] <- 4 # volunteered in wave 2 and does less in wave 4
vol$pattern[vol$volwave4 == vol$volwave2 & vol$wave2dum == 1 ] <- 5 # volunteered in wave 2 and does the same in wave 4
vol$pattern[vol$volwave4 > vol$volwave2 & vol$wave2dum == 1 ] <- 6 # volunteered in wave 2 and does more in wave 4

# check the distribution
addmargins(table(vol$pattern))

# get the numbers for the graph
nrow(vol) # 1921 obs
mytable <- addmargins(table(vol$wave2dum, vol$pattern)) # table showing volunteering in each wave
round(mytable/1921,2)*100 # convert to percentages with 1921 total youth as the base
# enter these percentages to make the sankey on sankeymatic.com

# final tidy up
rm(list = ls())

# END #
