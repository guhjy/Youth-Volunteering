#' ---	
#' title: "Merging USoc waves 1-4"	
#' author: "Meenakshi Parameshwaran"	
#' date: "29 October 2015"	
#' output: html_document	
#' ---	
#' 	
#' This file merges together waves 1-4 of USoc data files of type _indresp and _youth. This is part of the youth volunteering project with Matt Bennett.	
#' 	
#' Note that my USoc files are tab delimited for easy loading.	
#' 	
#' ### Indresp files	
#' 	
#' 	
# change the working directory to where the datasets are	
setwd("~/Documents/USocWaves14/tab")	
	
# see the files there	
list.files()	
	
# read in the _indresp files	
ai <- read.delim(file = "a_indresp.tab") # a is the wave 1 file	
View(ai) # have a quick look at the (ai) file	
bi <- read.delim(file = "b_indresp.tab")	
ci <- read.delim(file = "c_indresp.tab")	
di <- read.delim(file = "d_indresp.tab")	
#' 	
#' 	
#' All four _indresp datasets are now loaded in ok.	
#' Here's a helpful link on how to merge the individual datasets together and pt them into long-format. https://www.understandingsociety.ac.uk/d/201/user_guide.pdf?1438097637	
#' 	
#' I want to turn the dataset into long format, so I will use the help from p84.	
#' 	
#' First I need to remove prefixes and add a wave variable.	
#' 	
# all of these are done the lazy way, but would be better with a loop	
	
# remove prefixes from each wave	
names(ai) <- sub("^a_", "", names(ai))	
names(bi) <- sub("^b_", "", names(bi))	
names(ci) <- sub("^c_", "", names(ci))	
names(di) <- sub("^d_", "", names(di))	
	
# add a wave variable to each dataset	
ai$wave <- 1	
bi$wave <- 2	
ci$wave <- 3	
di$wave <- 4	
#' 	
#' 	
#' Now I need to append the four indresp dataframes together. The four dataframes have differenct numbers of variables, so I can't do use the standard rbind function. Instead, I'll use bind\_rows from the dplyr package, (this replaces the older function rbind\_list).	
#' 	
#' 	
# append the rows	
library(dplyr)	
ei <- bind_rows(ai, bi, ci, di)	
	
# order the data by pidp and check	
ei <- ei[order(ei$pidp),]	
fi <- as.data.frame(cbind(ei$pidp, ei$wave))	
fi[1:50,] # seems ok	
	
# write out the ei dataframe to Stata	
library(foreign)	
write.dta(ei, "indresplong.dta")	
	
# clean up workspace	
rm(list = ls())	
	
#' 	
#' 	
#' That's it for indresp datasets. The Stata dataset in long format of all four indresps is called "indresplong.dta".	
#' 	
#' ## Youth files	
#' 	
#' Now I'll repeat the process for the youth files.	
#' 	
#' 	
# change the working directory to where the datasets are	
setwd("~/Documents/USocWaves14/tab")	
	
# see the files there	
list.files()	
	
# read in the _youth files	
ai <- read.delim(file = "a_youth.tab")	
bi <- read.delim(file = "b_youth.tab")	
ci <- read.delim(file = "c_youth.tab")	
di <- read.delim(file = "d_youth.tab")	
#' 	
#' 	
#' All four youth datasets are now loaded in ok.	
#' Now need to remove prefixes and add a wave variable.	
#' 	
# all of these are done the lazy way, but would be better with a loop	
	
# remove prefixes from each wave	
names(ai) <- sub("^a_", "", names(ai))	
names(bi) <- sub("^b_", "", names(bi))	
names(ci) <- sub("^c_", "", names(ci))	
names(di) <- sub("^d_", "", names(di))	
	
# add a wave variable to each dataset	
ai$wave <- 1	
bi$wave <- 2	
ci$wave <- 3	
di$wave <- 4	
#' 	
#' 	
#' Now I need to append the four youth dataframes together. The four dataframes have differenct numbers of variables, so I can't do use the standard rbind function. Instead, I'll use bind\_rows from the dplyr package, (this replaces the older function rbind\_list).	
#' 	
#' 	
# append the rows	
library(dplyr)	
ei <- bind_rows(ai, bi, ci, di)	
	
# order the data by pidp and check	
ei <- ei[order(ei$pidp),]	
fi <- as.data.frame(cbind(ei$pidp, ei$wave))	
fi[1:50,] # seems ok	
	
# write out the ei dataframe to Stata	
library(foreign)	
write.dta(ei, "youthlong.dta")	
	
# clean up workspace	
rm(list = ls())	
	
#' 	
#' 	
#' That's it for youth datasets. The Stata dataset in long format of all four youths is called "youthlong.dta".	
#' 	
#' That's both files in long format now. They can be transferred to Dropbox or run on a local machine.	
