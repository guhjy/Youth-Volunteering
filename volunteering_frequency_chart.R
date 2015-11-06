# Title: Making a simple volunteering descriptive graph
# Author: Meenakshi Parameshwaran
# Date: 06/11/15

# This script makes a simple frequency graph for volunteering

# Once again, the numbers come from Stata:

# 
# . tab ypfvolunt wave if allok == 1
# 
# do voluntary or |         wave
# community work   |         2          4 |     Total
# ----------------------+----------------------+----------
#     most days |        77         41 |       118 
# at least once a week |       115        152 |       267 
# at least once a month |       165        145 |       310 
# several times a year |       291        323 |       614 
# once a year or less |       405        418 |       823 
# never/almost never |       868        842 |     1,710 
# ----------------------+----------------------+----------
#     Total |     1,921      1,921 |     3,842 


# clear the workspace
rm(list = ls())

# make a data frame with this data in it

volfr <- c("Most days", "At least once a week", "At least once a month", "Several times a year", "Once a year or less", "Never/almost never")
w2 <- c(77, 115, 165, 291, 405, 868)
w4 <- c(41, 152, 145, 323, 418, 842)

# combine to make a df
mydf <- data.frame(cbind(volfr, w2, w4))

# check df structure
str(mydf)

# change w2 and w4 to numeric
mydf$w2 <- as.numeric(levels(mydf$w2))[mydf$w2]
mydf$w4 <- as.numeric(levels(mydf$w4))[mydf$w4]

# reshape the data to wide format
library(tidyr)
mydf2 <- gather(data = mydf, key = wave, value = frequency, - volfr)

# change the name of the waves
levels(mydf2$wave) <- c("Wave 2", "Wave 4")

# add a column showing frequencies as totals
mydf2$perc <- (mydf2$frequency/1921)

# make the bar graph
library(ggplot2)
library(scales)
ggplot(mydf2, aes(x = volfr, y = perc, fill = wave)) + geom_bar(stat = "identity", position = position_dodge()) + scale_y_continuous(labels = percent, breaks = seq(0, 0.5, 0.1), limits = (c(0,0.5)))  + xlab("") + ylab("") + scale_x_discrete(limits = c("Never/almost never", "Once a year or less", "Several times a year", "At least once a month", "At least once a week", "Most days")) + scale_fill_discrete(name = "") + theme(legend.position = "bottom")

# save the plot
# ggsave("C:/Users/Meenakshi/Dropbox/Meena's work/The State of Social Capital in Britain/volunteering_frequency_graph.png", width = 12, height = 8, dpi = 300)

### END ###