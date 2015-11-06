# Title: Preparing an OR plot for volunteering frequency
# Author: Meenakshi Parameshwaran
# Date: 06/11/15

# This script uses OR coefficient and standard error estimates from a random effects Stata model on volunteering frequency (taking from social_capital_analysis.do)

# Full model output below


# Random-effects ordered logistic regression      Number of obs      =      3842
# Group variable: kidid                           Number of groups   =      1921
# 
# Random effects u_i ~ Gaussian                   Obs per group: min =         2
# avg =       2.0
# max =         2
# 
# Integration method: mvaghermite                 Integration points =        12
# 
# Wald chi2(8)       =    257.79
# Log likelihood  = -5493.8484                    Prob > chi2        =    0.0000
# 
# ---------------------------------------------------------------------------------
#     ypvolfreq | Odds Ratio   Std. Err.      z    P>|z|     [95% Conf. Interval]
# ----------------+----------------------------------------------------------------
#     ypdvage |   1.047174   .0247149     1.95   0.051     .9998374    1.096753
# rural |   1.101814   .0928509     1.15   0.250     .9340641     1.29969
# ypbothpar |   1.019053   .0762776     0.25   0.801     .8800013    1.180078
# ypsrhlth |   1.077129   .0474582     1.69   0.092     .9880159     1.17428
# rosenberg |   .9879482   .0111333    -1.08   0.282     .9663666    1.010012
# friends20 |     1.0222   .0080822     2.78   0.005     1.006481    1.038164
# religious_class |   2.109361   .2886818     5.45   0.000     1.613086    2.758317
# cultural |   1.738193    .067235    14.29   0.000     1.611287    1.875096
# ----------------+----------------------------------------------------------------
#     /cut1 |   .5278799   .4269552     1.24   0.216    -.3089368    1.364697
# /cut2 |   1.623673   .4283417     3.79   0.000     .7841386    2.463207
# /cut3 |    2.63759   .4309388     6.12   0.000     1.792965    3.482214
# /cut4 |   3.411709   .4338406     7.86   0.000     2.561397    4.262021
# /cut5 |   4.758612   .4425138    10.75   0.000     3.891301    5.625923
# ----------------+----------------------------------------------------------------
#     /sigma2_u |   .7727427   .1281506                      .5583067     1.06954
# ---------------------------------------------------------------------------------
#     LR test vs. ologit regression:   chibar2(01) =    57.48 Prob>=chibar2 = 0.0000


# Prepare dataframe with values -------------------------------------------

# this is just a copy paste from the model above, except I have renamed the variables

var <- c("Age", "Rural", "Lives with both parents", "Self-reported health", "Rosenberg scale score", "Number of close friends", "Attends religious classes", "Cultural capital")
or <- c(1.047174, 1.101814, 1.019053, 1.077129, .9879482, 1.0222, 2.109361, 1.738193)
se <- c(.0247149, .0928509, .0762776, .0474582, .0111333, .0080822, .2886818, .067235)
sig <- c("Insignificant", "Insignificant", "Insignificant", "Insignificant", "Insignificant", "Significant", "Significant", "Significant")
ub <- or + (1.96*se)
lb <- or - (1.96*se)

# combine these into a dataframe
mydf <- data.frame(cbind(var, or, se, sig, ub, lb))

# check the datatypes
str(mydf) # ok everything is a factor, when most should be numeric

# convert numeric variables back to numbers - here's a short function to do this
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}

# call the function on the target vars
mydf <- data.frame(mydf[c(1,4)], lapply(mydf[c(2,3,5,6)], as.numeric.factor))

# check the data types have changed
str(mydf)

# or could do - lapply(mydf, function(x) class(x))

# order by size of effect
mydf <- mydf[order(-or),] # minus sign in front of OR orders mydf in decreasing order

# make the plot
library(ggplot2)

ggplot(mydf, aes(x = var, y = or)) + 
    geom_point(aes(colour = sig), size = 4) +
    geom_errorbar(aes(ymin = lb, ymax = ub), width = 0.4, size = 0.75) + 
    geom_hline(yintercept = 1.0, colour = gray(1/2), lty = 2) + 
#     theme_classic() + 
    theme(legend.position="bottom", legend.title = element_blank()) +
    scale_x_discrete("Youth characteristics", limits = c("Rosenberg scale score","Lives with both parents", "Number of close friends", "Age", "Self-reported health", "Rural", "Cultural capital", "Attends religious classes")) + 
    scale_y_continuous("Odds-ratio", limits = c(0.8, 2.8), breaks = c(0.8, 1, 1.2, 1.4, 1.6, 1.8, 2.0, 2.2, 2.4, 2.6, 2.8)) + 
    coord_flip() + 
    guides(size = FALSE)

# ggsave("C:/Users/Meenakshi/Dropbox/Meena's work/The State of Social Capital in Britain/OR_graph.png", width = 12, height = 8, dpi = 300)

### END ###
