# Title: Preparing an OR plot for volunteering frequency
# Author: Meenakshi Parameshwaran
# Date: 09/11/15

# This script uses OR coefficient and standard error estimates from a random effects Stata model on volunteering frequency (taking from social_capital_analysis.do). Full model output:


# Random-effects ordered logistic regression      Number of obs      =      3842
# Group variable: kidid                           Number of groups   =      1921
# 
# Random effects u_i ~ Gaussian                   Obs per group: min =         2
# avg =       2.0
# max =         2
# 
# Integration method: mvaghermite                 Integration points =        12
# 
# Wald chi2(10)      =    280.29
# Log likelihood  = -5480.9078                    Prob > chi2        =    0.0000
# 
# ---------------------------------------------------------------------------------
#     ypvolfreq | Odds Ratio   Std. Err.      z    P>|z|     [95% Conf. Interval]
# ----------------+----------------------------------------------------------------
#     ypdvage |   1.044333   .0246073     1.84   0.066     .9972006    1.093694
# rural |   1.119229   .0938004     1.34   0.179     .9496887    1.319035
# ypbothpar |    1.01024    .091069     0.11   0.910     .8466272    1.205471
# ypsrhlth |   1.084368   .0477002     1.84   0.066     .9947941    1.182007
# rosenberg |   .9934524   .0112049    -0.58   0.560     .9717323    1.015658
# friends20 |   1.023149   .0080677     2.90   0.004     1.007459    1.039085
# religious_class |   2.081954   .2846493     5.36   0.000     1.592549    2.721756
# cultural |   1.713555   .0664281    13.89   0.000     1.588182    1.848826
# ypclass |   .9898003   .0199543    -0.51   0.611     .9514532    1.029693
# ypfemale |   1.460088   .1094943     5.05   0.000      1.26051    1.691267
# ----------------+----------------------------------------------------------------
#     /cut1 |   .7980064    .450568     1.77   0.077    -.0850907    1.681103
# /cut2 |   1.894024   .4522147     4.19   0.000     1.007699    2.780348
# /cut3 |   2.909289   .4549838     6.39   0.000     2.017537    3.801041
# /cut4 |   3.684885   .4579224     8.05   0.000     2.787373    4.582396
# /cut5 |   5.033478   .4664061    10.79   0.000     4.119339    5.947617
# ----------------+----------------------------------------------------------------
#     /sigma2_u |   .7414365   .1259004                      .5315385    1.034221
# ---------------------------------------------------------------------------------
#     LR test vs. ologit regression:   chibar2(01) =    54.32 Prob>=chibar2 = 0.0000


# Prepare dataframe with values -------------------------------------------

# this is just a copy paste from the model above, except I have renamed the variables

var <- c("Age", "Rural", "Lives with both parents", "Self-reported health", "Rosenberg scale score", "Number of close friends", "Attends religious classes", "Cultural capital", "Social class", "Female")
or <- c(1.044333, 1.119229, 1.01024, 1.084368, .9934524, 1.023149, 2.081954, 1.713555, .9898003, 1.460088)
se <- c(.0246073, .0938004, .091069, .0477002, .0112049, .0080677, .2846493, .0664281, .0199543, .0199543)
sig <- c("Insignificant", "Insignificant", "Insignificant", "Insignificant", "Insignificant", "Significant", "Significant", "Significant", "Insignificant", "Significant")
ub <- or + (1.96*se)
lb <- or - (1.96*se)

# combine these into a dataframe
mydf <- data.frame(var, or, se, sig, ub, lb)

# check the datatypes
str(mydf) # ok everything in the correct data type

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
    scale_x_discrete("Youth characteristics", limits = c("Social class", "Rosenberg scale score","Lives with both parents", "Number of close friends", "Age", "Self-reported health", "Rural", "Female", "Cultural capital", "Attends religious classes")) + 
    scale_y_continuous("Odds-ratio", limits = c(0.8, 2.8), breaks = c(0.8, 1, 1.2, 1.4, 1.6, 1.8, 2.0, 2.2, 2.4, 2.6, 2.8)) + 
    coord_flip() + 
    guides(size = FALSE)

# ggsave("C:/Users/Meenakshi/Dropbox/Meena's work/The State of Social Capital in Britain/OR_graph.png", width = 12, height = 8, dpi = 300)

### END ###
