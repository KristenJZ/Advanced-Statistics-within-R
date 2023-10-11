##################################################################################################################
####################################### RSCH 8120: Effect Size and Power ##################################
##################################################################################################################
####----------------------------------####
# Date: September, 2013

#clean environment
# rm(list=ls(all=TRUE))



####--------------------------------------------------------------------####
### Part I: Set-up...run this before starting other parts
# No data this time...power analyses are inherently a priori or before data collection
install.packages("PowerUpR")
library(PowerUpR)

#This gets rid of scientific notation if you don't like it.
options(scipen=999)

####--------------------------------------------------------------------####
# Statistical power, minimum detectable effect size (MDES), MDES difference (MDESD), 
# or minimum required sample size (MRSS) 
# can be requested by using the relevant function given design parameters. 
# Each function begins with an output name, follows by a period, and ends with a 
# design name in the form <output>.<design>(). There are three types of output; 
# mdes for main effects (mdes or mdesd for moderation effects), power, and mrss. 
# Each output can be requested for fourteen types of designs to detect main treatment effects;
# ira1r1, bira2r1, bira2f1, bira2c1, cra2r2, bira3r1, bcra3r2, bcra3f2, cra3r3, 
# bira4r1, bcra4r2, bcra4r3, bcra4f3, cra4r4, and five types of designs to detect 
# moderator effects; mod221, mod222, mod331, mod332, and mod333. To detect mediator effects, 
# only power can be requested for two types of designs; med211 and med221.

# For designs to detect main effects, first three letters stands for the type of assignment;
# for individual random assignment ira, for blocked individual random assignment bira, 
# for cluster random assignment cra, and for blocked cluster random assignment bcra. 
# Numbers indicate total number of levels and the level at which randomization takes 
# place correspondingly. The single letter inbetween refers to whether the top level 
# is random or fixed. Naming conventions are slighlty different for designs to 
# detect moderator and mediator effects. Numbers following mod keyword indicate total
# number of levels, the level at which randomization takes place, and the level at which
# the moderator resides correspondingly. As for the mediator effects, numbers following
# med keyword indicate the level at which path a, b and cp resides.

# For example, the function mdes.cra2r2() can be called to calculate MDES for main treatment
# effect in a two-level cluster-randomized trial. Similiarly, the function
# mdesd.mod222() can be called to calculate MDESD for moderator effect that
# resides at level 2 in a two-level cluster-randomized trial. Finally, the function
# power.med221() can be called to calculate statistical power for mediator effect
# that resides at level 2 in a two-level cluster-randomized trial.



####--------------------------------------------------------------------####
### Part II: Apply

###------------### PowerUPR

# Power for a main effect when the study uses individual random assignment
# es = effect size.  
# alpha = probability of type I error
# two.tailed logical; TRUE for two-tailed hypothesis testing, FALSE for one-tailed hypothesis testing.
# p = proportion of units randomly assigned to treatment. 0.5意思是一个 balanced design
# g1 = number of covariates.
# r21 = proportion of variance in the outcome explained by covariates.
# n = sample size.

power.ira1r1(es=.25, alpha=.05, two.tailed=TRUE,p=.50, g1=0, r21=0, n=50)

# Power is low: How does sample size (n) influence power?  
power.ira1r1(es=.25, alpha=.05, two.tailed=TRUE,p=.50, g1=0, r21=0, n=100)
power.ira1r1(es=.25, alpha=.05, two.tailed=TRUE,p=.50, g1=0, r21=0, n=200)
power.ira1r1(es=.25, alpha=.05, two.tailed=TRUE,p=.50, g1=0, r21=0, n=500)
power.ira1r1(es=.25, alpha=.05, two.tailed=TRUE,p=.50, g1=0, r21=0, n=1000)

# How does effect size (es) influence power?
power.ira1r1(es=.25, alpha=.05, two.tailed=TRUE,p=.50, g1=0, r21=0, n=50)
power.ira1r1(es=.5, alpha=.05, two.tailed=TRUE,p=.50, g1=0, r21=0, n=50)
power.ira1r1(es=.8, alpha=.05, two.tailed=TRUE,p=.50, g1=0, r21=0, n=50)

# How does including a covariate that explains variance in the outcome influence power?
power.ira1r1(es=.25, alpha=.05, two.tailed=TRUE,p=.50, g1=1, r21=.25, n=50)
power.ira1r1(es=.25, alpha=.05, two.tailed=TRUE,p=.50, g1=1, r21=.50, n=50)
power.ira1r1(es=.25, alpha=.05, two.tailed=TRUE,p=.50, g1=1, r21=.75, n=50)







