##################################################################################################################
####################################### RSCH 8120: Week 5 Repeated Measures ########################
##################################################################################################################
####----------------------------------####
# Date: November, 2019
# Author: Kyle Cox 
# Email: kyle.cox@uncc.edu
#clean environment
# rm(list=ls(all=TRUE))


####--------------------------------------------------------------------####
### Part I: Set-up...run this before starting other parts
#### Install the packages for repeated ANOVA
install.packages("car")
install.packages("ez")
install.packages("DescTools")
install.packages("psych")
library(car)
library(ez)
library(DescTools)
library(psych)
# 1) For ease make sure the file is in your Working Directory (with your script and other R files).
# 2) Import the data and save as an object
pride.long <- read.csv("pride_longS.csv")
pride.long$participant <- as.factor(pride.long$participant)

#This gets rid of scientific notation if you don't like it.
options(scipen=999)


####--------------------------------------------------------------------####
### New Data Inspection
str(pride.long)
summary(pride.long)


####--------------------------------------------------------------------####
### Conduct Repeated measures ANOVA and interpret results

# aov() is another function for ANOVA and we save the results as pride.anov 
pride.anov <- aov(accuracy ~ emotion + Error(participant/emotion), pride.long)
summary(pride.anov)


