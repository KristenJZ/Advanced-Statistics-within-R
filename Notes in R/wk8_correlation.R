##################################################################################################################
####################################### RSCH 8120: Week 8 Correlation ############################################
##################################################################################################################
####----------------------------------####
# Date: Oct, 2023

#clean environment
# rm(list=ls(all=TRUE))



####--------------------------------------------------------------------####
### Part I: Set-up...run this before starting other parts
# Get data first!

# Importing a .csv file
# 1) For ease make sure the file is in your Working Directory (with your script and other R files).
# 2) Import the data and save as an object
eclsk <- read.csv("eclsk.csv") #NOTE: it must be in quotes and have .csv

# Code variables as ordinal
# use - xtfrm to code some items into ordinal variables for future use
eclsk$income.cat<-xtfrm(eclsk$income.cat)
eclsk$parent.ed.cat <-xtfrm(eclsk$parent.ed)


#This gets rid of scientific notation if you don't like it.
options(scipen=999)
####--------------------------------------------------------------------####

##Review the variables and their type
names(eclsk)
str(eclsk)
summary(eclsk)

####--------------------------------------------------------------------####
### Part II: Checks

###------------###
#Correlation (bivariate)
# Assumption checks
# For Pearson, an important assumption is that the relationship between two variables is linear, and also the two are continuous variables, and normal distribution.
# Correct variable type for correlation method (e.g., two continuous variables for Pearson)

# Linearity
# See the scatter plot and try to see the trend
plot(eclsk$math,eclsk$read)

# Normal distribution...really only important if testing significance
hist(eclsk$math)
hist(eclsk$read)
# Shapiro-Wilk normality test for mpg
shapiro.test(eclsk$math)
shapiro.test(eclsk$read) 

####--------------------------------------------------------------------####
### Part III: Correlation
# Pearson
# cor range from -1 to +1. The sign means the direction, and the cor means the magnitude.
cor(eclsk$math,eclsk$read)

# Spearman's rho 
# for two variables that are both ordinal variables.
cor(eclsk$income.cat,eclsk$parent.ed.cat,method = "spearman") 
cor(eclsk$math,eclsk$read, method = "spearman") #robust to assumption violations 
## In spearman, no assumption about linearity, therefore the result is larger than 0.68. That's because the linearity will influence the results.

# Kendall's tau (similar usage as Spearman's rho)
cor(eclsk$income.cat,eclsk$parent.ed.cat,method = "kendall")

# Test of significant relationship
# We already know the cor, but we need to see the p-value.
cor.test(eclsk$math,eclsk$read)
cor.test(eclsk$math,eclsk$read, method="kendall")

####--------------------------------------------------------------------####
### Part IV: Correlation Extensions/Other types of correlation
# Spearman rank/Spearman's rho: X-ordinal, Y-ordinal. e.g., the relationship between the income.cat and the parent.ed.cat (We already transferred them into ordinal variables)
# Point-biserial: X-dichotomous, Y-continuous. e.g., the relationship between math scores (Y) and the gender (X).
# Phi coefficient: X-dichotomous, Y-dichotomous. 
# Biserial: X-dichotomous, Y-dichotomous. e.g., the relationship between gender (X) and pass or not (Y)
# Tetrachoric: X-dichotomous, Y-dichotomous. e.g, the relationship between gender (X) and if_minority (Y)
# Cramer's V: X-categorical, Y-categorical

install.packages("wCorr")
install.packages("ltm")
library(wCorr)
library(ltm)

# Point biserial
biserial.cor(eclsk$math, eclsk$female) 
# Tetrachoric 
weightedCorr(eclsk$female,eclsk$minority, method="Polychoric")
weightedCorr(eclsk$urban,eclsk$minority, method="Polychoric")
