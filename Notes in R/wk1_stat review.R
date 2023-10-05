###################################################################################################################
####################################### RSCH 8120: Week 1 Review and R ############################################
###################################################################################################################
####----------------------------------####
# Date: Sep, 2023
#clean environment
rm(list=ls(all=TRUE))
#Remove scientific notation
options(scipen=999)


####--------------------------------------------------------------------####
### Set-up
# Install and load the psych package then try 
install.packages("psych")
library(psych)

# Set your working directory (folder on your computer for R to use)
# MENU: Session -> Set Working Directory ->Choose Directory-> Pick Folder (files will be invisible)
# Window (bottom right): Files tab->Select Folder->Gear->Set as Working Directory

# Get data!
# Importing a .csv file
# 1) For ease make sure the file is in your Working Directory (with your script and other R files).
# 2) Import the data and save as an object
eclsk <- read.csv("eclsk.csv") #NOTE: it must be in quotes and have .csv
# Don't forget to set working directory

## Classify categorical variables in R:
eclsk$parent.ed.cat<-as.factor(eclsk$parent.ed.cat)
eclsk$income.cat<-as.factor(eclsk$income.cat)


####--------------------------------------------------------------------####
### Part I: Statistical Review

###------------###
## Inspect the data---
eclsk

##What's here? What kinds of variables?
names(eclsk) ## the name of the variable
str(eclsk)  ## structure of the data: data type(int/num)
summary(eclsk) ## decribe: min, max, median, mean, quarter

###------------###
## Graphical Representations of Data---
plot(eclsk$parent.ed.cat) # non-numerical
plot(eclsk$math,eclsk$read) ## scatter plot without the linearity line.

hist(eclsk$math) ## histogram
hist(eclsk$income)
hist(eclsk$gen)

boxplot(eclsk$math,eclsk$read,eclsk$gen) ## three boxplots 
boxplot(eclsk$income) # boxplot
boxplot(eclsk$parent.ed.cat) # Problem? # non-numerical

###------------###
##Data Analysis---
# Descriptive statistics summary() or the describe() function in the psych package
summary(eclsk)

# Descriptive statistics describe() SD, range, etc.
describe(eclsk)

###------------###
#Correlation (bivariate)

# Pearson the relationship between two continuous variable.
cor(eclsk$math,eclsk$read)


###------------###
# Hypothesis Testing Example with t-tests
?t.test 
###ONE SAMPLE:
#Are math scores different than 40 in the population:compare a specific number with the mean of a group.
t.test(eclsk$math, mu=40) #mu:mean

###TWO SAMPLE:
#t.test(<dependent variable/outcome>~<group>, ...)

#Are the math scores different in public and non-public schools?
t.test(math~public, data=eclsk)  
## t.test to see the difference in the mean of the two groups (y = math，x = public/non-public)

###------------###
#ANOVA (x, y - continuous variable)
#Let's create a group variable: low, med, or high age:  Age ranges from 57-79:
eclsk$age.cat <- "b - average"
eclsk$age.cat[which(eclsk$age >= 72)] <- "c - older"   #72 months or older
eclsk$age.cat[which(eclsk$age <= 66)] <- "a - younger" #66 months or younger

eclsk$age.cat <- as.factor(eclsk$age.cat)

#What do math scores look like across age groups?
boxplot(math~age.cat, data=eclsk)

#Test differences in math scores across age groups?
anova(lm(math~age.cat, data=eclsk))


###------------###
# Linear Regression: predict OR explain the dependent variable ~ independent variable relationship

# How well does parental income predict math scores?
lin.model <- lm (math~income,eclsk)
# The return doesn't print much but we have an object of class lm; summary() provides a better picture
lin.model
summary(lin.model)


