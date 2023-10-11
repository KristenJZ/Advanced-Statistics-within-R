#########################################################################################################
####################################### RSCH 8120: Assumptions  #########################################
#########################################################################################################
####----------------------------------####
# Date: Oct, 2023
#clean environment
# rm(list=ls(all=TRUE))

####--------------------------------------------------------------------####
### Part I: Set-up...run this before starting other parts
#Packages

library(psych)
library(foreign)
library(car)

# Importing a .csv file
# 1) For ease make sure the file is in your Working Directory (with your script and other R files).
# 2) Import the data and save as an object
eclsk <- read.csv("eclsk.csv") #NOTE: it must be in quotes and have .csv
hayden2005 <-read.spss("Hayden_2005.sav", to.data.frame=TRUE)
bp2000 <-read.spss("baguley_payne_2000.sav", to.data.frame=TRUE)

##This gets rid of scientific notation if you don't like it.
options(scipen=999)


####-------------####
### Assumptions
# We need to test model assumptions before the model building. 
# Statistical assumptions are about the world being modeled: the distribution of variables (normal distribution or not); type of relationship between variables (linear or robust); behavior of variables in different groups: type of variables.


####--------------------------------------------------------------------####
### Part I: Detect and Assess Assumptions

###------------### Tool#1: Descriptive statistics: mean, median, mode, max, min, standard deviation.
# Descriptive statistics summary() is adequate but I like the describe() function in the psych package
# Install and load the psych package then try each
# Find and understand measures of central tendency and variability
# Look for weird missing values (too many...none?)

summary(eclsk)
describe(eclsk)


###------------### Tool #2: Graphical Methods:histogram, frequency plots/bar graph, box plot, Q-Q- plot, scatter plot
#--- Histograms---
hist(eclsk$math)
hist(eclsk$math, breaks = 16)

# Frequency vs. Probability Density
hist(eclsk$math)
hist(eclsk$math, freq = FALSE, ylim = c(0,0.05))


#--- Box plots---
boxplot(eclsk$math,eclsk$read,eclsk$gen)

boxplot(eclsk$parent.ed.cat) # Problem?

#--- QQ plots --- Normality. Q-Q- plot is to compare sample distribution to theory distribution.
# First need to complete analysis (get regression model object)
lin.model <- lm (math~read,eclsk)
lin.model.residuals<-lin.model$residuals

# Basic QQplot using residuals
qqnorm(lin.model.residuals)
#Data should roughly follow the line throughout for normality
#Can also check residuals for approximate normality:
hist(lin.model.residuals,breaks = 24)

#--- Scatter Plots --- Linearity & constant variance
plot(eclsk$read,eclsk$math)

with(eclsk,plot(read, math))
abline(lm (math~read,eclsk))


#--- Scatter Plots --- Homogeneity of Variance and others 
#plot studentized residuals vs. fitted values
studentized.residuals <- rstudent(lin.model)
fitted.values <- fitted(lin.model)
plot(fitted.values, studentized.residuals)
abline(h=0)
# you are looking for values that are relatively evenly spread across fitted values
# patterns/fan shape suggest heterogeneity of variance 

## Usually for a regression model, we 1) check the normality with Q-Q- plot/Normality probability plot; 2) use scatter plot to check the linearity and constant variance.

####----For some statistical model and their assumptions----####
#1) t-test:
## Assumptions are: 1) normality distribution of quantitative variables; 2) continuity of variable; 3) homoscedasticity, that the variable of residue is the same; 4) independence.
## method: histogram (for normal distribution); weich's t-test.

#2) Pearson correlation:
## assumption: 1) continuous variables; 2) no outliers, which may reduce/inflate r; 3) normality of variables; 4) linearity;
## method: spearson's correlation; scatter plot.

#3) Simple linear regression:
## assumptions are: 1) linearity; 2) homoscedasticity; 3) independence, that the individual outcome doesn't depend on other variables. 4) normality
## method: scatter plot; Q-Q- plot.


