##################################################################################################################
####################################### RSCH 8120:Regression ############################################
##################################################################################################################
####----------------------------------####
# Date: November, 2023
# clean environment
# rm(list=ls(all=TRUE))



####--------------------------------------------------------------------####
### Part I: Set-up...run this before starting other parts
# install.packages("psych")
library(psych)

# Get data first!
# Importing a .csv file
# 1) For ease make sure the file is in your Working Directory (with your script and other R files).
# 2) Import the data and save as an object
eclsk <- read.csv("eclsk.csv") 

#This gets rid of scientific notation if you don't like it.
options(scipen=999)

# Review the variables and their type
str(eclsk)
describe(eclsk)

####--------------------------------------------------------------------####
### Part Ⅱ: Check if the data meet the assumptions of regression ###
### Simple regression
# Linearity
plot(eclsk$math,eclsk$read)
# Normality: Shapiro-Wilk Normality test 
shapiro.test(eclsk$math)
shapiro.test(eclsk$read)

####--------------------------------------------------------------------####
### Part Ⅲ: Regression ###
# predict dependent variable ~ independent variable
# linear regreassion is a foundational method and its format and output is replicated across a number of related analysis techniques and their functions
lin.model <- lm(math~read,eclsk)
summary(lin.model) #From the coeffitients, we can tell that how much increase in y will be caused by increasing 1 point in x
# From the model evaluation, we see the multiple R-squared and adjusted R-squared. And the F-statistics, and p-value.
typeof(lin.model);str(lin.model) # Just another way to pull the results

# Only want part of results? 
lin.model$coefficients
lin.model$residuals
lin.model$fitted.values

# Scatter plot with regression line
plot(eclsk$read,eclsk$math)   #a scatterplot of X=Reading Outcome and Y=math outcome scores. plot(x,y)
abline(lin.model)             #plot the least squares regression line. lm(y~x)


####--------------------------------------------------------------------####
### Part Ⅳ: Advanced and Extra Plots###

#Diagnostic plots
plot(lin.model) # Look at the QQ plot for normality

confint(lin.model) # Get confidence intervals for slope and intercept

### More Advanced plots
install.packages("ggplot2")
library(ggplot2)

# 95% CI
ggplot(eclsk, aes(x=read, y=math))+
  geom_point()+
  geom_smooth(method=lm, se=TRUE)

# 95% CI and prediction interval
temp_var <- predict(lin.model, interval="prediction")
new_df <- cbind(eclsk, temp_var)

ggplot(new_df, aes(read, math))+
  geom_point() +
  geom_line(aes(y=lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y=upr), color = "red", linetype = "dashed")+
  geom_smooth(method=lm, se=TRUE)




