##################################################################################################################
####################################### RSCH 8120:Regression ############################################
##################################################################################################################
####----------------------------------####
# Date: November, 2023
# clean environment
# rm(list=ls(all=TRUE))

### Part I: Set-up...run this before starting other parts
#Packages
#install.packages()

library(psych)
library(foreign)
library(lattice)
library(ggplot2)
library(GGally)
library(MASS)
library(car)

# Importing a .csv file
# 1) For ease make sure the file is in your Working Directory (with your script and other R files).
# 2) Import the data and save as an object
eclsk <- read.csv("eclsk.csv") #NOTE: it must be in quotes and have .csv
hayden2005 <-read.spss("hayden_2005.sav", to.data.frame=TRUE)

##This gets rid of scientific notation if you don't like it.
options(scipen=999)

####--------------------------------------------------------------------####
### Part II: Before model fitting

# Check for relationships
cor(eclsk[,c("math", "read", "age")])

# Graphical check for relationships:
splom(~eclsk[,c("math", "read", "age")])
ggpairs(eclsk[,c("math", "read", "income","age")])

####--------------------------------------------------------------------####
### Part III: Fit a multiple regression model

# We use the same lm() function.  lm() is a general function for linear models
# lm(outcome~predictor1+predictor2)

## Simple linear regression
model.simple <- lm(math~read, data=eclsk) 
model.simple
# Better view of results
summary(model.simple)

##We could ask, what is the relationship between math and reading while
#controlling for student age (in months):
model.multiple <- lm(math~read+age, data=eclsk) #age and reading

# Better view of results
summary (model.multiple)

####--------------------------------------------------------------------####
### Part IV: Compare Models

## Whis is the better model? Does including age matter?
anova(model.simple, model.multiple)
# testing reduction in residual sum of squares

# model.multiple is significantly different and has a larger r^2

####------------------------------------------------####
### Part V: Moderator and mediation effects ###
#Moderator Effect
model.int <- lm(math~read+age+read:age, data = eclsk)
summary(model.int)
plot(model.int)

#Mediator Effect
model.mediator <- lm(read~age, data =eclsk)
model.outcome <- lm(math~read+age, data=eclsk)
library(stargazer)
stargazer(model.mediator, model.outcome, type = "text", title = "Mediation Effects")

####--------------------------------------------------------------------####
### Part VI: Check Assumptions
# Recall the key assumptions with regression involve the residuals (error, leftovers from prediction)

###Independence and homogeneity of variance (homoscedasticity)
#plot studentized residuals vs. fitted values
#     (type of standardized residual) vs. predicted outcome value
studentized.residuals <- rstudent(model.multiple)
fitted.values <- fitted(model.multiple)
plot(fitted.values, studentized.residuals)
abline(h=0)
# *** Studentized residuals also help identify outliers (generally fall bewteen -4 and +4)

#or you can use the spreadLevelPlot
spreadLevelPlot(model.multiple)

#Either way, you are looking for values that are relatively evenly spread across X
# patterns suggest lack of independence, linearity, and/or heterogeneity of variance  

###Normality 
#QQplot:
qqnorm(studentized.residuals)
#OR
par(mfrow=c(1,1))
qqPlot(model.multiple)
#Data should roughly follow the line throughout for normality
#Can also check residuals for approximate normality:
hist(studentized.residuals)

# Evaluate Nonlinearity
# component + residual plot 
crPlots(model.multiple)
#looking for linear relationships


## Multicollinearity
vif(model.multiple) # variance inflation factors 
sqrt(vif(model.multiple)) > 2 # problem?


##Issues above suggest a nonlinear relationship between reading and math score.
plot(eclsk$read, eclsk$math)

# Fix? Better model? is the quadratic term a significant predictor?
model.multiple2 <- lm(math~read+age+I(read^2), data=eclsk) #age and reading
# Better view of results
summary (model.multiple2)
# Compare
anova(model.multiple,model.multiple2)
plot(model.multiple2)


