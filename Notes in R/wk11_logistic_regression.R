##################################################################################################################
####################################### RSCH 8120:Regression ############################################
##################################################################################################################
####----------------------------------####

#### The original codes are from Dr. Kyle Cox

#clean environment
# rm(list=ls(all=TRUE))

### Part Ⅰ: Set up ###
install.packages("pscl")
install.packages("ResourceSelection")

library(pscl)
library(car)
library(ggplot2)
library(GGally)
library(tidyverse)
library(ResourceSelection)
library(psych)
library(foreign)
library(lattice)
library(MASS)

##This gets rid of scientific notation if you don't like it.
options(scipen=999, digits=4)

##Set the working session and Import the file##
eclsk <- read.csv("eclsk.csv")
eclsk$public <- as.factor(eclsk$public)

### Part II: Before model fitting ###

# Check outcome variable: Which are dichotomous?
str(eclsk)

# Example of problematic regression
with(eclsk,plot(income,public))
abline(lm(public~income, data = eclsk))

# QQ plot for typical regression 
lin.model <-lm(public~income, data = eclsk)
qqnorm(rstandard(lin.model)) 

# Scatter plot
plot(eclsk$income,eclsk$public)

# studentized residuals vs fitted values (independence, heterogeneity of variance, ouliers)
studentized.residuals <- rstudent(lin.model)
fitted.values <- fitted(lin.model)
plot(fitted.values, studentized.residuals)

### Part III: Fit and interpret a logistic regression model

# Logistic regression:
#Use glm instead of lm (generalized) linear model
#AND tell R it is logistic regression with  family=binomial(logit)
model.log <- glm(public~attend,data=eclsk,family=binomial(logit))
# Our better way to view results
summary(model.log)

### Interpretation
# For every one unit (%) increase in attendance rate, 
# the predicted log odds of being in a public school go down -0.23
# What does that even mean?!?!

# As odds decrease from 1 to 0, the logit/log odds of Y is negative and decreasing
# Negative log odds suggests as attendance rate increases the odds of being in a public school decrease
# Opposite but equal view: As attendance rate decreases the odds of being in a public school increase

## Improve interpretation by converting coefficient in log odds to coefficient in odds
exp(summary(model.log)$coefficients[,"Estimate"]) 
#Recall that the outcome is in log odds / logit.  The coefficient for attend 
#can be interpreted as "for every one % increase in attendance rate", the odds
#of it being a public school increase by exp(-.2386) = 0.788 times...22% reduction in odds 
#(so they are reduced)

###-----------------
### Logistic regression can be extended to include multiple indicators/independent variables
# Parallels multiple regression
model.log2 <- glm(public~attend+parent.ed,data=eclsk,family=binomial(logit))
model.log2
# Our better way to view results
summary(model.log2)
exp(summary(model.log2)$coefficients[,"Estimate"]) 
# Class discussion: How do we interpret this: For every one 1% increase in attendance rate, the odds of it being a public school increase by 0.8103 times. So 19% reduction in odds.
# For the parent education, similarly, 24% reduction in odds for children being in public school.

### Part IV: Compare Models

## Logistic regression estimated differently (maximum likelihood) os new model comparison methods

# 1. Test significance of change in log likelihood (deviance = -2*LL)
# Use summary and look at residual deviance (lower is better, more explained...less residual)
summary(model.log)
summary(model.log2)
#or
anova(model.log,model.log2)

# Use chi-square statistic to determine if models significantly differ: output is p-value
pchisq(model.log$deviance - model.log2$deviance, 
       model.log$df.residual - model.log2$df.residual, lower.tail = FALSE) ## very significant
#Null deviance- goodness of fit when response only predicted using empty model (mean)
#Residual deviance-goodness of fit with model under consideration

# Hosmer and Lemeshow goodness of fit test
hl <- hoslem.test(model.log2$y,model.log2$fitted.values)
cbind(hl$observed,hl$expected)


# 2. Pseudo R-squareds 
# use pscl package
pR2(model.log)
pR2(model.log2)

# What are all these things?!?!
?pR2

# One example: 
# McFadden's: The log likelihood of the intercept model is treated as a total sum of squares, 
# and the log likelihood of the full model is treated as the sum of squared errors.
# The ratio of the likelihoods suggests the level of improvement over the intercept model offered by the full model.
# A likelihood falls between 0 and 1, so the log of a likelihood is less than or equal to zero.
# If a model has a very low likelihood, then the log of the likelihood will have a larger magnitude than the log of a more likely model.
# Thus, a small ratio of log likelihoods indicates that the full model is a far better fit than the intercept model.
# If comparing two models on the same data, McFadden's would be higher for the model with the greater likelihood.

# 3. Check information criterion (AIC) and (BIC)- Lower is better
AIC(model.log,model.log2)
BIC(model.log,model.log2)


####--------------------------------------------------------------------####
### Part V: Check Assumptions

## Multicollinearity...just use same model with traditional regression
model.test <- lm(public~attend+parent.ed,data=eclsk)
vif(model.test) # variance inflation factors 
sqrt(vif(model.test)) > 2 # problem?

##Check for problems with zero-count cells
xtabs(~public+parent.ed, data=eclsk)
