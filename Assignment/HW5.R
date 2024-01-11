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

##### question 1 #####
str(eclsk)

#### question 2 #####
model.log <- glm(public~attend+parent.ed+gen,data=eclsk,family=binomial(logit))
summary(model.log)
exp(summary(model.log)$coefficients[,"Estimate"]) 

#### Question 3 ####
model.log2 <- glm(public~attend+parent.ed+gen+female,data=eclsk,family=binomial(logit))
summary(model.log2)
exp(summary(model.log2)$coefficients[,"Estimate"]) 

pR2(model.log)
pR2(model.log2)
anova(model.log,model.log2)


#### Question 4 ####
model.log3 <- glm(public~attend+parent.ed+gen+female+income, data=eclsk, family=binomial(logit))
summary(model.log3)
exp(summary(model.log3)$coefficients[,"Estimate"])






