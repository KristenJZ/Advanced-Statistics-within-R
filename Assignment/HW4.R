##install.packages()

library(psych)
library(foreign)
library(lattice)
library(ggplot2)
library(GGally)
library(MASS)
library(car)

##import csv and clean the scientific notation

eclsk <- read.csv("eclsk.csv")
options(scipen=999)

##Question 1
# Check for relationships
cor(eclsk[,c("read", "math", "gen","age","income","attend","free.lunch")])

# Graphical check for relationships:
ggpairs(eclsk[,c("read", "math", "gen","age","income","attend","free.lunch")])

## Question 2
model.multiple <- lm(read~math+gen+income, data=eclsk)
summary(model.multiple)

## Question 3
model2 <- lm(read~math+gen+income+female, data = eclsk)
summary(model2)
anova(model.multiple,model2)

