##################################################################################################################
####################################### RSCH 8120: Week 3 Analysis of Covariance (ANCOVA)#########################
##################################################################################################################
####----------------------------------####
# Date: Sept, 2023

#clean environment
# rm(list=ls(all=TRUE))
#This gets rid of scientific notation if you don't like it.
options(scipen=999)


####--------------------------------------------------------------------####
### Part I: Set-up...run this before starting other parts
#Packages
install.packages("foreign")
library(foreign)


# If you are missing any packages use   install.packages("fill in this blank with package name")
# Importing a .csv file
# 1) For ease make sure the file is in your Working Directory (with your script and other R files).
# 2) Import the data and save as an object
eclsk <- read.csv("eclsk.csv") #NOTE: it must be in quotes and have .csv
diagram <-read.spss("diagram.sav", to.data.frame=TRUE)


# ANOVA/ANCOVA requires categorical variables: run the code below to create one
eclsk$age.cat <- "b - average"
eclsk$age.cat[which(eclsk$age >= 72)] <- "c - older"   #72 months or older
eclsk$age.cat[which(eclsk$age <= 66)] <- "a - younger" #66 months or younger

eclsk$age.cat <- as.factor(eclsk$age.cat)
#now we have an age categorical variable to analyze.  

#This gets rid of scientific notation if you don't like it.
options(scipen=999)

####--------------------------------------------------------------------####
### ANOVA review
# Examine dependent variable Y that must be continuous by our age.cat factor
boxplot(math~age.cat, data=eclsk)
# Does it look like these groups differ on math scores?

# Actual ANOVA
#anova(lm(<outcome>~<grouping variable that is a factor>, data=<data>))
anova(lm(math~age.cat, data=eclsk))


####--------------------------------------------------------------------####
### Before ANOVA/ANCOVA

## Inspect Variables (p. 477 and p. 508 Example 13.7)- four diagram conditions with
# text; outcome is quality of description
# Examine factor/independent variable X that must be categorical
plot(diagram$group)
# You should see 4 groups with 10 students in each
# It's continuous variable, but plot can only give you ten variables here.

# Examine dependent variable Y that must be continuous across the groups
boxplot(descript~group, data=diagram)
# Does it look like these groups differ on description score (descript)?



####--------------------------------------------------------------------####
### Part III: Conduct ANOVA/ANCOVA and interpret results

#anova:
#anova(lm(<outcome>~<grouping variable that is a factor>, data=<data>))
anova.output <- anova(lm(descript~group, data=diagram))
anova.output
#Notice that the 'total' isn't provided.  That is because it is redundant.  Just add.
#"Residuals" and "Error" are the same thing.


#ancova:  now we add a covariate 'time'; it must go first
ancova.output <- anova(lm(descript~time+group, data=diagram))
ancova.output

## The significance of the relationship between group and descript becomes stronger.

### Even with a non-significant predictor the use of a covariate increases power
# Note the p-value is much lower, reduced residual, increased between group MS

####----------------- eclsk example

boxplot(read~income.cat, data=eclsk)
#anova:
#anova(lm(<outcome>~<grouping variable that is a factor>, data=<data>))
anova.output2 <- anova(lm(read~income.cat, data=eclsk))
anova.output2


#ancova:  now we add a covariate 'parent.ed'; it must go first
ancova.output2 <- anova(lm(read~parent.ed+income.cat, data=eclsk))
ancova.output2

## After controlling parent.ed, the relationship between income and reading scores become non-significant
####--------------------------------------------------------------------####
