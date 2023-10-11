##################################################################################################################
####################################### RSCH 8120: Week 2 Analysis of Variance (ANOVA)############################
##################################################################################################################
####----------------------------------####
# Date:Aug, 2023

#clean environment
# rm(list=ls(all=TRUE))


####--------------------------------------------------------------------#### RUN THIS CODE
### Part I: Set-up...run this before starting other parts

# Importing a .csv file
# 1) For ease make sure the file is in your Working Directory (with your script and other R files).
# 2) Import the data and save as an object
eclsk <- read.csv("eclsk.csv") #NOTE: it must be in quotes and have .csv

# ANOVA requires categorical variables: run the code below to create one
eclsk$age.cat <- "b - average"
eclsk$age.cat[which(eclsk$age >= 72)] <- "c - older"   #72 months or older
eclsk$age.cat[which(eclsk$age <= 66)] <- "a - younger" #66 months or younger

eclsk$age.cat <- as.factor(eclsk$age.cat)
#now we have an age categorical variable to analyze.

#This gets rid of scientific notation if you don't like it.
options(scipen=999)
####--------------------------------------------------------------------#### STOP HERE


####--------------------------------------------------------------------#### 
### Part II: Before ANOVA

## Inspect Variables
# Examine factor/independent variable X that must be categorical
plot(eclsk$age.cat)

# Examine dependent variable Y that must be continuous by our age.cat factor
boxplot(math~age.cat, data=eclsk)
# Does it look like these groups differ on math scores?



####--------------------------------------------------------------------####
### Part III: Conduct ANOVA and interpret results

### THere are many ways to run an anova in R
#anova as a special linear model:
#anova(lm(<outcome>~<grouping variable that is a factor>, data=<data>))
anova(lm(math~age.cat, data=eclsk))
## As we can tell, there is a lm. So ANOVA is a special regression.
## F value (81.606)， p(< 2.2e-16)

