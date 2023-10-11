##################################################################################################################
####################################### RSCH 8120: Week 4 Factorial ANOVA #########################
##################################################################################################################
####----------------------------------####
# Date: Aug, 2019

#clean environment
# rm(list=ls(all=TRUE))
## Factorial ANOVA is for two-way interaction


####--------------------------------------------------------------------####
### Part I: Set-up...run this before starting other parts
#Packages

# If you are missing any packages use   install.packages("fill in this blank with package name")
# Importing a .csv file
# 1) For ease make sure the file is in your Working Directory (with your script and other R files).
# 2) Import the data and save as an object
eclsk <- read.csv("eclsk.csv") #NOTE: it must be in quotes and have .csv

# ANOVA/ANCOVA requires categorical variables: run the code below to create one
eclsk$age.cat <- "b - average"
eclsk$age.cat[which(eclsk$age >= 72)] <- "c - older"   #72 months or older
eclsk$age.cat[which(eclsk$age <= 66)] <- "a - younger" #66 months or younger

eclsk$age.cat <- as.factor(eclsk$age.cat)
#now we have an age categorical variable to analyze.
eclsk$income.cat <- as.factor(eclsk$income.cat)

#This gets rid of scientific notation if you don't like it.
options(scipen=999)

####--------------------------------------------------------------------####
### Part II: Before Factorial ANOVA...seen this before

## Inspect Variables
# Examine factor/independent variable X that must be categorical
plot(eclsk$age.cat)

# Examine dependent variable Y that must be continuous by our age.cat factor
boxplot(math~age.cat, data=eclsk)
# Does it look like these groups differ on math scores? 
# There is a difference in the mean of math scores across age cat.

## Inspect Variables
# Examine factor/independent variable X that must be categorical
plot(eclsk$income.cat)

# Examine dependent variable Y that must be continuous by our income.cat factor
boxplot(math~income.cat, data=eclsk)
# Does it look like these groups differ on math scores? 
# There is a difference in the mean of math scores across income.cat.

####--------------------------------------------------------------------####
### Part III: Conduct Factorial ANOVA and interpret results

#factorial anova:
#anova(lm(<outcome>~<grouping variable that is a factor>, data=<data>)) *****NEW +age.cat:income.cat***** 
factorial.anova.output <- anova(lm(math~age.cat+income.cat+age.cat:income.cat, data=eclsk))
factorial.anova.output
## We use: to find interaction.

interaction.plot(x.factor = eclsk$age.cat, trace.factor = eclsk$income.cat, 
                 response = eclsk$math, fun = mean)
## interaction plot 
## We have plot for the moderator. From the plot,we can tell that from younger group to average age group, there's no interaction. However, from the average group to the older age group, as the age increase, the impact of income on math scores become more evident.

#factorial anova:
#anova(lm(<outcome>~<grouping variable that is a factor>, data=<data>)) *****NEW +age.cat:income.cat***** 
factorial.anova.output2 <- anova(lm(math~minority+suburban+minority:suburban, data=eclsk))
factorial.anova.output2


interaction.plot(x.factor = eclsk$minority, trace.factor = eclsk$suburban, 
                 response = eclsk$math, fun = mean)
## From the p-value，if_minority has an obvious effect on the math score, bur the location of school has no effect
## For minority, school in suburban will make math scores better; but for non-minority, school in urban will be better for math scores.
