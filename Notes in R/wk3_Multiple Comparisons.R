##################################################################################################################
####################################### RSCH 8120: Multiple Comparison Procedures ########################
##################################################################################################################
####----------------------------------####
# Date: Sept, 2019

#clean environment
# rm(list=ls(all=TRUE))


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

#This gets rid of scientific notation if you don't like it.
options(scipen=999)

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

anova.output <- anova(lm(math~age.cat, data=eclsk))
anova.output
#Now, which age.categories are different from which others??

####--------------------------------------------------------------------####
### Part IV: Follow-up ANOVA and interpret results
# OR
### Consider possible planned comparisons

# P value adjustment
pairwise.t.test(eclsk$math, eclsk$age.cat, p.adj = "none")
pairwise.t.test(eclsk$math, eclsk$age.cat, p.adj = "bonf") # post hoc ??/C
pairwise.t.test(eclsk$math, eclsk$age.cat, p.adj = "holm") # planned ??/C-1

# Other methods
aov.output <- aov(math~age.cat, data=eclsk)
TukeyHSD(aov.output)
 
# Other functions
install.packages("DescTools")
library(DescTools)
PostHocTest(aov.output,method = "scheffe")
# methods= "hsd", "bonferroni", "lsd", "scheffe", "newmankeuls", "duncan" 



# ANOTHER EXAMPLE
####--------------------------------------------------------------------####
### Part III: Conduct ANOVA and interpret results
eclsk$parent.ed<-as.factor(eclsk$parent.ed)

anova.output2 <- anova(lm(math~parent.ed, data=eclsk))
anova.output2
#Now, which age.categories are different from which others??

####--------------------------------------------------------------------####
### Part IV: Follow-up ANOVA and interpret results
# OR
### Consider possible planned comparisons
# Examine dependent variable Y that must be continuous by our age.cat factor
boxplot(math~parent.ed, data=eclsk)
# Does it look like these groups differ on math scores?

# P value adjustment
pairwise.t.test(eclsk$math, eclsk$parent.ed, p.adj = "none")
pairwise.t.test(eclsk$math, eclsk$parent.ed, p.adj = "bonf") # post hoc ??/C
pairwise.t.test(eclsk$math, eclsk$parent.ed, p.adj = "holm") # planned ??/C-1

# Other methods
eclsk$parent.ed <-as.factor(eclsk$parent.ed)
aov.output2 <- aov(math~parent.ed, data=eclsk)
TukeyHSD(aov.output2)

# Other functions
PostHocTest(aov.output2,method = "scheffe")
# methods= "hsd", "bonferroni", "lsd", "scheffe", "newmankeuls", "duncan" 
