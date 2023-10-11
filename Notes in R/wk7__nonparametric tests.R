##################################################################################################################
####################################### RSCH 8120: Nonparametric tests ########################
##################################################################################################################
####----------------------------------####
# Date: Oct, 2019

#clean environment
# rm(list=ls(all=TRUE))



####--------------------------------------------------------------------####
### Part I: Set-up...run this before starting HW5
#Packages
install.packages("car")
library(car)
install.packages("MASS")
library(MASS) 

# If you are missing any packages use   install.packages("fill in this blank with package name")
# Importing a .csv file
# 1) For ease make sure the file is in your Working Directory (with your script and other R files).
# 2) Import the data and save as an object
eclsk <- read.csv("eclsk.csv") #NOTE: it must be in quotes and have .csv
TwoTowns <- read.table("http://rcompanion.org/documents/TwoTowns.csv",
                      header=TRUE, sep=",")
# We download another dataset here.


# ANOVA requires categorical variables: run the code below to create one
eclsk$age.cat <- "b - average"
eclsk$age.cat[which(eclsk$age >= 72)] <- "c - older"   #72 months or older
eclsk$age.cat[which(eclsk$age <= 66)] <- "a - younger" #66 months or younger

eclsk$age.cat <- as.factor(eclsk$age.cat)
#now we have an age categorical variable to analyze.  

#This gets rid of scientific notation if you don't like it.
options(scipen=999)

####--------------------------------------------------------------------####
# Check new data
head(immer) # data from MASS package
head(survey)
head(TwoTowns)

####Non-parametric Test------#####
##Distribution free, no need for normal distribution.
##Good for nominal/ordinal data, not only for interval/ratio data.
##Hypotheses don't involve specific parameter.
##Examples of nonparametric tests are below

####--------------------------------------------------------------------####
### Conduct nonparametric tests and interpret results

# chi square test of independence 
# chi square test of independence can be used to see whether there is a significant relationship between two categorical variables.
# Before chi square test, we need to set up a table, crossing the two categorical variables.
tbl <- table(survey$Smoke, survey$Exer) 
tbl
chisq.test(tbl) 
# The p-value > .05, so there is no significant relationship between smoke and exercise in this dataset.

# chi square goodness of fit test
# To see if the sample proportion fit the population proportion stated in the hypothesis.
tulip <- c(81, 50, 27) # We set up a dataset called tulip here.
results <- chisq.test(tulip, p = c(1/3, 1/3, 1/3)) # The hypothesis is that the distribution is 1/3, see if it's fitting in the sample dataset.
results

#change expected proportions
results <- chisq.test(tulip, p = c(1/2, 1/3, 1/6))
results

# independent 2-group Mann-Whitney U Test
# alternative to independent samples t-test. See the difference between two conditions/populations.
# Not normal distribution, so use nonparametric test to see the t-test.
wilcox.test(Income ~ Town,data=TwoTowns)
# where y is numeric and A is A binary factor

# matched 2-group Mann-Whitney U Test
wilcox.test(immer$Y1, immer$Y2, paired=TRUE) 

# Kruskal Wallis Test One Way Anova by Ranks
# See the difference between 3 or more conditions. As an alternative to one-way independent ANOVA.
kruskal.test(eclsk$read~eclsk$parent.ed.cat) # where y1 is numeric and A is a factor
kruskal.test(income.cat ~ female,data=eclsk) # where y is category and A is A binary factor

# binomial test
# See if the proporation in the two groups of the sample data is fitting in the p we set. 
# For binominial data like: pass/fail, complete/incomplete.
binom.test(c(682, 243), p = 3/4)
682/(682+243)
