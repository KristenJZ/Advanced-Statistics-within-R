## Set up the working directory and input the data
eclsk <- read.csv("eclsk.csv")
View(eclsk)
hist(eclsk$read)

## the relationship between eclsk$gen and eclsk$age
plot(eclsk$gen,eclsk$age)
cor(eclsk$gen,eclsk$age)

## The difference between male and female on reading scores
t.test(read~female, data=eclsk)

## determine if reading scores are significantly different across age categories
# Create a group variable: low, med, or high age:  Age ranges from 57-79:
eclsk$age.cat <- "b - average"
eclsk$age.cat[which(eclsk$age >= 72)] <- "c - older"   #72 months or older
eclsk$age.cat[which(eclsk$age <= 66)] <- "a - younger" #66 months or younger
eclsk$age.cat <- as.factor(eclsk$age.cat)

#look at the boxplot
boxplot(read~age.cat, data=eclsk)

# ANOVA analysis
anova(lm(read~age.cat, data=eclsk))


