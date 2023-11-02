# Set up the working directory first

# read the required datasets.
eclsk <- read.csv("eclsk.csv")
install.packages("haven")
library(haven)
hayden2005 <- read_sav("Hayden_2005.sav")
bp2000 <- read_sav("baguley_payne_2000.sav")


## Descriptive statistics ##
summary(bp2000)
describe(bp2000)


## Graphical assessment ## 
hist(hayden2005$days)
hist(hayden2005$days, breaks = 16)
boxplot(bp2000$percent_accuracy)
lin.model <- lm (gen~math,eclsk)
lin.model.residuals<-lin.model$residuals
qqnorm(lin.model.residuals)

plot(eclsk$gen,eclsk$math)
with(eclsk, plot(math,gen)) #(y=x)
abline(lm (gen~math,eclsk))
