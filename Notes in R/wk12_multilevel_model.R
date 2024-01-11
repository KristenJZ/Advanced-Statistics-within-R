##################################################################################################################
####################################### RSCH 8120:Regression ############################################
##################################################################################################################
####----------------------------------####

#### The original codes are from Dr. Kyle Cox

#clean environment
rm(list=ls(all=TRUE))

####--------------------------------------------------------------------####
### Part I: Set-up
library(lattice)
library(lme4)
library(lavaan)
library(semPlot)

# Import data
dat<-read.table(file="ml_data.rdata")

####--------------------------------------------------------------------####
# Check new data
#plot 
xyplot(y~x,group=L2id,data=dat)

####--------------------------------------------------------------------####
### Build up to multilevel analysis


## estimate the relationship between x and y
##run a single level regression model within school 1 (and then 2)
summary(lm(y~x,dat[which(dat$L2id==1),]))
summary(lm(y~x,dat[which(dat$L2id==2),]))

##estimate single level regression using data from all groups
summary(lm(y~x,dat))

##aggregate data to group level and then run a regression at the group level
datagg<-aggregate(dat,by=list(dat$L2id),FUN=mean)
summary(lm(y~x,datagg))

##estimate multilevel model
summary(lmer(y~x+(1|L2id),dat))

##note differences between single level and multilevel models (e.g., point estimates of X, SEs, and t-values)
