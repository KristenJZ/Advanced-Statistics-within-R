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


# SEM Example
d<-read.table("sem exercise data.dat",h=F)
names(d)<-c("y1","y2","y3","y4","y5","y6","y7","y8","y9","y10","y11","y12")

factor.model <- '
	# define latent variables
	motivation =~ y1+y2+y3
	priorachievement =~ y4+y5+y6
	engagement =~ y7+y8+y9
	achievement =~ y10+y11+y12
	'
factor.model.fit <- sem(factor.model, d)
summary(factor.model.fit )

se.model <- '
	# define latent variables
	motivation =~ y1+y2+y3
	priorachievement =~ y4+y5+y6
	engagement =~ y7+y8+y9
	achievement =~ y10+y11+y12

	engagement ~motivation+priorachievement
	achievement ~engagement
	'
se.model.fit <- sem(se.model, d)
summary(se.model.fit )
semPaths(se.model.fit, rotation=2,sizeMan=6,sizeLat=6,edge.label.cex=1)

# measures of model fit 
fitMeasures(se.model.fit)[c("chisq","df","pvalue","cfi","rmsea")]