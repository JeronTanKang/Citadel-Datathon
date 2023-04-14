#DSE1101 Week 12 lab

##########################################################################
###Here we cover some basics on implementing principal component analysis
##########################################################################

#standard setup: clear, load libraries and data:

rm(list=ls())

#Use the US arrests data directly from R:
arrdata=USArrests

# PCA on standardized data:

#In case where we are not running a principal components regression, we can get just
#the PCA results on X using the prcomp() function
#Syntax: prcopm(data, scale=TRUE)
#If scale is true, data is standardized to be mean 0, variance 1

prall = prcomp(arrdata, scale = TRUE)

#Now produce the biplot using the biplot() function on the prcomp object:
biplot(prall)

#To get to the scree plot, first summarize the prcomp object:
prall.s = summary(prall)


#We will find the proportion of variance explained in prall.s$importance

prall.s$importance

########################################################
#Making a scree plot to decide the number of components:
########################################################

scree = prall.s$importance[2,] #save the proportion of variance explained

#Finally, do the scree plot:

plot(scree, main = "Scree Plot", xlab = "Principal Component", 
     ylab = "Proportion of Variance Explained", ylim = c(0,1), type = 'b', cex = .8)


####################################################################
##Bond data and application of Principal Component Regression (PCR)
###################################################################

rm(list=ls()) #clean up

#install.packages("pls") #package to fit PCR

library(pls) #load the library to run PCR

set.seed(3457534) #set seed, since PCR cross-validation involves randomness

#Load the data on bonds
data = read.csv('Eurostat_AAA_06_13_fd.csv')

sum(is.na(data)) #locate NA values

data=na.omit(data) #remove NA values


#Can recycle the same code to examine principal components and loadings:

prall = prcomp(data, scale = TRUE)

#Now produce the biplot using the biplot() function on the prcomp object:
biplot(prall)

#To get to the scree plot, first summarize the prcomp object:
prall.s = summary(prall)

#We will find the proportion of variance explained in prall.s$importance

prall.s$importance

scree = prall.s$importance[2,] #save the proportion of variance explained

#Finally, do the scree plot:

plot(scree, main = "Bond Yields Scree Plot", xlab = "Principal Component", 
     ylab = "Proportion of Variance Explained", ylim = c(0,1), type = 'b', cex = .8)

#########################
###Fitting PC regression
#########################

#We will use the pcr() function from package pls

#Syntax for model and data definition is similar to lm() command

#scale=TRUE standardizes the predictors; scale=FALSE uses raw predictors

#validation="CV" performs automatic 10-fold CV; validation="LOO" will perform LOOCV

#Get summary of CV results and variance shares explained by components

pcr.fit=pcr(M1~.,data=data, scale=TRUE, validation="CV")





####################
#Prediction with PCR
####################

#Create new variable: the 1-year rate is one day ahead from the rest - we will predict
#its future value.
M1f=data$M1[2:1949]

#Now bind together the Y variable and the predictors lagged 1 day:
tsdata=cbind(M1f,data[1:1948,])

#Perform PCR
pcr.fit=pcr(M1f~.,data=tsdata, scale=TRUE, validation="CV")

plot(pcr.fit, "loadings", comps = 1:5, legendpos = "topright")
abline(h = 0) #add the zero line for reference

#If you wanted to use LOOCV, this is how it would look like:
pcr.fit=pcr(M1f~.,data=tsdata, scale=TRUE, validation="LOO")

#Plot CV MSE
validationplot(pcr.fit, val.type="MSEP", main="LOOCV",legendpos = "topright")


#Predicted values, as usual are formed with the predict() function. 
#Note you specify the number of components in the model using the 'ncomp' option.
pcr.pred=predict(pcr.fit, newdata=tsdata, ncomp=4)
mean((tsdata$M1f-pcr.pred)^2) #MSE for PCR

#Consider a simple model - just historical mean:
histav=lm(M1f~1, data=tsdata)
predlm=predict.lm(histav, newdata=tsdata)

mean((tsdata$M1f-predlm)^2)# MSE of historical mean

plot(data$M1, main = "Plot of M1", xlab="time", ylab="1-year bond yield change", type="l")
