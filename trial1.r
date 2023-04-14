#standard setup: clear, load libraries and data:

rm(list=ls())

#Load the data on crimes
library(readr)
crimes <- read_csv("Traffic, Investigations _ Other/crimes.csv")
View(crimes)

#package to fit PCR
install.packages("pls") 

#load the library to run PCR
library(pls) 
#set seed, since PCR cross-validation involves randomness
set.seed(3457534) 

sum(is.na(crimes)) #locate NA values

crimes=na.omit(crimes) #remove NA values

# Check data types of variables
var_types = sapply(crimes, class)
# Identify numeric variables
num_vars = names(crimes)[sapply(crimes, is.numeric)]
# Remove numeric variables
crimes = crimes[, (names(crimes) %in% num_vars)]

prall = prcomp(crimes, scale = TRUE)

#Now produce the biplot using the biplot() function on the prcomp object:
biplot(prall)
window()
#To get to the scree plot, first summarize the prcomp object:
prall.s = summary(prall)
