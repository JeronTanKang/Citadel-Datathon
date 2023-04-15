#standard setup: clear, load libraries and data:
#testpush

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

###Kernel Density Estimation
rm(list = ls()) 
install.packages("ks") 
library(ks) 
library(readr)

data1 <- read_csv("C:/Users/jeron/OneDrive/Desktop/Hackathons/crash_info_general.csv")
#we just want to test with the first 1000 rows
set.seed(1)
sampled_rows <- sample(1:1000, round(0.1 * 1000), replace = FALSE)
data <- data1[sampled_rows, ]
class(data)

X11()
plot(kde(data$ILLUMINATION, h = 0.1), main = "ILLUMINATION Kernel Density Estimate - h=0.1", xlab = "ILLUMINATION")
plot(kde(data$ILLUMINATION, h = 0.3), main = "ILLUMINATION Kernel Density Estimate - h=0.3", xlab = "ILLUMINATION")
plot(kde(data$ILLUMINATION, h = 0.5), main = "ILLUMINATION Kernel Density Estimate - h=0.5", xlab = "ILLUMINATION")
plot(kde(data$ILLUMINATION, h = 0.7), main = "ILLUMINATION Kernel Density Estimate - h=0.7", xlab = "ILLUMINATION")
