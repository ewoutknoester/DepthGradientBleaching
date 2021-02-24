# ------------------------------------------
# Analysis depth gradient (2019 - 2020): Bleaching and growth
# Kristina Hrelja & Ewout Knoester
# Created 05-Feb-2021
# ------------------------------------------

# Set R and packages
rm(list=ls()) #Clear workspace
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #Set directory 

##### Production #####

data1 <- read.csv(file.choose(), header=TRUE) # import data and save as data1
data1 # see dataset

#check type of variable
class(data1$production)
class(data1$depth)
class(data1$depth2)
class(data1$condition)
#to convert a variable to a factor 
data1$condition <- as.factor(data1$condition)

#HISTOGRAM - look at distribution of Y (production), if not normally distributed look at transformations of Y 
hist(data1$productiont)
hist(log(data1$production +1)) # adding 1 to avoid log of 0
hist(sqrt(data1$production))
#SCATTERPLOT - look at relationship between x and y
plot(data1$depth, data1$production, main="Scatterplot", ylab = "production", xlab = "depth") #plot all points
plot(data1$depth[data1$condition=="1"], data1$production[data1$condition=="1", xlab="depth", ylab="production"]) #plot only bleach 
points(data1$depth[data1$condition=="0"], data1$production[data1$condition=="0"], col=2) # adding points for non-bleach 

# transformations (if necessary)
lnproduction <- log(data1$production +1) 
data1 <- cbind(data1, lnproduction)   #add the variable lnproduction to the dataset 

sqrtproduction <- sqrt(data1$production)
data1 <- cbind(data1, sqrtproduction)

##Regression models##

install.packages(lme4)
library(lme4)

model1 <- lmer(production ~ condition + depth + depth2 + condition:depth + condition:depth2, data=data1)
summary(model1)
plot(model1) #checking assumptions

model2 <- lmer(production ~ condition + depth + depth2 + condition:depth + condition:depth2 + (1|nursery), data=data1)
summary(model2)
plot(model2)