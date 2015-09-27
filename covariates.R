library(ISLR)
library(ggplot2)
library(caret)
library(kernlab)
library(e1071)
library(dplyr)
library(RANN)

data(Wage)
summary(Wage)

# We partition the data BEFORE DOING ANYTHING ELSE
inTrain <- createDataPartition(y=Wage$wage, p=.7, list=F)
training <- Wage[inTrain,]
test <- Wage[-inTrain,]

# Covariate example 1: creating dummy vars from categorical ones
table(training$jobclass)
dummies <- dummyVars(wage ~ jobclass, data=training) # dummyVars creates a dummy var model
predict(dummies, newdata=training) # assigns values to the dummy vars. we can append the columns to original data set then.

# splines as covariates (stand-ins for more complex variables)
library(splines)
basis <- bs(training$age, df=4)
lmbasis <- lm(wage ~ basis, training)
plot(training$age, training$wage, pch=19)
points(training$age, lmbasis$fitted.values, col='red', pch=19)
