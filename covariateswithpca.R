library(caret)
library(kernlab)
library(e1071)
library(dplyr)
library(RANN)
data(spam)

# Partition data
inTrain <- createDataPartition(y=spam$type, p=0.75, list = F)
training <- spam[inTrain,]
test <- spam[-inTrain,]

