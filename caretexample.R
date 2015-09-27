library(caret)
library(kernlab)
library(e1071)
data(spam)

# Partition data
inTrain <- createDataPartition(y=spam$type, p=0.75, list = F)
training <- spam[inTrain,]
test <- spam[-inTrain,]

# Train model
set.seed(32343)
modelFit <- caret::train(type ~ ., data = training, method='glm')

# Use model to predict
predictions <- predict(modelFit, newdata=test)

# Show confusion matrix
confusionMatrix(predictions, test$type)

# K-fold cross validation
set.seed(32323)
folds <- createFolds(y=spam$type, k=10, list=T, returnTrain = T) # list=T because otherwise we won't have a key-list pair with the folds
head(folds)
sapply(folds, length) # look at length of folds
head(folds[[1]]) # Look at the head of the indices contained in the 1st fold

# Resample cross validation = WITH REPLACEMENT
resamples <- createResample(y=spam$type, times=9, list=T)
sapply(resamples, length)

# Time slices
tme <- 1:1000
timeSlices <- createTimeSlices(y=tme, initialWindow = 20, horizon = 10 )
# Will return an object containing 2 lists (train and test) per each index:
# - timeSlice$train[[1]] will return 1 to 20, timeSlice$test[[1]] will return 21 to 30.
# - timeSlice$train[[2]] will return 2 to 21, timeSlice$test[[2]] will return 22 to 31.
# - timeSlice$train[[3]] will return 3 to 22, timeSlice$test[[3]] will return 23 to 32.
# - and so on, so that each index contains a train and a test set.
str(timeSlices)

