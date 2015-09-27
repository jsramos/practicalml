# Initial data and libraries
library(ggplot2)
library(caret)
data("iris")

# Look at out predictors
names(iris)

# Look at the balance of our Response
table(iris$Species)

# Partition
inTrain <- createDataPartition(y=iris$Species, p=0.7, list=F)
training <- iris[inTrain,] # Don't forget the commas!
test <- iris[-inTrain,] # Don't forget the commas!

 # fit a model
fit <- train(Species ~ ., 
             training, 
             method='rf', 
             prox=T, # prox=T will generate a proximity matrix
             trControl = trainControl(method = "cv", # Use cross-validation
                                      number = 5)) # the number of FOLDS!
# fit a model
fitNoCv <- train(Species ~ ., 
             training, 
             method='rf', 
             prox=T) # prox=T will generate a proximity matrix

# Confusion matrix w/o CV
confusionMatrix(predict(fitNoCv, test), test$Species) # w/o CV the performance is better, but since it has no CV, this better performance is mainly due to chance.
# Confusion matrix with CV
confusionMatrix(predict(fit, test), test$Species) # with CV, the performance is lower, but we have eliminated the inherent bias due to model selection.
