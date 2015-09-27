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
dim(training)
dim(test)

# Explore training set
qplot(Petal.Width, Sepal.Width, color=Species, data=training)

# Train model
treeFit <- train(Species ~ ., method='tree', data=training)
print(treeFit$finalModel)

# Examine tree in a plot
library(rattle) # you may require to install gtk using brew
fancyRpartPlot(treeFit$finalModel)

# Confusion Matrix
confusionMatrix(predict(treeFit, newdata = test), test$Species)
