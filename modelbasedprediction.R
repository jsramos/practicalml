library(caret)
library(ggplot2)
data("iris")

# Partition
inTrain <- createDataPartition(y=iris$Species, p=0.7, list=F)
training <- iris[inTrain,] # Don't forget the commas!
test <- iris[-inTrain,] # Don't forget the commas!

# LDA
fitlda <- train(Species ~ ., training, method='lda')
predlda <- predict(fitlda, test)

# Naive Bayes
fitnb <- train(Species ~ ., training, method='nb')
prednb <- predict(fitnb, test)

# Confusion matrix for both fits
table(predlda, prednb) # the classified species by naive bayes vs those by linear discriminant analysis.

# Plotting misclassifications
correctPred <- (predlda==prednb)
qplot(Petal.Width, Sepal.Width, color=correctPred, data=test)