# Initial data and libraries
library(ggplot2)
library(caret)
library(ISLR)
data(Wage)

Wage <- subset(Wage, select=-c(logwage))

# Partition
inTrain <- createDataPartition(y=Wage$wage, p=0.7, list=F)
training <- Wage[inTrain,] # Don't forget the commas!
test <- Wage[-inTrain,] # Don't forget the commas!

trCtrl <- trainControl(method='repeatedcv', number=10, repeats=5)
fit <- train(wage ~ ., training, method='gbm', verbose=F, trainControl=trCtrl)
fit
pred <- predict(fit, newdata=test)
# confusionMatrix(pred, test$wage) # confusion matrices don't work for numeric predictors!!!

RMSE(pred, test$wage) # Performance measure for continuous variables
R2(pred, test$wage, formula='corr') # 2nd best preformance measure for continuous variables

plot(test$wage, pred) # the plot shows an almost 45-degree relationship, though with high variance. It also shows the wealth gap at 200K and failed predictions to the far right.