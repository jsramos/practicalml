##################################
## Q1
# Load the Alzheimer's disease data using the commands:
library(AppliedPredictiveModeling)
library(caret)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
# Which of the following commands will create training and test sets with about 50% of the observations assigned to each?

# Answer
dData = data.frame(diagnosis,predictors)
trainIndex = createDataPartition(diagnosis, p = 0.50,list=FALSE)
training = adData[trainIndex,]
testing = adData[-trainIndex,]
# OR
testIndex = createDataPartition(diagnosis, p=0.50, list=FALSE)
training = adData[-testIndex,]
testing = adData[testIndex,]

##################################
## Q2
# Load the cement data using the commands:
library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]
# Make a histogram and confirm the SuperPlasticizer variable is skewed. Normally you might use the log transform to try to make the data more symmetric. Why would that be a poor choice for this variable?

# Answer
par(mfrow=c(1,2))
hist(log(training$Superplasticizer))
hist(training$Superplasticizer)
# There are a large number of values that are the same and even if 
# you took the log(SuperPlasticizer + 1) they would still all be 
# identical so the distribution would not be symmetric.
# OR
# There are values of zero so when you take the log() transform 
# those values will be -Inf.

##################################
## Q3
# Load the Alzheimer's disease data using the commands:
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
# Find all the predictor variables in the training set that begin with IL. Perform principal components on these variables with the preProcess() function from the caret package. Calculate the number of principal components needed to capture 80% of the variance. How many are there?

# Answer
pcaCols <- select(training, starts_with('IL'))
preprocessPca <- preProcess(pcaCols, method='pca', thresh = 0.8)
preprocessPca$numComp
# 7

##################################
## Q3
# Load the Alzheimer's disease data using the commands:
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
# Create a training data set consisting of only the predictors with variable names beginning with IL and the diagnosis. Build two predictive models, one using the predictors as they are and one using PCA with principal components explaining 80% of the variance in the predictors. Use method="glm" in the train function. What is the accuracy of each method in the test set? Which is more accurate?

# Answer
# Training set
trainingRedux <- data.frame(select(training, starts_with('IL')), select(training, starts_with('diagnosis')))
# Test set
testRedux <- data.frame(select(testing, starts_with('IL')), select(testing, starts_with('diagnosis')))
# Normal fit function
normalFit <- train(diagnosis ~ ., method='glm',data=trainingRedux)
# PCA trainControl function
pcaThresh <- trainControl(preProcOptions = list(thresh=0.8))
# PCA fit function
pcaFit <- train(diagnosis ~ ., method='glm',preProcess = 'pca', trControl=pcaThresh, data=trainingRedux)
# Confusion matrix for normal fit
confusionMatrix(predict(normalFit, newdata=testRedux), testRedux$diagnosis) # Accuracy 65%
# Confusion matrix for PCA fit
confusionMatrix(predict(pcaFit, newdata=testRedux), testRedux$diagnosis) # Accuracy 72%
