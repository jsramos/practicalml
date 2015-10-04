################# Q1
library(ElemStatLearn)
library(caret)
data(vowel.train)
data(vowel.test) 
vowel.train$y <- as.factor(vowel.train$y)
vowel.test$y <- as.factor(vowel.test$y)
set.seed(33833)
fitrf <- train(y ~ ., data=vowel.train, method='rf')
fitgbm <- train(y ~ ., data=vowel.train, method='gbm')
predrf <- predict(fitrf, vowel.test)
predgbm <- predict(fitgbm, vowel.test)
confRf <- confusionMatrix(predrf, vowel.test$y)
confGbm <- confusionMatrix(predgbm, vowel.test$y)
confRf$overall[1] # 6082251 
confGbm$overall[1] # 0.5108225
# build dataframe with both predictions and correct outcome
agree <- data.frame(predrf, predgbm, y=vowel.test$y, agree=(predrf==predgbm))
# count all cases where the prediction from 1 model matched the observations
# and divide it by the total correct predictions regardless of model
accGbm <- sum(predgbm[agree$agree] == agree$y[agree$agree]) / sum(agree$agree)
accRf <- sum(predrf[agree$agree] == agree$y[agree$agree]) / sum(agree$agree)
accGbm == accRf # 0.6352201

################# Q2
library(caret)
library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
set.seed(62433)
# Random Forest
fitrf <- train(diagnosis ~ ., data=training, method='rf')
predrf <- predict(fitrf, testing)
confrf <- confusionMatrix(predrf, testing$diagnosis)

# Gradient boost machine
fitgbm <- train(diagnosis ~ ., data=training, method='gbm')
predgbm <- predict(fitgbm, testing)
confgbm <- confusionMatrix(predgbm, testing$diagnosis)

# Linear Discriminant Analysis
fitlda <- train(diagnosis ~ ., data=training, method='lda')
predlda <- predict(fitlda, testing)
conflda <- confusionMatrix(predlda, testing$diagnosis)

# Stacking
stackeddf <- data.frame(predrf, predgbm, predlda, diagnosis=testing$diagnosis)
stackedFit <- train(diagnosis ~ ., data=stackeddf, method='rf')
stackedPred <- predict(stackedFit, stackeddf)
confstacked <- confusionMatrix(stackedPred, stackeddf$diagnosis)

# Accuracies
confrf$overall[1]
confgbm$overall[1]
conflda$overall[1]
confstacked$overall[1]

################# Q3
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]
set.seed(233)
lassoFit <- train(CompressiveStrength ~ ., data=training, method='lasso')
plot.enet(lassoFit$finalModel, xvar="penalty", use.color=T) # Cement.
# The method grows lambda (the penalty to large coeffs), and this reduces
# them to 0 as lambda approaches infinity.
# When lambda = 0, then all coeffs are not regularized, so it's a standard linear regression.
# The idea of lasso is basically to draw a limit to a coeff so that it doesn't grow out of proportion.
# This is because normally, coeffs from LM are the best UNBIASED estimators.
# UNBIASED means that no matter how many samples we take from the estimator, its mean will
# always be the same.
# Lasso and Ridge relax the bias and we're free to choose larger estimators if they reduce the variance
# even further, but since we need to control the bias nonetheless, we need a mechanism to constrain
# the growth of the coeffs.
# The lasso puts this constraint based on a factor lambda.
# Lambda penalizes large coeffs, so it reduces them until they are irrelevant.

################# Q4
dat <- read.csv('https://d396qusza40orc.cloudfront.net/predmachlearn/gaData.csv', 
                strip.white = T, 
                stringsAsFactors = F,
                sep = ',')
library(lubridate)  # For year() function below
library(forecast)
training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr)
fit <- bats(tstrain)
fit
pred <- forecast(fit, level=95, h=dim(testing)[1])
names(data.frame(pred))
predComb <- cbind(testing, data.frame(pred))
names(testing)
names(predComb)
predComb$in95 <- (predComb$Lo.95 < predComb$visitsTumblr) & 
    (predComb$visitsTumblr < predComb$Hi.95)
# How many of the testing points is the true value within the 
# 95% prediction interval bounds?
prop.table(table(predComb$in95))[2] # 0.9617021

################# Q4
set.seed(3523)
library(AppliedPredictiveModeling)
library(e1071)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]
set.seed(325)
svmfit <- svm(CompressiveStrength ~ ., data=training)
pred <- predict(svmfit, testing)
acc <- accuracy(pred, testing$CompressiveStrength)