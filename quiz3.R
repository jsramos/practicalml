# Prep
options(scipen=999)

######################### Q1
library(AppliedPredictiveModeling)
library(caret)
data(segmentationOriginal)
# Partition
set.seed(125)
training <- subset(segmentationOriginal, Case == "Train")
test <- subset(segmentationOriginal, Case == "Test")
# Fit
fit <- train(Class ~ ., training, method='rpart')
fit$finalModel
# Plot tree

plot(fit$finalModel, uniform=TRUE,margin=0.2)
text(fit$finalModel, use.n=TRUE, all=TRUE, cex=.8)
# TotalIntench2 = 23,000; FiberWidthCh1 = 10; PerimStatusCh1=2: PS
# TotalIntench2 = 50,000; FiberWidthCh1 = 10;VarIntenCh4 = 100: WS
# TotalIntench2 = 57,000; FiberWidthCh1 = 8;VarIntenCh4 = 100: PS
# FiberWidthCh1 = 8;VarIntenCh4 = 100; PerimStatusCh1=2: Not possible to predict

######################### Q2
# If K is small in a K-fold cross validation is the bias in the estimate of out-of-sample
# (test set) accuracy smaller or bigger? If K is small is the variance in the estimate of
# out-of-sample (test set) accuracy smaller or bigger. Is K large or small in leave one
# out cross validation?
#        A: The bias is larger and the variance is smaller. Under leave one out cross 
#           validation K is equal to the sample size.

######################### Q3
library(pgmm)
data(olive)
olive <- olive[,-1]
fit <- train(Area ~ ., olive, method='rpart2')
predict(fit, newdata = as.data.frame(t(colMeans(olive))))
# 2.783. It is strange because Area should be a qualitative variable - but tree is 
# reporting the average value of Area as a numeric variable in the leaf predicted 
# for newdata

######################### Q4
library(ElemStatLearn)
library(caret)
data(SAheart)
set.seed(8484)
# data partition
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]
set.seed(13234)
# fit
# fit <- train(chd ~ age + alcohol + obesity + tobacco + typea + ldl, 
#             trainSA, method='glm', family='binomial') # function will suggest converting response to a factor
fit <- train(chd ~ age + alcohol + obesity + tobacco + typea + ldl,
           method='glm', data=trainSA, family='binomial')
# misclassification function
missClass <- function(values,prediction) {
        sum(((prediction > 0.5)*1) != values)/length(values)
}
missClass(trainSA$chd, predict(fit, trainSA)) # 0.2727273
missClass(testSA$chd, predict(fit, testSA)) # 0.3116883

######################### Q5
library(ElemStatLearn)
data(vowel.train)
data(vowel.test) 

# Transforms
vowel.test$y <- as.factor(vowel.test$y)
vowel.train$y <- as.factor(vowel.train$y)

# Seed
set.seed(33833)

# Fit
fit <- train(y ~ ., vowel.train, method='rf', importance=T, trControl = trainControl(method = "oob")) # [importance=T] argument is MANDATORY if you want to further calculate variable importance
importance <- varImp(fit$finalModel)
rownames(importance)[order(importance, decreasing = T)]
# The order of the variables is:
# x.2, x.1, x.5, x.6, x.8, x.4, x.9, x.3, x.7,x.10