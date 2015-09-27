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

hist(training$capitalAve) # See how the capitalAve is frequently very low, but there are some larger avgs that are present as well.

# Numeric evidence for the need for preprocessing
mean(training$capitalAve)
sd(training$capitalAve)

# Standardized Preprocessing for training set
preObj <- preProcess(training[-58], method = c('center','scale'))
prepCapitalAve <- predict(preObj, training[-58])$capitalAve
hist(prepCapitalAve) # Not our best work. Centered at 0, but still skewed
mean(prepCapitalAve)

# Standardized Preprocessing for test set
testPrepCapitalAve <- predict(preObj, test[-58])$capitalAve
mean(testPrepCapitalAve)
hist(testPrepCapitalAve)

# BoxCox Transforms
preObj <- preProcess(training[-58], method = c('BoxCox'))
boxcoxCapitalAve <- predict(preObj, training[-58])$capitalAve
hist(boxcoxCapitalAve) # Better, but the outlier at 0 still skews the distro.

# K-Nearest Neighbor
# Since this is for imputing missing values, we first need to take some of them to NA
selectedNA <- rbinom(nrow(training), size=1,prob=.5) == 1 # Will create a vector of random 0s and 1s, and will turn the 1s into TRUE
training <- training %>% mutate(imputedCapAve=ifelse(selectedNA == 1, NA, capitalAve))

# Impute and Standardize
preObj <- preProcess(training[-58], method='knnImpute')
capAveNew <- predict(preObj, training[-58])$imputedCapAve

training$capAveNew <- capAveNew # Assign new imputed vector as new column to original dataset

