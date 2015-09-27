library(ISLR)
library(ggplot2)
library(caret)
library(Hmisc)
data(Wage)
summary(Wage)

# We partition the data BEFORE DOING ANYTHING ELSE
inTrain <- createDataPartition(y=Wage$wage, p=.7, list=F)
training <- Wage[inTrain,]
test <- Wage[-inTrain,]
dim(training)
dim(test)

# Features plot
featurePlot(x=training[,c('age','education','jobclass')],
             y=training$wage,
             plot='pairs')

# Create a categorical out of a numeric
cutWage <- cut2(training$wage, g = 3) # divides the wage numeric variable into 3 quantile groups.
table(cutWage) # shows a table with the quantile groups and their occurrences.

# Contingency tables with other variables
t1 <- table(cutWage, training$jobclass)
t1
# Or the same contingency table but with proportions ACROSS wage groups -rows- (argument = 1, or 2 for column-wise proportions)
prop.table(t1,1)
